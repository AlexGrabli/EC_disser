# ======================================================================
# Сравнение данных Eddy Covariance: Курск vs Москва (2013)
# Анализ по фенофазам яровых зерновых
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(stringr)
  library(bigleaf)
})

# =====================================================================
# 1. КОНСТАНТЫ И УТИЛИТЫ
# =====================================================================

PHASE6_EN <- c("Emergence","Tillering","StemElong","Heading","Flowering","Ripening")
PHASE6_RU <- c("Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание")

# Палитра для двух станций
pal_site <- c("Курск" = "#d95f02", "Москва" = "#1b9e77")

# Границы фенофаз — Москва (даты)
B_MOSCOW <- list(
  Sowing="2013-05-14", Emergence="2013-05-17", Tillering="2013-06-03",
  StemElong="2013-06-27", Heading="2013-07-17", Flowering="2013-07-28",
  Ripening="2013-08-03", Harvesting="2013-08-14"
)

# Границы фенофаз — Курск (DOY → даты 2013)
doy_to_date <- function(doy, year = 2013) {
  as.Date(doy - 1, origin = paste0(year, "-01-01"))
}
B_KURSK <- list(
  Sowing    = as.character(doy_to_date(115)),
  Emergence = as.character(doy_to_date(118)),
  Tillering = as.character(doy_to_date(136)),
  StemElong = as.character(doy_to_date(157)),
  Heading   = as.character(doy_to_date(165)),
  Flowering = as.character(doy_to_date(180)),
  Ripening  = as.character(doy_to_date(196)),
  Harvesting= as.character(doy_to_date(226))
)

cat("Фенофазы Курска:\n")
print(data.frame(Phase = names(B_KURSK), Date = unlist(B_KURSK), row.names = NULL))
cat("\nФенофазы Москвы:\n")
print(data.frame(Phase = names(B_MOSCOW), Date = unlist(B_MOSCOW), row.names = NULL))

# --- Утилиты (из all_seasons_2026_Claudeversion.R) ---
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
safe_num <- to_num

fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 2), "н/д")

phase_by_bounds <- function(date_vec, bounds) {
  e  <- as.Date(bounds$Emergence);  t  <- as.Date(bounds$Tillering)
  s  <- as.Date(bounds$StemElong);  h  <- as.Date(bounds$Heading)
  f  <- as.Date(bounds$Flowering);  r  <- as.Date(bounds$Ripening)
  hv <- as.Date(bounds$Harvesting)
  res <- rep(NA_character_, length(date_vec))
  res[date_vec >= e  & date_vec <  t ] <- "Emergence"
  res[date_vec >= t  & date_vec <  s ] <- "Tillering"
  res[date_vec >= s  & date_vec <  h ] <- "StemElong"
  res[date_vec >= h  & date_vec <  f ] <- "Heading"
  res[date_vec >= f  & date_vec <  r ] <- "Flowering"
  res[date_vec >= r  & date_vec <= hv] <- "Ripening"
  factor(res, levels = PHASE6_EN)
}

fix_tillering_if_missing <- function(df, bounds) {
  if (!all(c("Date","Phase") %in% names(df))) return(df)
  if (sum(df$Phase == "Tillering", na.rm = TRUE) == 0) {
    t0 <- as.Date(bounds$Tillering); s0 <- as.Date(bounds$StemElong)
    sel <- which(!is.na(df$Date) & df$Date >= t0 & df$Date < s0)
    if (length(sel)) df$Phase[sel] <- "Tillering"
  }
  df
}

# Базовая тема для графиков
theme_base <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
        strip.background = element_rect(fill = "grey95", colour = "grey80"),
        plot.title = element_text(face = "bold", hjust = 0.02),
        legend.position = "bottom")

# =====================================================================
# 2. ЗАГРУЗКА И СТАНДАРТИЗАЦИЯ ДАННЫХ
# =====================================================================

cat("\n========================================\n")
cat("ЗАГРУЗКА ДАННЫХ\n")
cat("========================================\n\n")

# --- Курск ---
cat("Загрузка данных Курска...\n")
raw_kursk <- readr::read_csv("Kursk_Final_Recalculated.csv",
                             show_col_types = FALSE, guess_max = 1e6)
names(raw_kursk) <- trimws(names(raw_kursk))

# Парсинг времени Курска
kursk_dt <- as.POSIXct(raw_kursk$DateTime, tz = "UTC")
if (all(is.na(kursk_dt))) {
  kursk_dt <- suppressWarnings(ymd_hms(raw_kursk$DateTime, tz = "UTC", quiet = TRUE))
}
if (all(is.na(kursk_dt))) {
  kursk_dt <- suppressWarnings(ymd_hm(raw_kursk$DateTime, tz = "UTC", quiet = TRUE))
}

# Сдвиг времени Курска на +3 часа (UTC → MSK)
kursk_dt <- kursk_dt + hours(3)
cat("  Время Курска сдвинуто на +3 ч (UTC → MSK)\n")

df_kursk <- tibble(
  Site     = "Курск",
  datetime = kursk_dt,
  Date     = as.Date(kursk_dt),
  HourInt  = pmin(23L, pmax(0L, hour(kursk_dt))),
  GPP      = to_num(raw_kursk$GPP_DT_uStar),
  Reco     = to_num(raw_kursk$Reco_DT_uStar),
  NEE      = to_num(raw_kursk$NEE_uStar_f),
  LE       = to_num(raw_kursk$LE_f),
  H        = to_num(raw_kursk$H_f),
  Tair     = to_num(raw_kursk$Tair_f),
  VPD      = to_num(raw_kursk$VPD_f),
  Rg       = to_num(raw_kursk$Rg_f),
  PPFD     = to_num(raw_kursk$Rg_f) * 2.02   # конвертация Rg -> PPFD
) %>%
  filter(!is.na(datetime)) %>%
  mutate(
    # GPP не может быть < 0: меняем знак отрицательных значений
    GPP = ifelse(GPP < 0, -GPP, GPP),
    Phase = phase_by_bounds(Date, B_KURSK),
    Phase_lab = factor(as.character(Phase), levels = PHASE6_EN, labels = PHASE6_RU)
  )

n_gpp_flipped <- sum(to_num(raw_kursk$GPP_DT_uStar) < 0, na.rm = TRUE)
cat(sprintf("  Курск: %d строк, дат NA: %d, GPP NA: %d\n",
            nrow(df_kursk), sum(is.na(df_kursk$Date)), sum(is.na(df_kursk$GPP))))
cat(sprintf("  GPP: знак изменён у %d отрицательных значений\n", n_gpp_flipped))
cat(sprintf("  Диапазон дат: %s — %s\n",
            min(df_kursk$Date, na.rm = TRUE), max(df_kursk$Date, na.rm = TRUE)))

# --- Москва ---
cat("\nЗагрузка данных Москвы...\n")
raw_moscow <- readr::read_csv("Lasslop_2013_Complete_GapFilled.csv",
                              show_col_types = FALSE, guess_max = 1e6)
names(raw_moscow) <- trimws(names(raw_moscow))

# Парсинг ISO DateTime для Москвы
moscow_dt_raw <- gsub("Z$", "", raw_moscow$DateTime)
moscow_dt_raw <- gsub("T", " ", moscow_dt_raw, fixed = TRUE)
moscow_dt <- as.POSIXct(moscow_dt_raw, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
if (all(is.na(moscow_dt))) {
  moscow_dt <- suppressWarnings(ymd_hms(raw_moscow$DateTime, tz = "UTC", quiet = TRUE))
}

df_moscow <- tibble(
  Site     = "Москва",
  datetime = moscow_dt,
  Date     = as.Date(moscow_dt),
  HourInt  = pmin(23L, pmax(0L, hour(moscow_dt))),
  GPP      = to_num(raw_moscow$GPP),
  Reco     = to_num(raw_moscow$Reco),
  NEE      = to_num(raw_moscow$NEE_filled),
  LE       = to_num(raw_moscow$LE),
  H        = to_num(raw_moscow$H),
  Tair     = to_num(raw_moscow$Tair),
  VPD      = to_num(raw_moscow$VPD),
  Rg       = to_num(raw_moscow$Rg),
  PPFD     = to_num(raw_moscow$PPFD)
) %>%
  filter(!is.na(datetime)) %>%
  mutate(
    Phase = phase_by_bounds(Date, B_MOSCOW),
    Phase_lab = factor(as.character(Phase), levels = PHASE6_EN, labels = PHASE6_RU)
  )

cat(sprintf("  Москва: %d строк, дат NA: %d, GPP NA: %d\n",
            nrow(df_moscow), sum(is.na(df_moscow$Date)), sum(is.na(df_moscow$GPP))))
cat(sprintf("  Диапазон дат: %s — %s\n",
            min(df_moscow$Date, na.rm = TRUE), max(df_moscow$Date, na.rm = TRUE)))

# --- Объединение ---
df_all <- bind_rows(df_kursk, df_moscow) %>%
  filter(!is.na(datetime), !is.na(HourInt), !is.na(Phase_lab))

cat(sprintf("\nОбъединённый набор: %d строк\n", nrow(df_all)))
cat("Распределение по сайтам и фазам:\n")
print(df_all %>% count(Site, Phase_lab))

# Диагностика: продолжительность фенофаз
cat("\n--- Продолжительность фенофаз (дни) ---\n")
phase_dur <- tibble(
  Phase_EN = PHASE6_EN,
  Phase_RU = PHASE6_RU,
  Kursk_start = as.Date(c(B_KURSK$Emergence, B_KURSK$Tillering, B_KURSK$StemElong,
                           B_KURSK$Heading, B_KURSK$Flowering, B_KURSK$Ripening)),
  Kursk_end   = as.Date(c(B_KURSK$Tillering, B_KURSK$StemElong, B_KURSK$Heading,
                           B_KURSK$Flowering, B_KURSK$Ripening, B_KURSK$Harvesting)),
  Moscow_start = as.Date(c(B_MOSCOW$Emergence, B_MOSCOW$Tillering, B_MOSCOW$StemElong,
                            B_MOSCOW$Heading, B_MOSCOW$Flowering, B_MOSCOW$Ripening)),
  Moscow_end   = as.Date(c(B_MOSCOW$Tillering, B_MOSCOW$StemElong, B_MOSCOW$Heading,
                            B_MOSCOW$Flowering, B_MOSCOW$Ripening, B_MOSCOW$Harvesting))
) %>%
  mutate(
    Kursk_days  = as.integer(Kursk_end - Kursk_start),
    Moscow_days = as.integer(Moscow_end - Moscow_start)
  )
print(phase_dur %>% select(Phase_RU, Phase_EN, Kursk_days, Moscow_days))
readr::write_csv(phase_dur, "KM_phase_duration_comparison.csv")

# =====================================================================
# 3. СУТОЧНЫЙ ХОД NEE / GPP / Reco ПО ФЕНОФАЗАМ
# =====================================================================

cat("\n========================================\n")
cat("СУТОЧНЫЙ ХОД ПОТОКОВ ПО ФЕНОФАЗАМ\n")
cat("========================================\n\n")

# шаг 1: среднее по ДНЯМ
by_day <- df_all %>%
  group_by(Site, Phase_lab, Date, HourInt) %>%
  summarise(
    NEE  = mean(NEE,  na.rm = TRUE),
    GPP  = mean(GPP,  na.rm = TRUE),
    Reco = mean(Reco, na.rm = TRUE),
    .groups = "drop"
  )

# шаг 2: среднее + SE + CI
diu <- by_day %>%
  group_by(Site, Phase_lab, HourInt) %>%
  summarise(
    across(c(NEE, GPP, Reco),
           list(mean = ~mean(.x, na.rm = TRUE),
                se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(is.finite(.x)))),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) %>%
  mutate(
    across(ends_with("_se"), ~replace_na(.x, 0)),
    NEE_lwr  = NEE_mean  - 1.96 * NEE_se,  NEE_upr  = NEE_mean  + 1.96 * NEE_se,
    GPP_lwr  = GPP_mean  - 1.96 * GPP_se,  GPP_upr  = GPP_mean  + 1.96 * GPP_se,
    Reco_lwr = Reco_mean - 1.96 * Reco_se, Reco_upr = Reco_mean + 1.96 * Reco_se
  )

readr::write_csv(diu, "KM_diurnal_summary.csv")

plot_diurnal <- function(flux) {
  cols <- paste0(flux, c("_mean", "_lwr", "_upr"))
  ggplot(diu, aes(x = HourInt, y = .data[[cols[1]]],
                  color = Site, fill = Site)) +
    geom_ribbon(aes(ymin = .data[[cols[2]]], ymax = .data[[cols[3]]]),
                alpha = 0.14, colour = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.1) +
    facet_wrap(~Phase_lab, ncol = 3, drop = FALSE) +
    scale_color_manual(values = pal_site, name = "Станция") +
    scale_fill_manual(values = pal_site, guide = "none") +
    scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23), expand = c(0, 0)) +
    labs(title = paste0("Суточный ход ", flux, " — Курск vs Москва (2013)"),
         x = "Час суток", y = "мкмоль CO\u2082 м\u207b\u00b2 с\u207b\u00b9") +
    theme_base
}

p_NEE  <- plot_diurnal("NEE")
p_GPP  <- plot_diurnal("GPP")
p_Reco <- plot_diurnal("Reco")

ggsave("KM_diurnal_NEE.png",  p_NEE,  width = 12, height = 8, dpi = 800, bg = "white")
ggsave("KM_diurnal_GPP.png",  p_GPP,  width = 12, height = 8, dpi = 800, bg = "white")
ggsave("KM_diurnal_Reco.png", p_Reco, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_diurnal_NEE/GPP/Reco.png\n")

# =====================================================================
# 4. СВЕТОВЫЕ КРИВЫЕ (GPP vs PPFD) ПО ФЕНОФАЗАМ
# =====================================================================

cat("\n========================================\n")
cat("СВЕТОВЫЕ КРИВЫЕ ПО ФЕНОФАЗАМ\n")
cat("========================================\n\n")

# Подготовка данных для световых кривых
prep_light <- function(df, site_name) {
  df %>%
    filter(!is.na(Phase_lab),
           is.finite(PPFD), PPFD >= 10, PPFD <= 2200,
           is.finite(GPP),  GPP >= 0,   GPP <= 40) %>%
    transmute(
      Site      = site_name,
      Phase_lab = Phase_lab,
      PPFD      = PPFD,
      Rg        = bigleaf::PPFD.to.Rg(PPFD),
      GPP       = GPP
    )
}

light_kursk  <- prep_light(df_kursk,  "Курск")
light_moscow <- prep_light(df_moscow, "Москва")
light_all    <- bind_rows(light_kursk, light_moscow)

cat(sprintf("  Данные для световых кривых: Курск=%d, Москва=%d\n",
            nrow(light_kursk), nrow(light_moscow)))

# Фит α, β: GPP = (α * β * Rg) / (α * Rg + β)
fit_lrc_rg <- function(dat) {
  dat <- arrange(dat, Rg)
  n_points <- nrow(dat)
  if (n_points < 12 || diff(range(dat$Rg)) < 50 || var(dat$GPP) < 0.05) {
    return(tibble(alpha = NA_real_, beta = NA_real_, n_points = n_points,
                  r2 = NA_real_, rmse = NA_real_, status = "Insufficient data"))
  }
  bin_w <- 50
  db <- dat %>%
    mutate(Rg_bin = pmax(0, floor(Rg / bin_w) * bin_w)) %>%
    group_by(Rg_bin) %>%
    summarise(Rg = mean(Rg), GPP = mean(GPP), .groups = "drop") %>%
    arrange(Rg)

  low <- db %>% filter(Rg <= quantile(Rg, 0.2, na.rm = TRUE))
  a0 <- suppressWarnings(coef(lm(GPP ~ 0 + Rg, data = low)))[1]
  if (!is.finite(a0)) a0 <- 0.03
  a0 <- min(max(a0, 0.005), 0.12)
  b0 <- quantile(db$GPP, 0.95, na.rm = TRUE)
  if (!is.finite(b0) || b0 <= 0) b0 <- max(db$GPP, na.rm = TRUE)
  b0 <- min(max(b0, 5), 40)

  fit <- try(
    nls(GPP ~ (alpha * beta * Rg) / (alpha * Rg + beta),
        data = db,
        start = list(alpha = a0, beta = b0),
        algorithm = "port",
        lower = c(alpha = 1e-4, beta = 1),
        upper = c(alpha = 0.2,  beta = 60),
        control = nls.control(maxiter = 500, warnOnly = TRUE)),
    silent = TRUE
  )

  if (!inherits(fit, "try-error")) {
    co <- coef(fit)
    pred <- (co["alpha"] * co["beta"] * db$Rg) / (co["alpha"] * db$Rg + co["beta"])
    ss_res <- sum((db$GPP - pred)^2)
    ss_tot <- sum((db$GPP - mean(db$GPP))^2)
    r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)
    rmse <- sqrt(mean((db$GPP - pred)^2))
    return(tibble(alpha = unname(co["alpha"]), beta = unname(co["beta"]),
                  n_points = n_points, r2 = r2, rmse = rmse, status = "Success"))
  }

  # Грид-поиск
  grid_a <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.12, 0.2)
  grid_b <- c(5, 8, 10, 15, 20, 30, 40, 60)
  best <- list(a = NA_real_, b = NA_real_, rss = Inf)
  for (aa in grid_a) {
    for (bb in grid_b) {
      pred <- (aa * bb * db$Rg) / (aa * db$Rg + bb)
      rss <- sum((db$GPP - pred)^2)
      if (is.finite(rss) && rss < best$rss) best <- list(a = aa, b = bb, rss = rss)
    }
  }
  if (is.finite(best$a)) {
    pred <- (best$a * best$b * db$Rg) / (best$a * db$Rg + best$b)
    ss_res <- sum((db$GPP - pred)^2)
    ss_tot <- sum((db$GPP - mean(db$GPP))^2)
    r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)
    rmse <- sqrt(mean((db$GPP - pred)^2))
    return(tibble(alpha = best$a, beta = best$b, n_points = n_points,
                  r2 = r2, rmse = rmse, status = "Grid search"))
  }
  tibble(alpha = NA_real_, beta = NA_real_, n_points = n_points,
         r2 = NA_real_, rmse = NA_real_, status = "Fit error")
}

# Коэффициенты по (сайт × фаза)
coef_tbl <- light_all %>%
  group_by(Site, Phase_lab) %>%
  group_modify(~fit_lrc_rg(.x)) %>%
  ungroup() %>%
  mutate(GPPmax = beta)

cat("\nКоэффициенты световых кривых:\n")
coef_tbl %>%
  mutate(
    alpha_fmt = ifelse(is.finite(alpha), sprintf("%.4f", alpha), "N/A"),
    GPPmax_fmt = ifelse(is.finite(GPPmax), sprintf("%.2f", GPPmax), "N/A"),
    r2_fmt = ifelse(is.finite(r2), sprintf("%.3f", r2), "N/A")
  ) %>%
  select(Site, Phase_lab, alpha_fmt, GPPmax_fmt, n_points, r2_fmt, status) %>%
  print(n = Inf)

readr::write_csv(coef_tbl, "KM_light_response_coefficients.csv")

# Построение кривых
range_tbl <- light_all %>%
  group_by(Site, Phase_lab) %>%
  summarise(PPFD_max = min(2000, max(PPFD, na.rm = TRUE)), .groups = "drop") %>%
  mutate(PPFD_max = ifelse(!is.finite(PPFD_max) | PPFD_max <= 0, 500, PPFD_max))

curve_tbl <- coef_tbl %>%
  left_join(range_tbl, by = c("Site", "Phase_lab")) %>%
  filter(is.finite(alpha), is.finite(beta)) %>%
  rowwise() %>%
  mutate(
    data = list({
      xs_ppfd <- seq(0, PPFD_max, length.out = 200)
      xs_rg   <- bigleaf::PPFD.to.Rg(xs_ppfd)
      tibble(PPFD = xs_ppfd,
             GPP_hat = (alpha * beta * xs_rg) / (alpha * xs_rg + beta))
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(data) %>%
  select(Site, Phase_lab, PPFD, GPP_hat)

# Биннинг
bin_w <- 100
bins_tbl <- light_all %>%
  mutate(PPFD_bin = pmax(0, floor(PPFD / bin_w) * bin_w)) %>%
  group_by(Site, Phase_lab, PPFD_bin) %>%
  summarise(GPP_mean = mean(GPP), GPP_se = sd(GPP) / sqrt(dplyr::n()), .groups = "drop") %>%
  mutate(GPP_se = replace_na(GPP_se, 0))

# Общая шкала Y
y_max_fixed <- suppressWarnings(max(c(light_all$GPP, curve_tbl$GPP_hat), na.rm = TRUE))
if (!is.finite(y_max_fixed) || y_max_fixed <= 0) y_max_fixed <- 10
y_breaks <- pretty(c(0, y_max_fixed), n = 6)
y_max_fixed <- max(y_breaks)

# Аннотации
top_pad <- y_max_fixed * 0.04
y_step  <- max(y_max_fixed * 0.08, 1.0)
x_pad   <- 30

anno_fixed <- coef_tbl %>%
  group_by(Phase_lab) %>%
  arrange(Site) %>%
  mutate(
    x     = x_pad,
    y     = y_max_fixed - top_pad - (row_number() - 1) * y_step,
    label = paste0("\u03b1 = ", fmt_num(alpha), "  \u03b2 = ", fmt_num(beta))
  ) %>%
  ungroup()

p_light <- ggplot() +
  geom_point(data = light_all, aes(PPFD, GPP, color = Site), alpha = 0.25, size = 1) +
  geom_errorbar(data = bins_tbl,
                aes(x = PPFD_bin + (as.numeric(factor(Site)) - 1.5) * bin_w / 6,
                    ymin = GPP_mean - 1.96 * GPP_se,
                    ymax = GPP_mean + 1.96 * GPP_se,
                    color = Site), width = bin_w / 4, alpha = 0.6) +
  geom_point(data = bins_tbl,
             aes(x = PPFD_bin + (as.numeric(factor(Site)) - 1.5) * bin_w / 6,
                 y = GPP_mean, color = Site), size = 1.6, alpha = 0.8) +
  geom_line(data = curve_tbl, aes(PPFD, GPP_hat, color = Site), linewidth = 1.1) +
  geom_text(data = anno_fixed, aes(x = x, y = y, label = label, color = Site),
            hjust = 0, vjust = 1, size = 3.5, fontface = "bold") +
  facet_wrap(~Phase_lab, ncol = 3, scales = "fixed") +
  scale_color_manual(values = pal_site, name = "Станция") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Световые кривые фотосинтеза по фенофазам — Курск vs Москва",
       x = "PPFD (мкмоль фотонов м\u207b\u00b2 с\u207b\u00b9)",
       y = "GPP (мкмоль CO\u2082 м\u207b\u00b2 с\u207b\u00b9)") +
  theme_base

ggsave("KM_light_curves.png", p_light, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_light_curves.png\n")

# =====================================================================
# 5. WUE И iWUE ПО ФЕНОФАЗАМ
# =====================================================================

cat("\n========================================\n")
cat("WUE И iWUE ПО ФЕНОФАЗАМ\n")
cat("========================================\n\n")

prep_wue <- function(df) {
  df %>%
    filter(!is.na(Phase_lab),
           is.finite(GPP), GPP >= 0, GPP <= 40,
           is.finite(LE),  LE >= 5) %>%
    mutate(
      ET_kg  = ifelse(is.finite(LE) & is.finite(Tair),
                      bigleaf::LE.to.ET(LE, Tair),
                      ifelse(is.finite(LE), LE / 2.45e6, NA_real_)),
      E_mmol = ET_kg * 1000 / 0.018,
      WUE    = ifelse(is.finite(E_mmol) & E_mmol > 0 & is.finite(GPP),
                      GPP / E_mmol, NA_real_),
      IWUE   = ifelse(is.finite(E_mmol) & E_mmol > 0 & is.finite(GPP) & is.finite(VPD),
                      GPP * VPD / E_mmol, NA_real_)
    ) %>%
    filter(is.finite(WUE), WUE >= 0, WUE <= 40) %>%
    select(Site, datetime, Phase_lab, GPP, LE, VPD, Tair, PPFD, E_mmol, WUE, IWUE)
}

w_all <- prep_wue(df_all)

cat(sprintf("  Данные WUE: всего=%d, Курск=%d, Москва=%d\n",
            nrow(w_all), sum(w_all$Site == "Курск"), sum(w_all$Site == "Москва")))

# Сводные таблицы
summarise_wue <- function(d, value = "WUE") {
  d %>%
    group_by(Site, Phase_lab) %>%
    summarise(
      n  = dplyr::n(),
      mn = mean(.data[[value]], na.rm = TRUE),
      sd = sd(.data[[value]],   na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = "drop"
    ) %>%
    arrange(Phase_lab, Site)
}

wue_phase  <- summarise_wue(w_all, "WUE")
iwue_phase <- w_all %>% filter(is.finite(IWUE)) %>% summarise_wue("IWUE")

readr::write_csv(wue_phase,  "KM_WUE_by_phase.csv")
readr::write_csv(iwue_phase, "KM_iWUE_by_phase.csv")

# --- WUE барплот ---
ymax_wue <- suppressWarnings(max(wue_phase$mn + 1.96 * wue_phase$se, na.rm = TRUE))
if (!is.finite(ymax_wue) || ymax_wue <= 1) ymax_wue <- 10
ybreaks_wue <- pretty(c(0, ymax_wue), 6); ymax_wue <- max(ybreaks_wue)

pos <- position_dodge(width = 0.8)
p_wue_bar <- ggplot(wue_phase, aes(x = Phase_lab, y = mn, fill = Site)) +
  geom_col(position = pos, width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, mn - 1.96 * se), ymax = mn + 1.96 * se),
                position = pos, width = 0.2, linewidth = 0.5) +
  scale_fill_manual(values = pal_site, name = "Станция") +
  scale_y_continuous(limits = c(0, ymax_wue), breaks = ybreaks_wue,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "WUE по фенофазам — Курск vs Москва",
       x = "Фенофаза", y = "WUE (мкмоль CO\u2082 / ммоль H\u2082O)") +
  theme_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("KM_WUE_barplot.png", p_wue_bar, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_WUE_barplot.png\n")

# --- iWUE барплот ---
if (nrow(iwue_phase) > 0) {
  ymax_iwue <- suppressWarnings(max(iwue_phase$mn + 1.96 * iwue_phase$se, na.rm = TRUE))
  if (!is.finite(ymax_iwue) || ymax_iwue <= 1) ymax_iwue <- 20
  ybreaks_iwue <- pretty(c(0, ymax_iwue), 6); ymax_iwue <- max(ybreaks_iwue)

  p_iwue_bar <- ggplot(iwue_phase, aes(x = Phase_lab, y = mn, fill = Site)) +
    geom_col(position = pos, width = 0.7, alpha = 0.9) +
    geom_errorbar(aes(ymin = pmax(0, mn - 1.96 * se), ymax = mn + 1.96 * se),
                  position = pos, width = 0.2, linewidth = 0.5) +
    scale_fill_manual(values = pal_site, name = "Станция") +
    scale_y_continuous(limits = c(0, ymax_iwue), breaks = ybreaks_iwue,
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "iWUE по фенофазам — Курск vs Москва",
         x = "Фенофаза",
         y = "iWUE = GPP\u00d7VPD / ET (мкмоль CO\u2082 кПа / ммоль H\u2082O)") +
    theme_base +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("KM_iWUE_barplot.png", p_iwue_bar, width = 12, height = 8, dpi = 800, bg = "white")
  cat("  -> KM_iWUE_barplot.png\n")
}

# --- WUE boxplot ---
p_wue_box <- ggplot(w_all, aes(x = Phase_lab, y = WUE, fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = pal_site, name = "Станция") +
  labs(title = "WUE по фенофазам — сравнение Курск vs Москва",
       x = "Фенофаза", y = "WUE (мкмоль CO\u2082 / ммоль H\u2082O)") +
  theme_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("KM_WUE_boxplot.png", p_wue_box, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_WUE_boxplot.png\n")

# --- iWUE boxplot ---
if (sum(is.finite(w_all$IWUE)) > 10) {
  p_iwue_box <- ggplot(w_all %>% filter(is.finite(IWUE)),
                        aes(x = Phase_lab, y = IWUE, fill = Site)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = pal_site, name = "Станция") +
    labs(title = "iWUE по фенофазам — сравнение Курск vs Москва",
         x = "Фенофаза",
         y = "iWUE (мкмоль CO\u2082 кПа / ммоль H\u2082O)") +
    theme_base +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("KM_iWUE_boxplot.png", p_iwue_box, width = 12, height = 8, dpi = 800, bg = "white")
  cat("  -> KM_iWUE_boxplot.png\n")
}

# =====================================================================
# 6. КУМУЛЯТИВНЫЕ ПОТОКИ ЗА ВЕГЕТАЦИОННЫЙ ПЕРИОД
# =====================================================================

cat("\n========================================\n")
cat("КУМУЛЯТИВНЫЕ ПОТОКИ\n")
cat("========================================\n\n")

# Таблица границ вегетационного периода
season_bounds <- tibble(
  Site       = c("Курск", "Москва"),
  Sowing     = as.Date(c(B_KURSK$Sowing, B_MOSCOW$Sowing)),
  Harvesting = as.Date(c(B_KURSK$Harvesting, B_MOSCOW$Harvesting))
)

coeff_gC <- 1800 * 12 / 1e6  # µmol CO2 m-2 s-1 → g C m-2 за 30 мин

df_daily <- df_all %>%
  filter(!is.na(Date)) %>%
  inner_join(season_bounds, by = "Site") %>%
  filter(Date >= Sowing, Date <= Harvesting) %>%
  mutate(
    GPP_gC  = ifelse(is.finite(GPP),  GPP  * coeff_gC, 0),
    Reco_gC = ifelse(is.finite(Reco), Reco * coeff_gC, 0),
    NEE_gC  = ifelse(is.finite(NEE),  NEE  * coeff_gC, 0)
  ) %>%
  group_by(Site, Date, Sowing) %>%
  summarise(
    GPP_daily  = sum(GPP_gC,  na.rm = TRUE),
    Reco_daily = sum(Reco_gC, na.rm = TRUE),
    NEE_daily  = sum(NEE_gC,  na.rm = TRUE),
    n_half_hours = n(),
    .groups = "drop"
  ) %>%
  mutate(
    NEE_daily_balance = Reco_daily - GPP_daily,
    Days_from_sowing  = as.numeric(Date - Sowing) + 1
  )

# Заполнение пропущенных дней
df_daily_complete <- df_daily %>%
  group_by(Site) %>%
  group_modify(~{
    sow <- unique(.x$Sowing)
    all_dates <- tibble(Date = seq(min(.x$Date), max(.x$Date), by = "1 day"))
    all_dates %>%
      left_join(.x %>% select(-Sowing), by = "Date") %>%
      mutate(
        GPP_daily  = ifelse(is.na(GPP_daily),  0, GPP_daily),
        Reco_daily = ifelse(is.na(Reco_daily), 0, Reco_daily),
        NEE_daily  = ifelse(is.na(NEE_daily),  0, NEE_daily),
        NEE_daily_balance = Reco_daily - GPP_daily,
        n_half_hours = ifelse(is.na(n_half_hours), 0L, n_half_hours),
        Days_from_sowing = as.numeric(Date - sow) + 1
      )
  }) %>%
  ungroup()

# Кумулятивные суммы
df_cum <- df_daily_complete %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  mutate(
    GPP_cum  = cumsum(GPP_daily),
    Reco_cum = cumsum(Reco_daily),
    NEE_cum  = cumsum(NEE_daily_balance)
  ) %>%
  ungroup()

# Вертикальные линии фенофаз в днях от сева
phase_lines <- tibble(
  Site  = rep(c("Курск", "Москва"), each = 6),
  Phase = rep(PHASE6_RU, 2),
  Days  = c(
    # Курск
    as.numeric(as.Date(B_KURSK$Emergence) - as.Date(B_KURSK$Sowing)) + 1,
    as.numeric(as.Date(B_KURSK$Tillering) - as.Date(B_KURSK$Sowing)) + 1,
    as.numeric(as.Date(B_KURSK$StemElong) - as.Date(B_KURSK$Sowing)) + 1,
    as.numeric(as.Date(B_KURSK$Heading)   - as.Date(B_KURSK$Sowing)) + 1,
    as.numeric(as.Date(B_KURSK$Flowering) - as.Date(B_KURSK$Sowing)) + 1,
    as.numeric(as.Date(B_KURSK$Ripening)  - as.Date(B_KURSK$Sowing)) + 1,
    # Москва
    as.numeric(as.Date(B_MOSCOW$Emergence) - as.Date(B_MOSCOW$Sowing)) + 1,
    as.numeric(as.Date(B_MOSCOW$Tillering) - as.Date(B_MOSCOW$Sowing)) + 1,
    as.numeric(as.Date(B_MOSCOW$StemElong) - as.Date(B_MOSCOW$Sowing)) + 1,
    as.numeric(as.Date(B_MOSCOW$Heading)   - as.Date(B_MOSCOW$Sowing)) + 1,
    as.numeric(as.Date(B_MOSCOW$Flowering) - as.Date(B_MOSCOW$Sowing)) + 1,
    as.numeric(as.Date(B_MOSCOW$Ripening)  - as.Date(B_MOSCOW$Sowing)) + 1
  )
)

# --- Кумулятивный GPP ---
p_gpp_cum <- ggplot(df_cum, aes(x = Days_from_sowing, y = GPP_cum, color = Site)) +
  geom_vline(data = phase_lines, aes(xintercept = Days, color = Site),
             linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = pal_site, name = "Станция") +
  labs(title = "Кумулятивный GPP по дням вегетационного периода",
       subtitle = "Пунктир — начало фенофаз",
       x = "Дни от посева", y = "Кумулятивный GPP (г C м\u207b\u00b2)") +
  theme_base

ggsave("KM_cumulative_GPP.png", p_gpp_cum, width = 12, height = 8, dpi = 800, bg = "white")

# --- Кумулятивный Reco ---
p_reco_cum <- ggplot(df_cum, aes(x = Days_from_sowing, y = Reco_cum, color = Site)) +
  geom_vline(data = phase_lines, aes(xintercept = Days, color = Site),
             linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = pal_site, name = "Станция") +
  labs(title = "Кумулятивный Reco по дням вегетационного периода",
       subtitle = "Пунктир — начало фенофаз",
       x = "Дни от посева", y = "Кумулятивный Reco (г C м\u207b\u00b2)") +
  theme_base

ggsave("KM_cumulative_Reco.png", p_reco_cum, width = 12, height = 8, dpi = 800, bg = "white")

# --- Кумулятивный NEE ---
p_nee_cum <- ggplot(df_cum, aes(x = Days_from_sowing, y = NEE_cum, color = Site)) +
  geom_vline(data = phase_lines, aes(xintercept = Days, color = Site),
             linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  scale_color_manual(values = pal_site, name = "Станция") +
  labs(title = "Кумулятивный NEE по дням вегетационного периода",
       subtitle = "Пунктир — начало фенофаз; отрицательные значения = поглощение CO\u2082",
       x = "Дни от посева", y = "Кумулятивный NEE (г C м\u207b\u00b2)") +
  theme_base

ggsave("KM_cumulative_NEE.png", p_nee_cum, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_cumulative_GPP/Reco/NEE.png\n")

# --- Кумулятивные суммы активных температур (GDD >10°C) ---
gdd_daily <- df_all %>%
  filter(!is.na(Date), !is.na(Tair)) %>%
  inner_join(season_bounds, by = "Site") %>%
  filter(Date >= Sowing, Date <= Harvesting) %>%
  group_by(Site, Date, Sowing) %>%
  summarise(Tair_mean = mean(Tair, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    GDD_daily = pmax(0, Tair_mean - 10),
    Days_from_sowing = as.numeric(Date - Sowing) + 1
  ) %>%
  select(-Sowing) %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  mutate(GDD_cum = cumsum(GDD_daily)) %>%
  ungroup()

p_gdd <- ggplot(gdd_daily, aes(x = Days_from_sowing, y = GDD_cum, color = Site)) +
  geom_vline(data = phase_lines, aes(xintercept = Days, color = Site),
             linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = pal_site, name = "Станция") +
  labs(title = "Кумулятивная сумма активных температур (>10\u00b0C)",
       subtitle = "Пунктир — начало фенофаз",
       x = "Дни от посева", y = "GDD (\u00b0C\u00b7день)") +
  theme_base

ggsave("KM_cumulative_GDD.png", p_gdd, width = 12, height = 8, dpi = 800, bg = "white")
cat("  -> KM_cumulative_GDD.png\n")

# =====================================================================
# 7. ИТОГОВАЯ СТАТИСТИКА
# =====================================================================

cat("\n========================================\n")
cat("SUMMARY STATISTICS\n")
cat("========================================\n\n")

# --- Итоговые кумулятивные суммы ---
cumulative_summary <- df_cum %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    Start_date     = min(Date, na.rm = TRUE),
    End_date       = max(Date, na.rm = TRUE),
    Days           = as.numeric(End_date - Start_date) + 1,
    GPP_total_gC   = dplyr::last(GPP_cum),
    Reco_total_gC  = dplyr::last(Reco_cum),
    NEE_total_gC   = dplyr::last(NEE_cum),
    GPP_mean_daily = mean(GPP_daily, na.rm = TRUE),
    Reco_mean_daily = mean(Reco_daily, na.rm = TRUE),
    NEE_mean_daily = mean(NEE_daily_balance, na.rm = TRUE),
    .groups = "drop"
  )

cat("Cumulative flux totals (g C m-2):\n")
print(cumulative_summary)

# GDD итого
gdd_summary <- gdd_daily %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    GDD_total        = dplyr::last(GDD_cum),
    GDD_mean_daily   = mean(GDD_daily, na.rm = TRUE),
    Days_above_10C   = sum(GDD_daily > 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nGrowing degree days (>10C):\n")
print(gdd_summary)

# Объединённая сравнительная таблица
comparison_table <- cumulative_summary %>%
  left_join(gdd_summary, by = "Site") %>%
  select(Site, Days, GPP_total_gC, Reco_total_gC, NEE_total_gC,
         GDD_total, Days_above_10C)

cat("\nFull comparison table:\n")
print(comparison_table)
readr::write_csv(comparison_table, "KM_cumulative_comparison.csv")

# --- Сводная статистика по фенофазам ---
cat("\nFlux statistics by site and phenophase:\n\n")

flux_stats <- df_all %>%
  filter(!is.na(Phase_lab)) %>%
  group_by(Site, Phase_lab) %>%
  summarise(
    n = n(),
    GPP_mean  = mean(GPP,  na.rm = TRUE),
    GPP_sd    = sd(GPP,    na.rm = TRUE),
    Reco_mean = mean(Reco, na.rm = TRUE),
    Reco_sd   = sd(Reco,   na.rm = TRUE),
    NEE_mean  = mean(NEE,  na.rm = TRUE),
    NEE_sd    = sd(NEE,    na.rm = TRUE),
    VPD_mean  = mean(VPD,  na.rm = TRUE),
    Tair_mean = mean(Tair, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Phase_lab, Site)

print(flux_stats, n = Inf)
readr::write_csv(flux_stats, "KM_flux_stats_by_phase.csv")

# --- WUE/iWUE сводка ---
cat("\nWUE statistics by site and phenophase:\n\n")
print(wue_phase, n = Inf)

if (nrow(iwue_phase) > 0) {
  cat("\niWUE statistics by site and phenophase:\n\n")
  print(iwue_phase, n = Inf)
}

# --- Общий WUE по сайтам ---
cat("\nOverall WUE by site:\n")
w_all %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    WUE_mean  = mean(WUE,  na.rm = TRUE),
    WUE_sd    = sd(WUE,    na.rm = TRUE),
    IWUE_mean = mean(IWUE, na.rm = TRUE),
    IWUE_sd   = sd(IWUE,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\n========================================\n")
cat("ГОТОВО. Все файлы сохранены.\n")
cat("========================================\n\n")

cat("Графики:\n")
cat("  KM_diurnal_NEE.png / KM_diurnal_GPP.png / KM_diurnal_Reco.png\n")
cat("  KM_light_curves.png\n")
cat("  KM_WUE_barplot.png / KM_iWUE_barplot.png\n")
cat("  KM_WUE_boxplot.png / KM_iWUE_boxplot.png\n")
cat("  KM_cumulative_GPP.png / KM_cumulative_Reco.png / KM_cumulative_NEE.png\n")
cat("  KM_cumulative_GDD.png\n")
cat("\nТаблицы:\n")
cat("  KM_diurnal_summary.csv\n")
cat("  KM_light_response_coefficients.csv\n")
cat("  KM_WUE_by_phase.csv / KM_iWUE_by_phase.csv\n")
cat("  KM_flux_stats_by_phase.csv\n")
cat("  KM_cumulative_comparison.csv\n")
cat("  KM_phase_duration_comparison.csv\n")
