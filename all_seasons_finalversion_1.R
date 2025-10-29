# ======================================================================
# Полный скрипт: сравнение DIURNAL за 2013, 2016, 2023 (6 фенофаз, без «После уборки»)
# - 2023: используем заранее усреднённые по biomass_f потоки (d23_avg или CSV)
# - 2013/2016: робастный парсинг времени, жёсткое назначение 6 фаз по датам
# - Диурналы для NEE, GPP, Reco: ось X = 0..23; усреднение «сначала по дням → по фазе»
# - На выход: три PNG + CSV со сводкой
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(stringr)
  library(bigleaf)
  library(tidyr)
})

# ----------------------- Константы фаз -----------------------
PHASE6_EN <- c("Emergence","Tillering","StemElong","Heading","Flowering","Ripening")
PHASE6_RU <- c("Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание")

# Границы фаз по датам (6 фаз + Harvesting для отсечки)
B2013 <- list(
  Sowing="2013-05-05", Emergence="2013-05-12", Tillering="2013-05-28",
  StemElong="2013-06-15", Heading="2013-06-30", Flowering="2013-07-08",
  Ripening="2013-07-22", Harvesting="2013-08-14"
)
B2016 <- list(
  Sowing="2016-05-11", Emergence="2016-05-18", Tillering="2016-05-26",
  StemElong="2016-06-08", Heading="2016-06-22", Flowering="2016-06-30",
  Ripening="2016-08-12", Harvesting="2016-08-27"
)

# ----------------------- Утилиты -----------------------
to_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

pick_col <- function(nm, pats){
  for (p in pats){ 
    hit <- grep(paste0("^",p,"$"), nm, ignore.case=TRUE, value=TRUE)
    if (length(hit)) return(hit[1]) 
  }
  low <- tolower(nm)
  for (p in pats){ 
    hit <- nm[str_detect(low, tolower(p))]
    if (length(hit)) return(hit[1]) 
  }
  NA_character_
}

# ======= Парсинг времени БЕЗ parse_date_time с длинными orders (избегаем regex-ошибок) =======
parse_with_formats <- function(x, fmts, tz="UTC"){
  xch <- as.character(x)
  for (f in fmts){
    out <- suppressWarnings(as.POSIXct(strptime(xch, format=f, tz=tz)))
    if (sum(!is.na(out)) > 0) return(out)
  }
  rep(as.POSIXct(NA, tz=tz), length(xch))
}

parse_compact_12_14 <- function(x, tz="UTC"){
  # Поддержка 201305100030(00)
  xch <- gsub("[^0-9]", "", as.character(x))
  n <- nchar(xch)
  out <- rep(as.POSIXct(NA, tz=tz), length(xch))
  idx14 <- which(n == 14); if (length(idx14)) out[idx14] <- suppressWarnings(as.POSIXct(strptime(xch[idx14], "%Y%m%d%H%M%S", tz=tz)))
  idx12 <- which(n == 12); if (length(idx12)) out[idx12] <- suppressWarnings(as.POSIXct(strptime(xch[idx12], "%Y%m%d%H%M",   tz=tz)))
  out
}

parse_iso_simple <- function(x, tz="UTC"){
  xch <- as.character(x)
  xch <- gsub("Z$", "", xch, ignore.case = TRUE)
  xch <- gsub("T", " ", xch, fixed = TRUE)
  parse_with_formats(xch,
                     c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                       "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
                       "%Y.%m.%d %H:%M:%S", "%Y.%m.%d %H:%M",
                       "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d"),
                     tz=tz)
}

parse_dmy_simple <- function(x, tz="UTC"){
  parse_with_formats(x,
                     c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M",
                       "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M",
                       "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
                       "%d.%m.%Y", "%d-%m-%Y", "%d/%m/%Y"),
                     tz=tz)
}

best_time_from_candidates <- function(df, tz="UTC"){
  nm <- names(df)
  pref <- c("timestamp_start","timestamp_end","datetime","datetime_1",
            "date_time","date_time_1","date","time","time_utc")
  cand <- union(intersect(pref, nm), nm[str_detect(nm, "(date|time|stamp)")])
  if (!length(cand)) return(rep(as.POSIXct(NA, tz=tz), nrow(df)))

  best_out <- rep(as.POSIXct(NA, tz=tz), nrow(df)); best_n <- -1L
  for (cn in cand){
    v <- df[[cn]]
    out <- rep(as.POSIXct(NA, tz=tz), length(v))

    if (is.numeric(v) && suppressWarnings(max(v, na.rm=TRUE) > 1e9)) {
      out <- as.POSIXct(v, origin="1970-01-01", tz=tz)
    } else {
      out <- parse_compact_12_14(v, tz=tz)
      if (all(is.na(out))) out <- parse_iso_simple(v, tz=tz)
      if (all(is.na(out))) out <- parse_dmy_simple(v, tz=tz)
      if (all(is.na(out))) out <- suppressWarnings(ymd_hms(v, tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(ymd_hm(v,  tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(dmy_hms(v, tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(dmy_hm(v,  tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(ymd(v,     tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(dmy(v,     tz=tz, quiet=TRUE))
    }

    n_ok <- sum(!is.na(out))
    if (n_ok > best_n){
      best_n <- n_ok
      best_out <- out
      if (best_n == length(v)) break
    }
  }
  best_out
}

force_year <- function(dt, year_ref){
  if (all(is.na(dt))) return(dt)
  y <- suppressWarnings(lubridate::year(dt))
  y_med <- suppressWarnings(stats::median(y, na.rm = TRUE))
  if (is.finite(y_med) && as.integer(round(y_med)) != year_ref) {
    return(update(dt, year = year_ref))
  }
  dt
}

# ЖЁСТКОЕ назначение 6 фаз по интервалам
phase_by_bounds <- function(date_vec, bounds){
  e  <- as.Date(bounds$Emergence); t  <- as.Date(bounds$Tillering)
  s  <- as.Date(bounds$StemElong); h  <- as.Date(bounds$Heading)
  f  <- as.Date(bounds$Flowering); r  <- as.Date(bounds$Ripening)
  hv <- as.Date(bounds$Harvesting)
  res <- rep(NA_character_, length(date_vec))
  res[ date_vec >= e  & date_vec <  t ] <- "Emergence"
  res[ date_vec >= t  & date_vec <  s ] <- "Tillering"
  res[ date_vec >= s  & date_vec <  h ] <- "StemElong"
  res[ date_vec >= h  & date_vec <  f ] <- "Heading"
  res[ date_vec >= f  & date_vec <  r ] <- "Flowering"
  res[ date_vec >= r  & date_vec <= hv] <- "Ripening"
  factor(res, levels = PHASE6_EN)
}

# Страховка: если в году пропала «Кущение», пометить окно [Tillering; StemElong)
fix_tillering_if_missing <- function(df, bounds){
  if (!all(c("Date","Phase") %in% names(df))) return(df)
  if (sum(df$Phase == "Tillering", na.rm = TRUE) == 0) {
    t0 <- as.Date(bounds$Tillering); s0 <- as.Date(bounds$StemElong)
    sel <- which(!is.na(df$Date) & df$Date >= t0 & df$Date < s0)
    if (length(sel)) df$Phase[sel] <- "Tillering"
  }
  df
}

# Сборщик года (2013/2016): время + потоки + фазы
build_year_df <- function(raw, year, bounds, tz_in="UTC", shift_hours=0L){
  nm <- names(raw)
  dt <- best_time_from_candidates(raw, tz=tz_in)

  # Fallback: DOY + (hhmm|hour|minute)
  if (all(is.na(dt)) && "doy" %in% nm){
    H <- if ("hour" %in% nm) as.integer(raw$hour) else 0L
    M <- if ("minute" %in% nm) as.integer(raw$minute) else 0L
    if ("hhmm" %in% nm) { hh <- suppressWarnings(as.integer(raw$hhmm)); H <- floor(hh/100); M <- hh %% 100 }
    base <- as.Date(pmax(1L, pmin(366L, as.integer(raw$doy))) - 1L, origin = sprintf("%04d-01-01", year))
    dt <- as.POSIXct(base, tz=tz_in) + H*3600 + M*60
  }

  dt <- force_year(dt, year)
  if (shift_hours != 0L && any(!is.na(dt))) dt <- dt + hours(shift_hours)

  col_gpp  <- pick_col(nm, c("gpp_dt_u50","gpp_dt_u_star","gpp_u50_f","gpp_u50","gpp_u_star_f","gpp_f","gpp"))
  col_reco <- pick_col(nm, c("reco_dt_u50","reco_dt_u_star","reco_u50_f","reco_u50","reco_u_star_f","er","reco","reco_f"))
  col_nee  <- pick_col(nm, c("nee_u50_f","nee_u50","nee_u_star_f","nee_filled", "nee","fc"))

  GPP  <- if (!is.na(col_gpp))  to_num(raw[[col_gpp]])  else rep(NA_real_, nrow(raw))
  Reco <- if (!is.na(col_reco)) to_num(raw[[col_reco]]) else rep(NA_real_, nrow(raw))
  NEE  <- if (!is.na(col_nee))  to_num(raw[[col_nee]])  else Reco - GPP
  if (all(is.na(NEE)) && any(is.finite(GPP)) && any(is.finite(Reco))) NEE <- Reco - GPP

  out <- tibble(
    Year = year,
    datetime = dt,
    Date = as.Date(dt),
    HourInt = pmin(23L, pmax(0L, hour(dt))),
    NEE = NEE, GPP = GPP, Reco = Reco
  )

  # Фазы (строго 6, без «после уборки»)
  out$Phase <- phase_by_bounds(out$Date, bounds)
  out <- fix_tillering_if_missing(out, bounds)
  out$Phase_lab <- factor(as.character(out$Phase), levels = PHASE6_EN, labels = PHASE6_RU)

  out
}

# ----------------------- 2023: d23_avg или CSV -----------------------
if (!exists("d23_avg") && file.exists("fluxes_2023_biomass_mean.csv")) {
  d23_avg <- readr::read_csv("fluxes_2023_biomass_mean.csv", show_col_types = FALSE) |> clean_names()
}
ts23_col <- dplyr::first(intersect(c("timestamp_msk","timestamp","datetime"), names(d23_avg)))
stopifnot(length(ts23_col) == 1)

df23 <- d23_avg %>%
  rename(datetime = !!rlang::sym(ts23_col)) %>%
  mutate(
    Year = 2023L,
    Date = as.Date(datetime),
    HourInt = pmin(23L, pmax(0L, hour(datetime))),
    Phase_lab = factor(phase_lab, levels = PHASE6_RU),
    NEE = nee,
    GPP = gpp,
    Reco = reco,
    PPFD = ppfd
  ) %>%
  select(Year, datetime, Date, HourInt, Phase_lab, NEE, GPP, Reco, PPFD)

readr::write_csv(
  df23 %>%
    select(datetime, Phase_lab, starts_with("PPFD"), matches("(?i)^gpp$|^gpp_"),
           matches("(?i)^nee$|^nee_"), matches("(?i)^reco$|^reco_")) %>%
    arrange(datetime),
  "debug_df23_subset.csv"
)

# ----------------------- Загрузка 2013/2016 и сборка -----------------------
f2013 <- "Lasslop_2013_Complete_GapFilled.csv"
f2016 <- "Moscow_2016_verFin.csv"
stopifnot(file.exists(f2013), file.exists(f2016))
raw13 <- readr::read_csv(f2013, show_col_types = FALSE, guess_max = 1e6) |> clean_names()
raw16 <- readr::read_csv(f2016, show_col_types = FALSE, guess_max = 1e6) |> clean_names()

df13 <- build_year_df(raw13, 2013, B2013, tz_in="UTC", shift_hours=0L) 
df16 <- build_year_df(raw16, 2016, B2016, tz_in="UTC", shift_hours=0L)

# Диагностика (можно закомментировать):
cat("\nДиагностика фаз 2013/2016:\n")
print(bind_rows(df13, df16) %>% count(Year, Phase_lab))

# ----------------------- Объединение и сглаживание -----------------------
df_all <- bind_rows(df13, df16, df23) %>%
  filter(!is.na(datetime), !is.na(HourInt), !is.na(Phase_lab))

# шаг 1: среднее по ДНЯМ в каждом ЧАСЕ
by_day <- df_all %>%
  group_by(Year, Phase_lab, Date, HourInt) %>%
  summarise(
    NEE  = mean(NEE,  na.rm = TRUE),
    GPP  = mean(GPP,  na.rm = TRUE),
    Reco = mean(Reco, na.rm = TRUE),
    .groups = "drop"
  )

# шаг 2: среднее по дням фазы + SE + 95% CI
diu <- by_day %>%
  group_by(Year, Phase_lab, HourInt) %>%
  summarise(
    across(c(NEE,GPP,Reco),
           list(mean = ~mean(.x, na.rm=TRUE),
                se   = ~sd(.x, na.rm=TRUE)/sqrt(sum(is.finite(.x)))),
           .names="{.col}_{.fn}"),
    .groups="drop"
  ) %>%
  mutate(across(ends_with("_se"), ~replace_na(.x, 0)),
         NEE_lwr  = NEE_mean  - 1.96*NEE_se,  NEE_upr  = NEE_mean  + 1.96*NEE_se,
         GPP_lwr  = GPP_mean  - 1.96*GPP_se,  GPP_upr  = GPP_mean  + 1.96*GPP_se,
         Reco_lwr = Reco_mean - 1.96*Reco_se, Reco_upr = Reco_mean + 1.96*Reco_se)

# ----------------------- Графики -----------------------
pal_year <- c(`2013`="#1b9e77", `2016`="#d95f02", `2023`="#7570b3")
theme_base <- theme_bw(base_size = 12) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linewidth=0.2, colour="grey85"),
        strip.background=element_rect(fill="grey95", colour="grey80"),
        plot.title=element_text(face="bold", hjust=0.02),
        legend.position="bottom")

plot_flux <- function(flux){
  cols <- paste0(flux, c("_mean","_lwr","_upr"))
  ggplot(diu, aes(x = HourInt, y = .data[[cols[1]]],
                  color = factor(Year), fill = factor(Year))) +
    geom_ribbon(aes(ymin = .data[[cols[2]]], ymax = .data[[cols[3]]]),
                alpha = 0.14, colour = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.1) +
    facet_wrap(~Phase_lab, ncol = 3, drop = FALSE) +
    scale_color_manual(values = pal_year, name = "Год") +
    scale_fill_manual(values = pal_year, guide = "none") +
    scale_x_continuous(breaks = seq(0,23,6), limits = c(0,23), expand = c(0,0)) +
    labs(title = paste("Diurnal —", flux),
         x = "Час суток", y = "µmol CO₂ m⁻² s⁻¹") +
    theme_base
}

p_NEE  <- plot_flux("NEE")
p_GPP  <- plot_flux("GPP")
p_Reco <- plot_flux("Reco")

print(p_NEE); print(p_GPP); print(p_Reco)

# ----------------------- Сохранение результатов -----------------------
readr::write_csv(diu, "diurnal_summary_2013_2016_2023.csv")
ggsave("compare_diurnal_NEE_2013_2016_2023.png",  p_NEE,  width=12, height=8, dpi=300, bg="white")
ggsave("compare_diurnal_GPP_2013_2016_2023.png",  p_GPP,  width=12, height=8, dpi=300, bg="white")
ggsave("compare_diurnal_Reco_2013_2016_2023.png", p_Reco, width=12, height=8, dpi=300, bg="white")

cat("\nГотово. Файлы сохранены:\n  - diurnal_summary_2013_2016_2023.csv\n  - compare_diurnal_*.png\n")

# СВЕТОВЫЕ КРИВЫЕ

df23$Phase_lab <- factor(df23$Phase_lab, levels = PHASE6_RU)
# === PATCH ===
# Добавьте этот блок ПЕРЕД первой строкой, где вызывается pick_first_present()
# (в самое начало файла или перед секцией подготовки данных по годам).

# Безопасное приведение к numeric (если у вас его нет — пригодится дальше)
safe_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x))  x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Возвращает ПЕРВОЕ существующее имя столбца из заданного списка candidates.
# Если ни один не найден — возвращает NA_character_.
pick_first_present <- function(nm, candidates){
  cand <- intersect(candidates, nm)
  if (length(cand) > 0) cand[1] else NA_character_
}

# Удобная «обёртка»: сразу стопаемся с понятным сообщением,
# если подходящая колонка не найдена.
first_or_stop <- function(df, candidates, label = "колонку"){
  col <- pick_first_present(names(df), candidates)
  if (is.na(col)) {
    stop(sprintf("Не нашли %s среди: %s\nИскали: %s",
                 label, paste(names(df), collapse=", "),
                 paste(candidates, collapse=", ")))
  }
  col
}
# === END PATCH ===
# прочитаем PPFD для 2023 из вложенного файла (если PPFD уже «живой» в df23 — пропустим join)
need_join_23 <- !("PPFD" %in% names(df23) && sum(is.finite(df23$PPFD) & df23$PPFD>10) > 100)
if (need_join_23) {
  pp23_path <- "fluxes_2023_biomass_mean.csv"
  stopifnot(file.exists(pp23_path))
  pp23_raw <- readr::read_csv(pp23_path, show_col_types = FALSE, guess_max = 1e6) %>% clean_names()
  t_pp     <- best_time_from_candidates(pp23_raw, tz = "UTC")
  pp_col   <- pick_first_present(names(pp23_raw), c("ppfd","ppfd_f","ppfd_fall","ppfd_orig","ppfd_mean","ppfd_u50"))
  stopifnot(!is.na(pp_col))
  pp23 <- tibble(datetime = t_pp, PPFD = safe_num(pp23_raw[[pp_col]])) %>%
    filter(!is.na(datetime)) %>%
    mutate(dt30 = floor_date(datetime, "30 minutes")) %>%
    group_by(dt30) %>% summarise(PPFD = mean(PPFD, na.rm=TRUE), .groups="drop")

  df23 <- df23 %>%
    mutate(dt30 = floor_date(datetime, "30 minutes")) %>%
    select(-any_of(c("PPFD","PPFD.x","PPFD.y"))) %>%
    left_join(pp23, by="dt30") %>%
    select(-dt30)
}

# --------------------- ГАРАНТИРОВАННОЕ приведение типов по КАЖДОМУ году ---------------------
# Выбираем ровно ОДНУ колонку GPP и ОДНУ PPFD, приводим к numeric ДО объединения.

prep_year <- function(df, year){
  stopifnot(all(c("datetime","Phase_lab") %in% names(df)))
  nm <- names(df)

  gpp_col <- pick_first_present(nm, c("GPP","gpp","gpp_dt_u50","gpp_dt_u_star","gpp_u50","gpp_u_star_f"))
  if (is.na(gpp_col)) stop(sprintf("(%s) Не нашли колонку GPP в df%02d", year, year))

  ppfd_col <- pick_first_present(nm, c("PPFD","ppfd","PPFD_f","ppfd_f","ppfd_mean","PPFD_mean","ppfd_orig","PPFD_orig"))
  if (is.na(ppfd_col)) stop(sprintf("(%s) Не нашли колонку PPFD в df%02d", year, year))

  out <- df %>%
    transmute(
      Year      = as.integer(year),
      datetime  = as.POSIXct(datetime, tz = "UTC"),
      Phase_lab = factor(Phase_lab, levels = PHASE6_RU),
      PPFD      = safe_num(.data[[ppfd_col]]),
      GPP       = safe_num(.data[[gpp_col]])
    )

  # жёсткая проверка: никаких factor/character не осталось
  stopifnot(is.numeric(out$PPFD), is.numeric(out$GPP))
  out
}

# df13/df16 уже есть из вашего пайплайна
stopifnot(exists("df13"), exists("df16"))
df13$Phase_lab <- factor(df13$Phase_lab, levels = PHASE6_RU)
df16$Phase_lab <- factor(df16$Phase_lab, levels = PHASE6_RU)
# ---------- утилиты ----------
safe_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x))  x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

pick_first_present <- function(nm, candidates){
  cand <- intersect(candidates, nm)
  if (length(cand)) cand[1] else NA_character_
}

best_time_from_candidates <- function(df, tz="UTC"){
  nm <- names(df)
  cand <- union(intersect(c("timestamp_msk","timestamp","datetime","date_time","date","time"), nm),
                nm[stringr::str_detect(nm,"(date|time|stamp)")])
  if (!length(cand)) return(rep(as.POSIXct(NA, tz=tz), nrow(df)))
  best <- rep(as.POSIXct(NA, tz=tz), nrow(df)); best_n <- -1L
  for (cn in cand){
    v <- df[[cn]]; out <- rep(as.POSIXct(NA, tz=tz), length(v))
    if (is.numeric(v) && suppressWarnings(max(v, na.rm=TRUE) > 1e9)) {
      out <- as.POSIXct(v, origin="1970-01-01", tz=tz)
    } else {
      x <- as.character(v); x <- gsub("Z$","",x); x <- gsub("T"," ",x,fixed=TRUE)
      for (f in c("%Y-%m-%d %H:%M:%S","%Y-%m-%d %H:%M",
                  "%Y/%m/%d %H:%M:%S","%Y/%m/%d %H:%M",
                  "%d.%m.%Y %H:%M:%S","%d.%m.%Y %H:%M",
                  "%Y-%m-%d","%Y/%m/%d","%d.%m.%Y")){
        out <- suppressWarnings(as.POSIXct(strptime(x, f, tz=tz)))
        if (sum(!is.na(out))>0) break
      }
      if (all(is.na(out))) out <- suppressWarnings(ymd_hms(v, tz=tz, quiet=TRUE))
      if (all(is.na(out))) out <- suppressWarnings(dmy_hms(v, tz=tz, quiet=TRUE))
    }
    n_ok <- sum(!is.na(out))
    if (n_ok > best_n){ best <- out; best_n <- n_ok; if (best_n==length(v)) break }
  }
  best
}

force_year <- function(dt, year_ref){
  if (all(is.na(dt))) return(dt)
  y <- suppressWarnings(lubridate::year(dt))
  y_med <- suppressWarnings(stats::median(y, na.rm=TRUE))
  if (is.finite(y_med) && as.integer(round(y_med)) != year_ref) {
    return(lubridate::update(dt, year = year_ref))
  }
  dt
}

# строим lookup PPFD из файла (если нет PPFD — считаем из радиации)
build_ppfd_lookup <- function(file, year, tz_in="UTC", shift_hours=0L, rg_to_ppfd=2.04){
  if (!file.exists(file)) stop("Нет файла: ", file)
  raw <- readr::read_csv(file, show_col_types = FALSE, guess_max = 1e6) %>% clean_names()

  t_raw <- best_time_from_candidates(raw, tz = tz_in)
  t_raw <- force_year(t_raw, year)
  if (shift_hours != 0L && any(!is.na(t_raw))) t_raw <- t_raw + hours(shift_hours)

  # пробуем явный PPFD
  pp_col <- pick_first_present(names(raw),
                               c("ppfd","ppfd_f","ppfd_fall","ppfd_orig","ppfd_mean","ppfd_u50","par","par_in","parin"))
  ppfd <- if (!is.na(pp_col)) safe_num(raw[[pp_col]]) else NA_real_

  # если его нет — считаем из радиации (Rg [W m-2] → PPFD [µmol m-2 s-1])
  if (sum(is.finite(ppfd) & ppfd>0, na.rm=TRUE) < 50) {
    rad_col <- pick_first_present(names(raw),
                                  c("rg","rg_f","rg_orig","sw_in","shortwave_in","kdown","pot_rad_new","pot_rad","r_global","rn_orig"))
    if (!is.na(rad_col)) {
      rad <- safe_num(raw[[rad_col]])
      # конверсия 2.04 µmol/J (как в bigleaf по умолчанию)
      ppfd <- rad * rg_to_ppfd
    }
  }

  tibble(datetime = t_raw, PPFD = ppfd) %>%
    filter(is.finite(datetime), is.finite(PPFD)) %>%
    mutate(dt30 = floor_date(datetime, "30 minutes")) %>%
    group_by(dt30) %>% summarise(PPFD = mean(PPFD, na.rm=TRUE), .groups="drop")
}

# присоединяем PPFD к df, сравнивая покрытие: берём лучшее
attach_ppfd <- function(df, ppfd_lookup){
  stopifnot(all(c("datetime","Phase_lab") %in% names(df)))
  has_ppfd <- "PPFD" %in% names(df)
  cov_old  <- if (has_ppfd) sum(is.finite(df$PPFD) & df$PPFD > 10) else 0L

  df_new <- df %>%
    mutate(dt30 = floor_date(datetime, "30 minutes")) %>%
    left_join(ppfd_lookup, by = c("dt30")) %>%
    select(-dt30)

  # если была своя PPFD — выбираем лучшую (с большим числом дневных точек)
  if (has_ppfd) {
    cov_new <- sum(is.finite(df_new$PPFD.y) & df_new$PPFD.y > 10)
    if (cov_new > cov_old) {
      df_new <- df_new %>% transmute(across(-c(PPFD.x, PPFD.y)),
                                     PPFD = PPFD.y)
    } else {
      df_new <- df_new %>% transmute(across(-c(PPFD.x, PPFD.y)),
                                     PPFD = PPFD.x)
    }
  } else {
    names(df_new)[names(df_new)=="PPFD"] <- "PPFD"
  }
  df_new
}

# ---------- ПРИМЕНЕНИЕ К 2013/2016 ----------
# укажите верные пути, если отличаются
file_2013 <- "Lasslop_2013_Complete_GapFilled.csv"
file_2016 <- "Moscow_2016_verFin.csv"

# 2013: в исходнике время было в UTC → сдвигаем к МСК +3 ч
pp2013 <- build_ppfd_lookup(file_2013, year = 2013, tz_in = "UTC", shift_hours = 0L)
df13   <- attach_ppfd(df13, pp2013)

# 2016: как правило уже локальное/UTC без сдвига
pp2016 <- build_ppfd_lookup(file_2016, year = 2016, tz_in = "UTC", shift_hours = 0L)
df16   <- attach_ppfd(df16, pp2016)

# ---------- Диагностика покрытия ----------
diag_pp <- bind_rows(
  df13 %>% mutate(Year = 2013),
  df16 %>% mutate(Year = 2016)
) %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    ppfd_non_na = sum(is.finite(PPFD)),
    ppfd_day    = sum(PPFD > 10, na.rm=TRUE),
    ppfd_min    = suppressWarnings(min(PPFD, na.rm=TRUE)),
    ppfd_med    = suppressWarnings(median(PPFD, na.rm=TRUE)),
    ppfd_max    = suppressWarnings(max(PPFD, na.rm=TRUE)),
    .groups="drop"
  )
print(diag_pp)

d13 <- prep_year(df13, 2013)
d16 <- prep_year(df16, 2016)
d23 <- prep_year(df23, 2023)

# --------------------- Объединение без «обнуления» ---------------------
# ВАЖНО: объединяем уже очищенные числовые колонки с одинаковыми именами.
df_all <- bind_rows(d13, d16, d23)

# Диагностика — убедимся, что после bind_rows всё живое:
cat("\n[CHK] Диагностика после объединения:\n")
print(
  df_all %>%
    group_by(Year) %>%
    summarise(
      n = n(),
      ppfd_n = sum(is.finite(PPFD)),
      ppfd_pos = sum(PPFD > 10, na.rm=TRUE),
      ppfd_min = suppressWarnings(min(PPFD, na.rm=TRUE)),
      ppfd_med = suppressWarnings(median(PPFD, na.rm=TRUE)),
      ppfd_max = suppressWarnings(max(PPFD, na.rm=TRUE)),
      gpp_n = sum(is.finite(GPP)),
      gpp_min = suppressWarnings(min(GPP, na.rm=TRUE)),
      gpp_med = suppressWarnings(median(GPP, na.rm=TRUE)),
      gpp_max = suppressWarnings(max(GPP, na.rm=TRUE)),
      .groups="drop"
    )
)

# --------------------- Построение световых кривых ---------------------
# Фильтры: дневные точки и реалистичные пределы
light_all <- df_all %>%
  filter(!is.na(Phase_lab),
         is.finite(PPFD), PPFD >= 10, PPFD <= 2200,
         is.finite(GPP),  GPP  >= 0,  GPP  <= 40)

# Биннинг по PPFD для устойчивого фита и «усиков»
bin_w <- 100
binned <- light_all %>%
  mutate(PPFD_bin = pmax(0, floor(PPFD/bin_w)*bin_w)) %>%
  group_by(Year, Phase_lab, PPFD_bin) %>%
  summarise(PPFD = mean(PPFD), GPP = mean(GPP), n = dplyr::n(), .groups="drop") %>%
  arrange(Year, Phase_lab, PPFD)

# Фит прямоугольной гиперболы y = (α β x)/(α x + β) с ограничениями
fit_lrc_group <- function(dat){
  dat <- arrange(dat, PPFD)
  if (nrow(dat) < 10 || diff(range(dat$PPFD)) < 200 || var(dat$GPP) < 0.1)
    return(tibble(alpha=NA_real_, beta=NA_real_))

  # стартовые значения: наклон «внизу» + асимптотический максимум
  low <- dat %>% filter(PPFD <= quantile(PPFD, 0.2, na.rm=TRUE))
  a0  <- suppressWarnings(coef(lm(GPP ~ 0 + PPFD, data = low)))[1]
  if (!is.finite(a0)) a0 <- 0.03
  a0  <- min(max(a0, 0.005), 0.12)
  b0  <- quantile(dat$GPP, 0.95, na.rm=TRUE); if (!is.finite(b0) || b0 <= 0) b0 <- max(dat$GPP, na.rm=TRUE)
  b0  <- min(max(b0, 5), 40)

  fit <- try(
    nls(GPP ~ (alpha*beta*PPFD)/(alpha*PPFD + beta),
        data = dat,
        start = list(alpha = a0, beta = b0),
        algorithm = "port",
        lower = c(alpha = 1e-4, beta = 1),
        upper = c(alpha = 0.2,  beta = 60),
        control = nls.control(maxiter = 500, warnOnly = TRUE)),
    silent = TRUE
  )

  if (!inherits(fit, "try-error")) {
    co <- coef(fit); return(tibble(alpha = unname(co["alpha"]), beta = unname(co["beta"])))
  }

  # Резерв: грубый грид-поиск
  grid_a <- c(0.005,0.01,0.02,0.03,0.05,0.08,0.12,0.2)
  grid_b <- c(5,8,10,15,20,30,40,60)
  best <- list(a=NA_real_, b=NA_real_, rss=Inf)
  for (aa in grid_a){
    for (bb in grid_b){
      pred <- (aa*bb*dat$PPFD)/(aa*dat$PPFD + bb)
      rss  <- sum((dat$GPP - pred)^2)
      if (is.finite(rss) && rss < best$rss) best <- list(a=aa, b=bb, rss=rss)
    }
  }
  tibble(alpha = best$a, beta = best$b)
}

coef_tbl <- binned %>%
  group_by(Year, Phase_lab) %>%
  group_modify(~fit_lrc_group(.x)) %>%
  ungroup()

# Сетка для кривых и сами кривые
# 1) xmax теперь считаем по КАЖДОЙ паре (Year, Phase_lab) из фактических данных,
#    а не по одной фазе на все годы.
range_tbl <- light_all %>%
  dplyr::group_by(Year, Phase_lab) %>%
  dplyr::summarise(vmax = suppressWarnings(max(PPFD, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::mutate(
    xmax = dplyr::case_when(
      is.finite(vmax) & vmax > 0 ~ pmin(vmax, 2000),
      TRUE                       ~ 500         # безопасный минимум, если данных мало
    )
  ) %>%
  dplyr::select(-vmax)

# 2) Кривые считаем «построчно» (rowwise), чтобы не обращаться к .y$xmax.
curve_tbl <- coef_tbl %>%
  dplyr::left_join(range_tbl, by = c("Year","Phase_lab")) %>%
  dplyr::filter(is.finite(alpha), is.finite(beta), is.finite(xmax), xmax > 0) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    data = list(tibble(
      PPFD    = seq(0, xmax, length.out = 200),
      GPP_hat = (alpha * beta * PPFD) / (alpha * PPFD + beta)
    ))
  ) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(data) %>%
  dplyr::select(Year, Phase_lab, PPFD, GPP_hat)

# 3) (Не обязательно) Проверка: нет ли пустых кривых
if (nrow(curve_tbl) == 0L) {
  warning("curve_tbl пуст: проверьте, что в coef_tbl есть конечные alpha/beta и в light_all есть дневные точки PPFD ≥ 10.")
}

# «Усики» (SE) для наглядности
bins_for_plot <- light_all %>%
  mutate(PPFD_bin = pmax(0, floor(PPFD/bin_w)*bin_w)) %>%
  group_by(Year, Phase_lab, PPFD_bin) %>%
  summarise(PPFD = mean(PPFD), GPP_mean = mean(GPP),
            GPP_se = sd(GPP)/sqrt(dplyr::n()), .groups="drop") %>%
  mutate(GPP_se = replace_na(GPP_se, 0))

# Аннотации α и β
y_top <- light_all %>%
  group_by(Phase_lab) %>% summarise(ymax = max(GPP, na.rm=TRUE), .groups="drop")

fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format="f", digits=2), "н/д")
anno <- coef_tbl %>%
  left_join(y_top, by="Phase_lab") %>%
  group_by(Phase_lab) %>% arrange(Year) %>%
  mutate(y = ymax * (0.95 - 0.08*(row_number()-1)),
         x = 30,
         label = paste0("α = ", fmt_num(alpha), "  β = ", fmt_num(beta))) %>%
  ungroup()

# --------------------- Графики (3 варианта) ---------------------
theme_base <- theme_bw(base_size=12) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linewidth=0.2, colour="grey85"),
        strip.background=element_rect(fill="grey95", colour="grey80"),
        plot.title=element_text(face="bold", hjust=0),
        legend.position="bottom")

# ===================== PATCH: фиксированные подписи α и β =====================
# Делает подписи в КАЖДОМ фасете вверху слева, на общих Y-осях.
# Замените ваш блок расчёта `anno` и построения графиков на этот.

# 0) Общая шкала Y (как раньше)
y_max_fixed <- suppressWarnings(max(c(light_all$GPP, curve_tbl$GPP_hat), na.rm = TRUE))
if (!is.finite(y_max_fixed) || y_max_fixed <= 0) y_max_fixed <- 10
y_breaks <- pretty(c(0, y_max_fixed), n = 6)
y_max_fixed <- max(y_breaks)

# 1) Формат чисел
fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format="f", digits=2), "н/д")

# 2) Фиксируем позиции подписей: слева сверху в каждом фасете
year_levels <- c(2013, 2016, 2023)             # порядок строк-подписей
top_pad     <- y_max_fixed * 0.04              # отступ сверху
y_step      <- max(y_max_fixed * 0.08, 1.0)    # шаг между годами (минимум 1)
x_pad       <- 30                              # отступ слева по PPFD

anno_fixed <- coef_tbl %>%
  mutate(Year = factor(Year, levels = year_levels)) %>%
  group_by(Phase_lab) %>%
  arrange(Year) %>%
  mutate(
    x     = x_pad,
    y     = y_max_fixed - top_pad - (row_number()-1) * y_step,
    label = paste0("α = ", fmt_num(alpha), "  β = ", fmt_num(beta))
  ) %>%
  ungroup()

stopifnot(exists("light_all"), exists("curve_tbl"), exists("coef_tbl"))

# ================================================================
# Сравнение световых кривых (2013, 2016, 2023) с формулой:
#   GPP_pred = (α_phase * β_phase * bigleaf::PPFD.to.Rg(PPFD_f)) /
#              (α_phase * bigleaf::PPFD.to.Rg(PPFD_f) + β_phase)
# ---------------------------------------------------------------
# Требуется: готовые df13, df16, df23 с колонками datetime, Phase_lab,
#            GPP (или аналоги) и PPFD_f/PPFD (или аналоги).
# Выход: три графика и таблица коэффициентов α, β по годам и фазам.
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# --- bigleaf для конверсии PPFD -> Rg ---
if (!requireNamespace("bigleaf", quietly = TRUE)) {
  install.packages("bigleaf", repos = "https://cloud.r-project.org")
}
library(bigleaf)

# ---------- настройки ----------
PHASE6_RU <- c("Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание")
pal_year  <- c(`2013`="#1b9e77", `2016`="#d95f02", `2023`="#7570b3")

# ---------- утилиты ----------
safe_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x))  x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
pick_first_present <- function(nm, candidates){
  cand <- intersect(candidates, nm)
  if (length(cand)) cand[1] else NA_character_
}
first_or_stop <- function(df, candidates, label = "колонку"){
  col <- pick_first_present(names(df), candidates)
  if (is.na(col)) stop(sprintf("Не нашли %s. Искали: %s",
                               label, paste(candidates, collapse=", ")))
  col
}

# ---------- стандартизация по году: берём РОВНО одну PPFD и одну GPP ----------
prep_year_ppfd_rg <- function(df, year){
  stopifnot(all(c("datetime","Phase_lab") %in% names(df)))
  gpp_col  <- first_or_stop(df,
                            c("GPP","gpp","gpp_dt_u50","gpp_dt_u_star","gpp_u50_f","gpp_u_star_f"),
                            "колонку GPP")
  ppfd_col <- first_or_stop(df,
                            c("PPFD_f","ppfd_f","PPFD","ppfd","ppfd_mean","PPFD_mean","ppfd_orig","PPFD_orig"),
                            "колонку PPFD/PPFD_f")

  out <- df %>%
    transmute(
      Year      = as.integer(year),
      datetime  = as.POSIXct(datetime, tz = "UTC"),
      Phase_lab = factor(Phase_lab, levels = PHASE6_RU),
      PPFD      = safe_num(.data[[ppfd_col]]),
      Rg        = bigleaf::PPFD.to.Rg(PPFD),      # перевод в радиацию (W m-2)
      GPP       = safe_num(.data[[gpp_col]])
    ) %>%
    # фильтры только для дальнейшего анализа (исходники НЕ трогаем)
    filter(!is.na(Phase_lab),
           is.finite(PPFD), PPFD >= 10, PPFD <= 2200,
           is.finite(Rg),   Rg   >= 5,  Rg   <= 1200,
           is.finite(GPP),  GPP  >= 0,  GPP  <= 40)
  out
}

# ---------- устойчивый фит α, β в терминах Rg ----------
# модель: GPP = (α * β * Rg) / (α * Rg + β)
fit_lrc_rg <- function(dat){
  dat <- arrange(dat, Rg)
  if (nrow(dat) < 12 || diff(range(dat$Rg)) < 50 || var(dat$GPP) < 0.05) {
    return(tibble(alpha = NA_real_, beta = NA_real_))
  }
  # биннинг для устойчивости
  bin_w <- 50
  db <- dat %>%
    mutate(Rg_bin = pmax(0, floor(Rg/bin_w)*bin_w)) %>%
    group_by(Rg_bin) %>%
    summarise(Rg = mean(Rg), GPP = mean(GPP), .groups="drop") %>%
    arrange(Rg)

  # стартовые оценки
  low <- db %>% filter(Rg <= quantile(Rg, 0.2, na.rm=TRUE))
  a0  <- suppressWarnings(coef(lm(GPP ~ 0 + Rg, data = low)))[1]; if (!is.finite(a0)) a0 <- 0.03
  a0  <- min(max(a0, 0.005), 0.12)
  b0  <- quantile(db$GPP, 0.95, na.rm=TRUE); if (!is.finite(b0) || b0 <= 0) b0 <- max(db$GPP, na.rm=TRUE)
  b0  <- min(max(b0, 5), 40)

  fit <- try(
    nls(GPP ~ (alpha*beta*Rg)/(alpha*Rg + beta),
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
    return(tibble(alpha = unname(co["alpha"]), beta = unname(co["beta"])))
  }

  # резервный грид-поиск
  grid_a <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.12, 0.2)
  grid_b <- c(   5,    8,   10,   15,   20,   30,   40,  60)
  best <- list(a = NA_real_, b = NA_real_, rss = Inf)
  for (aa in grid_a){
    for (bb in grid_b){
      pred <- (aa*bb*db$Rg)/(aa*db$Rg + bb)
      rss  <- sum((db$GPP - pred)^2)
      if (is.finite(rss) && rss < best$rss) best <- list(a=aa, b=bb, rss=rss)
    }
  }
  tibble(alpha = best$a, beta = best$b)
}

# ---------- подготовка всех лет ----------
stopifnot(exists("df13"), exists("df16"), exists("df23"))
df13$Phase_lab <- factor(df13$Phase_lab, levels = PHASE6_RU)
df16$Phase_lab <- factor(df16$Phase_lab, levels = PHASE6_RU)
df23$Phase_lab <- factor(df23$Phase_lab, levels = PHASE6_RU)

d13 <- prep_year_ppfd_rg(df13, 2013)
d16 <- prep_year_ppfd_rg(df16, 2016)
d23 <- prep_year_ppfd_rg(df23, 2023)

light_all <- bind_rows(d13, d16, d23)

# ---------- коэффициенты α, β по (год × фаза) ----------
coef_tbl <- light_all %>%
  group_by(Year, Phase_lab) %>%
  group_modify(~fit_lrc_rg(.x)) %>%
  ungroup()

# ---------- кривые: строим по PPFD-сетке, как просили (через Rg из PPFD) ----------
# xmax_PPFD — индивидуально для каждой пары «год × фаза»
range_tbl <- light_all %>%
  group_by(Year, Phase_lab) %>%
  summarise(PPFD_max = min(2000, max(PPFD, na.rm=TRUE)), .groups = "drop") %>%
  mutate(PPFD_max = ifelse(!is.finite(PPFD_max) | PPFD_max <= 0, 500, PPFD_max))

curve_tbl <- coef_tbl %>%
  left_join(range_tbl, by = c("Year","Phase_lab")) %>%
  filter(is.finite(alpha), is.finite(beta)) %>%
  rowwise() %>%
  mutate(
    data = list({
      xs_ppfd <- seq(0, PPFD_max, length.out = 200)
      xs_rg   <- bigleaf::PPFD.to.Rg(xs_ppfd)
      tibble(PPFD = xs_ppfd,
             GPP_hat = (alpha*beta*xs_rg)/(alpha*xs_rg + beta))
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(data) %>%
  select(Year, Phase_lab, PPFD, GPP_hat)

# ---------- биннинг для «усиков» (по PPFD) ----------
bin_w <- 100
bins_tbl <- light_all %>%
  mutate(PPFD_bin = pmax(0, floor(PPFD/bin_w)*bin_w)) %>%
  group_by(Year, Phase_lab, PPFD_bin) %>%
  summarise(GPP_mean = mean(GPP), GPP_se = sd(GPP)/sqrt(dplyr::n()), .groups="drop") %>%
  mutate(GPP_se = replace_na(GPP_se, 0))

# ---------- аннотации α и β ----------
y_top <- light_all %>% group_by(Phase_lab) %>% summarise(ymax = max(GPP, na.rm=TRUE), .groups="drop")
fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format="f", digits=2), "н/д")
anno <- coef_tbl %>%
  left_join(y_top, by="Phase_lab") %>%
  group_by(Phase_lab) %>% arrange(Year) %>%
  mutate(y = ymax * (0.95 - 0.08*(row_number()-1)),
         x = 30,
         label = paste0("α = ", fmt_num(alpha), "  β = ", fmt_num(beta))) %>%
  ungroup()

# ---------- графики ----------
theme_base <- theme_bw(base_size=12) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linewidth=0.2, colour="grey85"),
        strip.background=element_rect(fill="grey95", colour="grey80"),
        plot.title=element_text(face="bold", hjust=0),
        legend.position="bottom")

# 0) Общая шкала Y (как раньше)
y_max_fixed <- suppressWarnings(max(c(light_all$GPP, curve_tbl$GPP_hat), na.rm = TRUE))
if (!is.finite(y_max_fixed) || y_max_fixed <= 0) y_max_fixed <- 10
y_breaks <- pretty(c(0, y_max_fixed), n = 6)
y_max_fixed <- max(y_breaks)

# 1) Формат чисел
fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format="f", digits=2), "н/д")

# 2) Фиксируем позиции подписей: слева сверху в каждом фасете
year_levels <- c(2013, 2016, 2023)             # порядок строк-подписей
top_pad     <- y_max_fixed * 0.04              # отступ сверху
y_step      <- max(y_max_fixed * 0.08, 1.0)    # шаг между годами (минимум 1)
x_pad       <- 30                              # отступ слева по PPFD

anno_fixed <- coef_tbl %>%
  mutate(Year = factor(Year, levels = year_levels)) %>%
  group_by(Phase_lab) %>%
  arrange(Year) %>%
  mutate(
    x     = x_pad,
    y     = y_max_fixed - top_pad - (row_number()-1) * y_step,
    label = paste0("α = ", fmt_num(alpha), "  β = ", fmt_num(beta))
  ) %>%
  ungroup()

# 3) Графики с едиными осями Y и фиксированными подписями
p_whisk <- ggplot() +
  geom_point(data = light_all, aes(PPFD, GPP, color = factor(Year)), alpha=0.25, size=1) +
  geom_errorbar(data = bins_tbl,
                aes(x = PPFD_bin + (as.numeric(factor(Year))-2)*100/6,
                    ymin = GPP_mean-1.96*GPP_se, ymax = GPP_mean+1.96*GPP_se,
                    color=factor(Year)), width = 100/4, alpha=0.6) +
  geom_point(data = bins_tbl,
             aes(x = PPFD_bin + (as.numeric(factor(Year))-2)*100/6,
                 y = GPP_mean, color=factor(Year)), size=1.6, alpha=0.8) +
  geom_line(data = curve_tbl, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.1) +
  geom_text(data = anno_fixed, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.5, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  labs(title="Световая кривая — фазы (тренд с «усиками», общая ось Y)",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

p_lines_pts <- ggplot() +
  geom_point(data = light_all, aes(PPFD, GPP, color=factor(Year)), alpha=0.25, size=1) +
  geom_line (data = curve_tbl, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.1) +
  geom_text(data = anno_fixed, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.5, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  labs(title="Световая кривая — фазы",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

p_lines_only <- ggplot() +
  geom_line(data = curve_tbl, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.2) +
  geom_text(data = anno_fixed, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.7, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  labs(title="Световая кривая — фазы (только тренд, общая ось Y)",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

print(p_whisk); print(p_lines_pts); print(p_lines_only)
# При необходимости сохранить:
# ggsave("lightRG_whiskers_fixedY.png",     p_whisk,      width=12, height=8, dpi=300, bg="white")
# ggsave("lightRG_lines_points_fixedY.png", p_lines_pts,  width=12, height=8, dpi=300, bg="white")
# ggsave("lightRG_lines_only_fixedY.png",   p_lines_only, width=12, height=8, dpi=300, bg="white")

readr::write_csv(coef_tbl %>% arrange(Phase_lab, Year), "lightRG_coefficients_by_year_phase.csv")

# ================================================================
# PATCH: «Всходы» — кривая 2016 года пунктиром (или скрыть вовсе)
# ------------------------------------------------
# Вставьте блок ПОСЛЕ расчёта: light_all, bins_tbl, curve_tbl, coef_tbl
# и ДО построения трёх графиков.
# По умолчанию линия 2016 в фазе «Всходы» рисуется пунктиром.
# Можно скрыть её совсем, установив HIDE_2016_EMERGENCE <- TRUE.
# ================================================================

# --- настройки поведения ---
DASH_2016_EMERGENCE <- TRUE   # показать пунктиром
HIDE_2016_EMERGENCE <- TRUE  # если TRUE — вообще не рисовать кривую (только точки/усики останутся)

# --- единая шкала Y (если ещё не посчитана) ---
if (!exists("y_max_fixed") || !is.finite(y_max_fixed)) {
  y_max_fixed <- suppressWarnings(max(c(light_all$GPP, curve_tbl$GPP_hat), na.rm = TRUE))
  if (!is.finite(y_max_fixed) || y_max_fixed <= 0) y_max_fixed <- 10
  y_breaks <- pretty(c(0, y_max_fixed), n = 6)
  y_max_fixed <- max(y_breaks)
}

# --- подготовим модифицированную таблицу кривых ---
curve_tbl_mod <- curve_tbl %>%
  mutate(curve_style = ifelse(Year == 2016 & Phase_lab == "Всходы", "dashed", "solid"))

if (isTRUE(HIDE_2016_EMERGENCE)) {
  curve_tbl_mod <- curve_tbl_mod %>%
    filter(!(Year == 2016 & Phase_lab == "Всходы"))
}

# --- подписи α,β фиксируем вверху слева; если скрываем кривую — убираем и подпись ---
fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format="f", digits=2), "н/д")
year_levels <- c(2013, 2016, 2023)
top_pad  <- y_max_fixed * 0.04
y_step   <- max(y_max_fixed * 0.08, 1.0)
x_pad    <- 30

anno_fixed_mod <- coef_tbl %>%
  mutate(Year = factor(Year, levels = year_levels)) %>%
  group_by(Phase_lab) %>%
  arrange(Year) %>%
  mutate(
    x     = x_pad,
    y     = y_max_fixed - top_pad - (row_number()-1) * y_step,
    label = paste0("α = ", fmt_num(alpha), "  β = ", fmt_num(beta))
  ) %>%
  ungroup()

if (isTRUE(HIDE_2016_EMERGENCE)) {
  anno_fixed_mod <- anno_fixed_mod %>% filter(!(Year == 2016 & Phase_lab == "Всходы"))
}

# --- ГРАФИКИ ---------------------------------------------------

theme_base <- theme_bw(base_size=12) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linewidth=0.2, colour="grey85"),
        strip.background=element_rect(fill="grey95", colour="grey80"),
        plot.title=element_text(face="bold", hjust=0),
        legend.position="bottom")

# a) тренд + «усики» + точки
p_whisk <- ggplot() +
  geom_point(data = light_all, aes(PPFD, GPP, color = factor(Year)), alpha=0.25, size=1) +
  geom_errorbar(data = bins_tbl,
                aes(x = PPFD_bin + (as.numeric(factor(Year))-2)*100/6,
                    ymin = GPP_mean-1.96*GPP_se, ymax = GPP_mean+1.96*GPP_se,
                    color=factor(Year)),
                width = 100/4, alpha=0.6) +
  geom_point(data = bins_tbl,
             aes(x = PPFD_bin + (as.numeric(factor(Year))-2)*100/6,
                 y = GPP_mean, color=factor(Year)), size=1.6, alpha=0.8) +
  {
    # линия тренда: с типом линии по условию
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year), linetype=curve_style), linewidth=1.1)
    else
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.1)
  } +
  geom_text(data = anno_fixed_mod, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.5, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  {
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      scale_linetype_manual(values = c(solid = "solid", dashed = "22"), guide = "none")
    else
      NULL
  } +
  labs(title="Световая кривая — фазы (тренд с «усиками», общая ось Y)",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

# b) тренд + точки
p_lines_pts <- ggplot() +
  geom_point(data = light_all, aes(PPFD, GPP, color=factor(Year)), alpha=0.25, size=1) +
  {
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year), linetype=curve_style), linewidth=1.1)
    else
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.1)
  } +
  geom_text(data = anno_fixed_mod, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.5, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  {
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      scale_linetype_manual(values = c(solid = "solid", dashed = "22"), guide = "none")
    else
      NULL
  } +
  labs(title="Световая кривая — фазы",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

# c) только тренд
p_lines_only <- ggplot() +
  {
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year), linetype=curve_style), linewidth=1.2)
    else
      geom_line(data = curve_tbl_mod, aes(PPFD, GPP_hat, color=factor(Year)), linewidth=1.2)
  } +
  geom_text(data = anno_fixed_mod, aes(x=x, y=y, label=label, color=factor(Year)),
            hjust=0, vjust=1, size=3.7, fontface="bold") +
  facet_wrap(~Phase_lab, ncol=3, scales="fixed") +
  scale_color_manual(values=pal_year, name="Год") +
  scale_y_continuous(limits = c(0, y_max_fixed), breaks = y_breaks, expand = expansion(mult = c(0, 0.02))) +
  {
    if (isTRUE(DASH_2016_EMERGENCE) && !isTRUE(HIDE_2016_EMERGENCE))
      scale_linetype_manual(values = c(solid = "solid", dashed = "22"), guide = "none")
    else
      NULL
  } +
  labs(title="Световая кривая — фазы (только тренд, общая ось Y)",
       x="PPFD (µmol photons m⁻² s⁻¹)", y="GPP (µmol CO₂ m⁻² s⁻¹)") +
  theme_base

# --- показать/сохранить (как обычно) ---
print(p_whisk); print(p_lines_pts); print(p_lines_only)
# ggsave("lightRG_whiskers_fixedY.png",     p_whisk,      width=12, height=8, dpi=300, bg="white")
# ggsave("lightRG_lines_points_fixedY.png", p_lines_pts,  width=12, height=8, dpi=300, bg="white")
# ggsave("lightRG_lines_only_fixedY.png",   p_lines_only, width=12, height=8, dpi=300, bg="white")

# -----WUE----- палитра и порядок фаз -----
if (!exists("pal_year"))  pal_year  <- c(`2013`="#1b9e77", `2016`="#d95f02", `2023`="#7570b3")
if (!exists("PHASE6_RU")) PHASE6_RU <- c("Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание")

# ----- утилиты -----
safe_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x))  x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
pick_first_present <- function(nm, candidates){
  cand <- intersect(candidates, nm)
  if (length(cand)) cand[1] else NA_character_
}
first_or_stop <- function(df, candidates, label = "колонку"){
  col <- pick_first_present(names(df), candidates)
  if (is.na(col)) stop(sprintf("Не нашли %s. Искали: %s", label, paste(candidates, collapse=", ")))
  col
}

# ----- подготовка по году: выбираем ровно по 1 колонке для GPP/LE/VPD/Tair и считаем WUE -----
prep_year_wue <- function(df, year){
  stopifnot(all(c("datetime","Phase_lab") %in% names(df)))
  nm <- names(df)

  gpp_col  <- first_or_stop(df,
                            c("GPP","gpp","gpp_dt_u50","gpp_dt_u_star","gpp_u50_f","gpp_u_star_f"),
                            "GPP")
  le_col   <- first_or_stop(df,
                            c("LE_f","le","le_f","le_orig","le_u50_f","le_u_star_f","le_fall"),
                            "LE (латентный поток)")
  vpd_col  <- pick_first_present(nm, c("VPD","vpd","vpd_f","vpd_orig","VPD_f"))
  tair_col <- pick_first_present(nm, c("TA","ta","tair","tair_f","tair_orig","air_temp","t_air"))

  out <- df %>%
    transmute(
      Year      = as.integer(year),
      datetime  = as.POSIXct(datetime, tz = "UTC"),
      Phase_lab = factor(Phase_lab, levels = PHASE6_RU),
      GPP  = safe_num(.data[[gpp_col]]),          # μmol CO2 m-2 s-1
      LE   = safe_num(.data[[le_col]]),           # W m-2
      VPD  = if (!is.na(vpd_col))  safe_num(.data[[vpd_col]])  else NA_real_,  # kPa
      Tair = if (!is.na(tair_col)) safe_num(.data[[tair_col]]) else NA_real_   # °C
    ) %>%
    # расчёт λ (Джоулей на кг) с поправкой на Tair, если она есть
    mutate(lambda_J_kg = ifelse(is.finite(Tair),
                                2.501e6 - 2.361e3 * Tair,
                                2.45e6),
           # mmol H2O m-2 s-1
           E_mmol = 1000 * LE / (lambda_J_kg * 0.018),
           WUE  = ifelse(is.finite(E_mmol) & E_mmol > 0 & is.finite(GPP),  GPP / E_mmol, NA_real_),
           IWUE = ifelse(is.finite(E_mmol) & E_mmol > 0 & is.finite(GPP) & is.finite(VPD),
                         GPP * VPD / E_mmol, NA_real_)) %>%
    # фильтр очевидных выбросов/некорректных значений
    filter(!is.na(Phase_lab),
           is.finite(WUE),  WUE  >= 0,  WUE  <= 40,
           is.finite(GPP),  GPP  >= 0,  GPP  <= 40,
           is.finite(LE),   LE   >= 5) %>%      # исключаем почти нулевой LE
    select(Year, datetime, Phase_lab, GPP, LE, VPD, Tair, E_mmol, WUE, IWUE)

  out
}

# ----- применяем к трём годам -----
stopifnot(exists("df13"), exists("df16"), exists("df23"))
df13$Phase_lab <- factor(df13$Phase_lab, levels = PHASE6_RU)
df16$Phase_lab <- factor(df16$Phase_lab, levels = PHASE6_RU)
df23$Phase_lab <- factor(df23$Phase_lab, levels = PHASE6_RU)

w13 <- prep_year_wue(df13, 2013)
w16 <- prep_year_wue(df16, 2016)
w23 <- prep_year_wue(df23, 2023)

w_all <- bind_rows(w13, w16, w23)

# ----- сводные таблицы для барплотов -----
summarise_wue <- function(d, value = "WUE"){
  d %>%
    group_by(Year, Phase_lab) %>%
    summarise(
      n  = dplyr::n(),
      mn = mean(.data[[value]], na.rm = TRUE),
      sd = sd(.data[[value]],   na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(Year = factor(Year)) %>%
    arrange(Phase_lab, Year)
}

wue_phase  <- summarise_wue(w_all, "WUE")
iwue_phase <- w_all %>% filter(is.finite(IWUE)) %>% summarise_wue("IWUE")

# общий верхний предел для единой оси Y (по WUE)
ymax_wue <- suppressWarnings(max(wue_phase$mn + 1.96*wue_phase$se, na.rm = TRUE))
if (!is.finite(ymax_wue) || ymax_wue <= 1) ymax_wue <- 10
ybreaks_wue <- pretty(c(0, ymax_wue), 6); ymax_wue <- max(ybreaks_wue)

# ----- график: WUE по фазам (барплот с SE) -----
pos <- position_dodge(width = 0.8)
p_wue_phase <- ggplot(wue_phase, aes(x = Year, y = mn, fill = Year)) +
  geom_col(position = pos, width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, mn - 1.96*se), ymax = mn + 1.96*se),
                position = pos, width = 0.2, linewidth = 0.5) +
  facet_wrap(~Phase_lab, ncol = 3, scales = "fixed") +
  scale_fill_manual(values = pal_year, guide = "none") +
  scale_y_continuous(limits = c(0, ymax_wue), breaks = ybreaks_wue,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "WUE по фенофазам",
       x = "Год", y = "WUE (μmol CO₂ per mmol H₂O)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth=0.2, colour="grey85"),
        strip.background  = element_rect(fill="grey95", colour="grey80"))

print(p_wue_phase)
ggsave("WUE_by_phase_bar.png", p_wue_phase, width = 12, height = 8, dpi = 300, bg = "white")

# ----- (опционально) IWUE, если есть VPD -----
if (nrow(iwue_phase) > 0) {
  ymax_iwue <- suppressWarnings(max(iwue_phase$mn + 1.96*iwue_phase$se, na.rm = TRUE))
  if (!is.finite(ymax_iwue) || ymax_iwue <= 1) ymax_iwue <- 20
  ybreaks_iwue <- pretty(c(0, ymax_iwue), 6); ymax_iwue <- max(ybreaks_iwue)

  p_iwue_phase <- ggplot(iwue_phase, aes(x = Year, y = mn, fill = Year)) +
    geom_col(position = pos, width = 0.7, alpha = 0.9) +
    geom_errorbar(aes(ymin = pmax(0, mn - 1.96*se), ymax = mn + 1.96*se),
                  position = pos, width = 0.2, linewidth = 0.5) +
    facet_wrap(~Phase_lab, ncol = 3, scales = "fixed") +
    scale_fill_manual(values = pal_year, guide = "none") +
    scale_y_continuous(limits = c(0, ymax_iwue), breaks = ybreaks_iwue,
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "IWUE по фенофазам",
         x = "Год", y = "IWUE = GPP×VPD / E (μmol CO₂ kPa per mmol H₂O)") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth=0.2, colour="grey85"),
          strip.background  = element_rect(fill="grey95", colour="grey80"))

  print(p_iwue_phase)
  ggsave("IWUE_by_phase_bar.png", p_iwue_phase, width = 12, height = 8, dpi = 300, bg = "white")
} else {
  message("IWUE не построен: VPD отсутствует хотя бы в одном году/фазе.")
}

# ----- (дополнительно) суммарное сравнение по году без разбиения на фазы -----
wue_year <- w_all %>%
  group_by(Year) %>%
  summarise(n = dplyr::n(),
            mn = mean(WUE, na.rm=TRUE),
            sd = sd(WUE, na.rm=TRUE),
            se = sd/sqrt(n), .groups="drop") %>%
  mutate(Year = factor(Year))

ymax_year <- suppressWarnings(max(wue_year$mn + 1.96*wue_year$se, na.rm=TRUE))
if (!is.finite(ymax_year) || ymax_year <= 1) ymax_year <- ymax_wue
ybreaks_year <- pretty(c(0, ymax_year), 6); ymax_year <- max(ybreaks_year)

p_wue_year <- ggplot(wue_year, aes(Year, mn, fill = Year)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, mn - 1.96*se), ymax = mn + 1.96*se),
                width = 0.2, linewidth = 0.5) +
  scale_fill_manual(values = pal_year, guide = "none") +
  scale_y_continuous(limits = c(0, ymax_year), breaks = ybreaks_year,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "WUE — сравнение по годам (весь сезон)",
       x = "Год", y = "WUE (μmol CO₂ per mmol H₂O)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth=0.2, colour="grey85"))

print(p_wue_year)
