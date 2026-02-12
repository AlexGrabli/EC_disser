# --- 1. УСТАНОВКА И ЗАГРУЗКА ПАКЕТОВ ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, patchwork, scales, dplyr, tidyr, readr, stringr, ggplot2, slider, signal)

# --- 2. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ (УПРОЩЕННЫЙ МЕТОД) ---
Sys.setlocale("LC_TIME", "C")

path_main_data   <- "ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv"
path_biomet_extra<- "Anal11_biomet.csv"
path_precip      <- "Осадки.csv"
veg_months <- 6:9
roll_window_days <- 7
sg_poly_order <- 3
sg_default_window <- 15
ppfd_start_thresh <- 1500
end_month_day <- "09-01"

## main_data: базовые переменные
main_data <- read_csv(path_main_data, col_types = cols(timestamp = "c")) %>%
  select(
    DateTime = timestamp,
    TA       = Tair_f,
    TS_single= Tsoil_f,
    PPFD     = PPFD_f,
    VPD      = VPD_f
  ) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd HM"))) %>%
  dplyr::filter(!is.na(DateTime))

# --- NEW: заголовки файла биомета и чтение с RH + SWC 6 датчиков ---
biomet_names <- names(read_csv(path_biomet_extra, n_max = 0, na = "--"))
extra_data <- read_csv(path_biomet_extra, skip = 2, col_names = biomet_names, na = "--") %>%
  select(
    DateTime,
    RH = RH_1_1_1,
    SWC_1_1_1, SWC_1_1_2, SWC_1_1_3, SWC_1_1_4, SWC_1_1_5, SWC_1_1_6
  ) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd HM"))) %>%
  dplyr::filter(!is.na(DateTime))

# объединение по времени
meteo_full <- left_join(main_data, extra_data, by = "DateTime")

# --- ПАРАМЕТРЫ ФИЛЬТРА ДНЕВНОГО PPFD ---
ppfd_thresh <- 350      # мкмоль м^-2 с^-1; всё ниже считаем ночью. Можно 0 или 50.
use_hour_window <- FALSE
day_hours <- c(5, 21)  # если use_hour_window=TRUE, берём только часы 05-21

# --- SWC среднее по 6 датчикам на получасовом шаге ---
swc_cols <- intersect(
  c("SWC_1_1_1", "SWC_1_1_2", "SWC_1_1_3", "SWC_1_1_4", "SWC_1_1_5", "SWC_1_1_6"),
  names(meteo_full)
)

meteo_full <- meteo_full %>%
  mutate(
    SWC_mean_hh = if (length(swc_cols) > 0)
      rowMeans(across(all_of(swc_cols)), na.rm = TRUE) else NA_real_
  )

# --- ДАННЫЕ ВЕГЕТАЦИОННОГО ПЕРИОДА БЕЗ УСРЕДНЕНИЙ ---
meteo_veg <- meteo_full %>%
  dplyr::filter(month(DateTime) %in% veg_months) %>%
  mutate(
    VPD_kPa = VPD / 10,                                   # ваши VPD в hPa -> kPa
    PPFD_day = if (use_hour_window) {
      if_else(between(hour(DateTime), day_hours[1], day_hours[2]), PPFD, NA_real_)
    } else {
      if_else(PPFD > ppfd_thresh, PPFD, NA_real_)         # дневной фильтр по PPFD
    }
  ) %>%
  arrange(DateTime)

if (nrow(meteo_veg) == 0) stop("Нет данных за вегетационный период.")

sg_window_n <- function(idx, days_window, default_n = 15) {
  if (length(idx) < 2) return(default_n)
  step_mins <- as.numeric(median(diff(idx), na.rm = TRUE), units = "mins")
  if (!is.finite(step_mins) || step_mins <= 0) return(default_n)
  n_raw <- round((days_window * 24 * 60) / step_mins)
  n_raw <- max(3, n_raw)
  if (n_raw %% 2 == 0) n_raw <- n_raw + 1
  n_raw
}

sg_smooth <- function(x, p = 3, n = 15) {
  if (length(x) < n) {
    n <- max(3, floor(length(x) / 2) * 2 + 1)
  }
  if (n %% 2 == 0) n <- n + 1
  if (n < p + 2) return(x)

  x_interp <- x
  na_idx <- is.na(x)
  if (all(na_idx)) return(x)
  if (any(na_idx)) {
    x_interp <- approx(seq_along(x)[!na_idx], x[!na_idx],
                       xout = seq_along(x), rule = 2)$y
  }

  result <- tryCatch(
    sgolayfilt(x_interp, p = p, n = n),
    error = function(e) x_interp
  )

  result[na_idx] <- NA
  result
}

sg_window_n_days <- sg_window_n(meteo_veg$DateTime, roll_window_days, default_n = sg_default_window)

meteo_roll <- meteo_veg %>%
  mutate(
    ta_roll   = sg_smooth(TA,          p = sg_poly_order, n = sg_window_n_days),
    ts_roll   = sg_smooth(TS_single,   p = sg_poly_order, n = sg_window_n_days),
    vpd_roll  = sg_smooth(VPD_kPa,     p = sg_poly_order, n = sg_window_n_days),
    rh_roll   = sg_smooth(RH,          p = sg_poly_order, n = sg_window_n_days),
    ppfd_roll = sg_smooth(PPFD_day,    p = sg_poly_order, n = sg_window_n_days),
    swc_roll  = sg_smooth(SWC_mean_hh, p = sg_poly_order, n = sg_window_n_days)
  )


# --- НОВЫЙ РАЗДЕЛ: Загрузка и обработка осадков ---
dat <- read_delim(
  file = path_precip, delim = ";", skip = 1,
  locale = locale(encoding = "CP1251"),
  col_types = cols(.default = col_character()), trim_ws = TRUE
)

dat <- dat %>%
  rename(day = 1) %>%
  dplyr::filter(str_detect(day, "^\\d+$")) %>%
  mutate(day = as.integer(day))

name_clean <- names(dat)
name_clean <- str_replace(name_clean, "^\ufeff", "")
name_clean <- str_trim(name_clean)
rename_map <- c(
  "Апр" = "Apr", "Апр." = "Apr", "Апрель" = "Apr",
  "Май" = "May", "Май." = "May",
  "Июн" = "Jun", "Июн." = "Jun", "Июнь" = "Jun",
  "Июл" = "Jul", "Июл." = "Jul", "Июль" = "Jul",
  "Авг" = "Aug", "Авг." = "Aug", "Август" = "Aug",
  "Сен" = "Sep", "Сен." = "Sep", "Сент" = "Sep", "Сентябрь" = "Sep",
  "Июня" = "Jun", "Июля" = "Jul", "Августа" = "Aug", "Сентября" = "Sep"
)
names(dat) <- recode(name_clean, !!!rename_map)
month_cols <- setdiff(names(dat), "day")
months_keep <- intersect(month.abb, month_cols)
if (length(months_keep) == 0) {
  months_keep <- month_cols
}

precip_year <- year(min(meteo_veg$DateTime, na.rm = TRUE))

precip_daily <- dat %>%
  pivot_longer(all_of(months_keep), names_to = "month", values_to = "mm") %>%
  mutate(
    mm = if_else(mm %in% c("-", "", NA_character_), "0", mm),
    mm = as.numeric(str_replace(mm, ",", ".")),
    month_num = match(month, month.abb),
    date = make_date(year = precip_year, month = month_num, day = day)
  ) %>%
  dplyr::filter(!is.na(date)) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(P_mm = sum(mm, na.rm = TRUE), .groups = "drop")  # на случай дублей

# --- ЕДИНАЯ ОСЬ X (СТАРТ ПО ФАР, КОНЕЦ 31.08 ИЛИ 01.09) ---
ppfd_start_time <- meteo_veg %>%
  dplyr::filter(PPFD_day >= ppfd_start_thresh) %>%
  summarise(start_time = min(DateTime, na.rm = TRUE)) %>%
  pull(start_time)

if (!is.finite(as.numeric(ppfd_start_time))) {
  ppfd_start_time <- min(meteo_veg$DateTime, na.rm = TRUE)
}

start_date <- as.Date(ppfd_start_time)
end_date <- as.Date(sprintf("%d-%s", precip_year, end_month_day))

start_datetime <- as.POSIXct(start_date)
end_datetime <- as.POSIXct(end_date + days(1))

meteo_plot <- meteo_roll %>%
  dplyr::filter(DateTime >= start_datetime, DateTime <= end_datetime)

precip_plot_data <- precip_daily %>%
  mutate(DateTime = as.POSIXct(date)) %>%
  dplyr::filter(DateTime >= start_datetime, DateTime <= end_datetime)

# --- 3. СОЗДАНИЕ ГРАФИКОВ ---
caption_text_ru <- sprintf(
  "Точки - исходные данные, линия - фильтр Савицкого-Голея (окно ~%d дней).",
  roll_window_days
)
caption_text_en <- sprintf(
  "Points are raw data, line is Savitzky-Golay filter (window ~%d days).",
  roll_window_days
)
time_scale <- scale_x_datetime(
  date_breaks = "2 weeks",
  date_labels = "%d.%m",
  limits = c(start_datetime, end_datetime)
)

p_axis_x_blank <- theme(axis.title.x = element_blank())

make_meteo_plots <- function(lang = c("ru", "en")) {
  lang <- match.arg(lang)
  labels <- if (lang == "ru") {
    list(
      x = "Дата",
      ppfd = expression(paste("ФАР, ", mu, "моль м"^{-2}, " с"^{-1})),
      ta = "Температура воздуха, °C",
      ts = "Температура почвы, °C",
      vpd = "Дефицит давления пара (VPD), кПа",
      swc = "Влажность почвы, %",
      rh = "Относительная влажность",
      rh_axis = "Относительная влажность, %",
      precip = "Осадки",
      precip_axis = "Осадки, мм/день",
      air_legend = "Температура воздуха",
      soil_legend = "Температура почвы"
    )
  } else {
    list(
      x = "Date",
      ppfd = expression(paste("PPFD, ", mu, "mol m"^{-2}, " s"^{-1})),
      ta = "Air temperature, °C",
      ts = "Soil temperature, °C",
      vpd = "Vapor pressure deficit (VPD), kPa",
      swc = "Soil water content, %",
      rh = "Relative humidity",
      rh_axis = "Relative humidity, %",
      precip = "Precipitation",
      precip_axis = "Precipitation, mm/day",
      air_legend = "Air temperature",
      soil_legend = "Soil temperature"
    )
  }

  p_ppfd <- ggplot(dplyr::filter(meteo_plot, !is.na(PPFD_day)), aes(x = DateTime)) +
    geom_point(aes(y = PPFD_day), color = "grey50", size = 0.35, alpha = 0.35) +
    geom_line(aes(y = ppfd_roll), color = "darkgreen", linewidth = 0.7, na.rm = TRUE) +
    labs(y = labels$ppfd, x = labels$x) +
    time_scale +
    theme_bw()

  p_ta <- ggplot(meteo_plot, aes(x = DateTime)) +
    geom_point(aes(y = TA, color = labels$air_legend), size = 0.35, alpha = 0.35) +
    geom_line(aes(y = ta_roll, color = labels$air_legend), linewidth = 0.7, na.rm = TRUE) +
    scale_color_manual(
      name = "",
      values = c(labels$air_legend = "#D55E00")
    ) +
    labs(y = labels$ta, x = labels$x) +
    time_scale +
    theme_bw() +
    theme(legend.position = "top", legend.background = element_blank())

  p_ts <- ggplot(meteo_plot, aes(x = DateTime)) +
    geom_point(aes(y = TS_single, color = labels$soil_legend), size = 0.35, alpha = 0.35) +
    geom_line(aes(y = ts_roll, color = labels$soil_legend), linewidth = 0.7, na.rm = TRUE) +
    scale_color_manual(
      name = "",
      values = c(labels$soil_legend = "#000000")
    ) +
    labs(y = labels$ts, x = labels$x) +
    time_scale +
    theme_bw() +
    theme(legend.position = "top", legend.background = element_blank())

  p_vpd <- ggplot(meteo_plot, aes(x = DateTime)) +
    geom_point(aes(y = VPD_kPa), color = "grey50", size = 0.35, alpha = 0.35) +
    geom_line(aes(y = vpd_roll), color = "#0072B2", linewidth = 0.7, na.rm = TRUE) +
    labs(y = labels$vpd, x = labels$x) +
    time_scale +
    theme_bw()

  p_swc <- ggplot(meteo_plot, aes(x = DateTime)) +
    geom_point(aes(y = SWC_mean_hh), color = "grey50", size = 0.35, alpha = 0.35, na.rm = TRUE) +
    geom_line(aes(y = swc_roll), color = "#009E73", linewidth = 0.7, na.rm = TRUE) +
    labs(y = labels$swc, x = labels$x) +
    time_scale +
    theme_bw()

  p_rh_precip <- ggplot() +
    geom_col(
      data = precip_plot_data,
      aes(x = DateTime, y = P_mm, fill = labels$precip),
      width = 24 * 60 * 60 * 0.9,
      alpha = 0.85
    ) +
    geom_point(
      data = meteo_plot,
      aes(x = DateTime, y = RH * rh_sf, color = labels$rh),
      size = 0.35,
      alpha = 0.35
    ) +
    geom_line(
      data = meteo_plot,
      aes(x = DateTime, y = rh_roll * rh_sf, color = labels$rh),
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    scale_y_continuous(
      name = labels$precip_axis,
      sec.axis = sec_axis(~ . / rh_sf, name = labels$rh_axis)
    ) +
    scale_fill_manual(NULL, values = c(labels$precip = "#92C5DE")) +
    scale_color_manual(NULL, values = c(labels$rh = "grey30")) +
    labs(x = labels$x) +
    time_scale +
    theme_bw() +
    theme(legend.position = "bottom", legend.background = element_blank())

  list(
    ppfd = p_ppfd,
    ta = p_ta,
    ts = p_ts,
    vpd = p_vpd,
    swc = p_swc,
    rh_precip = p_rh_precip
  )
}

rh_max <- max(meteo_plot$RH, na.rm = TRUE)
precip_max <- max(precip_plot_data$P_mm, na.rm = TRUE)
rh_sf <- if (is.finite(rh_max) && rh_max > 0 && is.finite(precip_max) && precip_max > 0) {
  precip_max / rh_max
} else {
  1
}

plots_ru <- make_meteo_plots("ru")
plots_en <- make_meteo_plots("en")

final_plot_ru <- (plots_ru$ta + p_axis_x_blank) | (plots_ru$ppfd + p_axis_x_blank)
final_plot_ru <- final_plot_ru /
  ((plots_ru$vpd + p_axis_x_blank) | (plots_ru$rh_precip + p_axis_x_blank)) /
  (plots_ru$ts | plots_ru$swc) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(caption = caption_text_ru)

final_plot_en <- (plots_en$ta + p_axis_x_blank) | (plots_en$ppfd + p_axis_x_blank)
final_plot_en <- final_plot_en /
  ((plots_en$vpd + p_axis_x_blank) | (plots_en$rh_precip + p_axis_x_blank)) /
  (plots_en$ts | plots_en$swc) +
  plot_layout(heights = c(1, 1, 1)) +
  plot_annotation(caption = caption_text_en)

print(final_plot_ru)
print(final_plot_en)

ggsave(
  "Meteo_veg_raw_points_sg_ru.png",
  plot = final_plot_ru,
  width = 12,
  height = 12,
  dpi = 300
)

ggsave(
  "Meteo_veg_raw_points_sg_en.png",
  plot = final_plot_en,
  width = 12,
  height = 12,
  dpi = 300
)
