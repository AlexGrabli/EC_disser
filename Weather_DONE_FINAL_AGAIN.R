# --- 1. УСТАНОВКА И ЗАГРУЗКА ПАКЕТОВ ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, patchwork, scales, dplyr, tidyr, readr, stringr, ggplot2, slider)

# --- 2. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ (УПРОЩЕННЫЙ МЕТОД) ---
Sys.setlocale("LC_TIME", "C")

path_main_data   <- "ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv"
path_biomet_extra<- "Anal11_biomet.csv"
path_precip      <- "Осадки.csv"
veg_months <- 6:9
roll_window_days <- 7
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
  filter(!is.na(DateTime))

# --- NEW: заголовки файла биомета и чтение с RH + SWC 6 датчиков ---
biomet_names <- names(read_csv(path_biomet_extra, n_max = 0, na = "--"))
extra_data <- read_csv(path_biomet_extra, skip = 2, col_names = biomet_names, na = "--") %>%
  select(
    DateTime,
    RH = RH_1_1_1,
    SWC_1_1_1, SWC_1_1_2, SWC_1_1_3, SWC_1_1_4, SWC_1_1_5, SWC_1_1_6
  ) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd HM"))) %>%
  filter(!is.na(DateTime))

# объединение по времени
meteo_full <- left_join(main_data, extra_data, by = "DateTime")

# --- ПАРАМЕТРЫ ФИЛЬТРА ДНЕВНОГО PPFD ---
ppfd_thresh <- 350      # мкмоль м^-2 с^-1; всё ниже считаем ночью. Можно 0 или 50.
use_hour_window <- FALSE
day_hours <- c(5, 21)  # если use_hour_window=TRUE, берём только часы 05-21

# --- SWC среднее по 6 датчикам на получасовом шаге ---
swc_cols <- c("SWC_1_1_1","SWC_1_1_2","SWC_1_1_3","SWC_1_1_4","SWC_1_1_5","SWC_1_1_6")
swc_cols <- intersect(swc_cols, names(meteo_full))

meteo_full <- meteo_full %>%
  mutate(
    SWC_mean_hh = if (length(swc_cols) > 0)
      rowMeans(across(all_of(swc_cols)), na.rm = TRUE) else NA_real_
  )

# --- ДАННЫЕ ВЕГЕТАЦИОННОГО ПЕРИОДА БЕЗ УСРЕДНЕНИЙ ---
meteo_veg <- meteo_full %>%
  filter(month(DateTime) %in% veg_months) %>%
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

roll_mean_index <- function(x, idx, days_window) {
  slide_index_dbl(
    x,
    idx,
    ~mean(.x, na.rm = TRUE),
    .before = days(days_window - 1),
    .complete = FALSE
  )
}

meteo_roll <- meteo_veg %>%
  mutate(
    ta_roll   = roll_mean_index(TA,          DateTime, roll_window_days),
    ts_roll   = roll_mean_index(TS_single,   DateTime, roll_window_days),
    vpd_roll  = roll_mean_index(VPD_kPa,     DateTime, roll_window_days),
    rh_roll   = roll_mean_index(RH,          DateTime, roll_window_days),
    ppfd_roll = roll_mean_index(PPFD_day,    DateTime, roll_window_days),
    swc_roll  = roll_mean_index(SWC_mean_hh, DateTime, roll_window_days)
  )


# --- НОВЫЙ РАЗДЕЛ: Загрузка и обработка осадков ---
dat <- read_delim(
  file = path_precip, delim = ";", skip = 1,
  locale = locale(encoding = "CP1251"),
  col_types = cols(.default = col_character()), trim_ws = TRUE
)

dat <- dat %>%
  rename(day = 1) %>%
  filter(str_detect(day, "^\\d+$")) %>%
  mutate(day = as.integer(day))

rename_map <- c("Июн" = "Jun", "Июл" = "Jul", "Авг" = "Aug", "Сен" = "Sep")
names(dat) <- recode(names(dat), !!!rename_map)
months_keep <- intersect(month.abb[veg_months], names(dat))
if (length(months_keep) == 0) {
  stop("Нет столбцов с месяцами вегетационного периода в файле осадков.")
}

precip_year <- year(min(meteo_veg$DateTime, na.rm = TRUE))

precip_daily <- dat %>%
  pivot_longer(all_of(months_keep), names_to = "month", values_to = "mm") %>%
  mutate(
    mm = if_else(mm %in% c("-", "", NA_character_), "0", mm),
    mm = as.numeric(str_replace(mm, ",", ".")),
    month_num = match(month, month.abb),
    date = as.Date(sprintf("%d-%02d-%02d", precip_year, month_num, day))
  ) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(P_mm = sum(mm, na.rm = TRUE), .groups = "drop")  # на случай дублей

# --- ЕДИНАЯ ОСЬ X (СТАРТ ПО ФАР, КОНЕЦ 31.08 ИЛИ 01.09) ---
ppfd_start_time <- meteo_veg %>%
  filter(PPFD_day >= ppfd_start_thresh) %>%
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
  filter(DateTime >= start_datetime, DateTime <= end_datetime)

precip_plot_data <- precip_daily %>%
  mutate(DateTime = as.POSIXct(date)) %>%
  filter(DateTime >= start_datetime, DateTime <= end_datetime)

# --- 3. СОЗДАНИЕ ГРАФИКОВ ---
caption_text <- sprintf(
  "Точки - исходные данные, линия - скользящее среднее (%d дней).",
  roll_window_days
)
time_scale <- scale_x_datetime(
  date_breaks = "2 weeks",
  date_labels = "%d.%m",
  limits = c(start_datetime, end_datetime)
)

p_ppfd <- ggplot(filter(meteo_plot, !is.na(PPFD_day)), aes(x = DateTime)) +
  geom_point(aes(y = PPFD_day), color = "grey50", size = 0.35, alpha = 0.35) +
  geom_line(aes(y = ppfd_roll), color = "darkgreen", linewidth = 0.7, na.rm = TRUE) +
  labs(
    y = expression(paste("ФАР, ", mu, "моль м"^{-2}, " с"^{-1})),
    x = "Дата"
  ) +
  time_scale +
  theme_bw()

p_ta <- ggplot(meteo_plot, aes(x = DateTime)) +
  geom_point(aes(y = TA, color = "Температура воздуха"), size = 0.35, alpha = 0.35) +
  geom_line(aes(y = ta_roll, color = "Температура воздуха"), linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(
    name = "",
    values = c("Температура воздуха" = "#D55E00")
  ) +
  labs(y = "Температура воздуха, °C", x = "Дата") +
  time_scale +
  theme_bw() +
  theme(legend.position = "top", legend.background = element_blank())

p_ts <- ggplot(meteo_plot, aes(x = DateTime)) +
  geom_point(aes(y = TS_single, color = "Температура почвы"), size = 0.35, alpha = 0.35) +
  geom_line(aes(y = ts_roll, color = "Температура почвы"), linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(
    name = "",
    values = c("Температура почвы" = "#000000")
  ) +
  labs(y = "Температура почвы, °C", x = "Дата") +
  time_scale +
  theme_bw() +
  theme(legend.position = "top", legend.background = element_blank())

p_vpd <- ggplot(meteo_plot, aes(x = DateTime)) +
  geom_point(aes(y = VPD_kPa), color = "grey50", size = 0.35, alpha = 0.35) +
  geom_line(aes(y = vpd_roll), color = "#0072B2", linewidth = 0.7, na.rm = TRUE) +
  labs(y = "Дефицит давления пара (VPD), кПа", x = "Дата") +
  time_scale +
  theme_bw()

p_swc <- ggplot(meteo_plot, aes(x = DateTime)) +
  geom_point(aes(y = SWC_mean_hh), color = "grey50", size = 0.35, alpha = 0.35, na.rm = TRUE) +
  geom_line(aes(y = swc_roll), color = "#009E73", linewidth = 0.7, na.rm = TRUE) +
  labs(y = "Влажность почвы, %", x = "Дата") +
  time_scale +
  theme_bw()

rh_max <- max(meteo_plot$RH, na.rm = TRUE)
precip_max <- max(precip_plot_data$P_mm, na.rm = TRUE)
rh_sf <- if (is.finite(rh_max) && rh_max > 0 && is.finite(precip_max) && precip_max > 0) {
  precip_max / rh_max
} else {
  1
}

p_rh_precip <- ggplot() +
  geom_col(
    data = precip_plot_data,
    aes(x = DateTime, y = P_mm, fill = "Осадки"),
    width = 24 * 60 * 60 * 0.9,
    alpha = 0.85
  ) +
  geom_point(
    data = meteo_plot,
    aes(x = DateTime, y = RH * rh_sf, color = "Относительная влажность"),
    size = 0.35,
    alpha = 0.35
  ) +
  geom_line(
    data = meteo_plot,
    aes(x = DateTime, y = rh_roll * rh_sf, color = "Относительная влажность"),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    name = "Осадки, мм/день",
    sec.axis = sec_axis(~ . / rh_sf, name = "Относительная влажность, %")
  ) +
  scale_fill_manual(NULL, values = c("Осадки" = "#92C5DE")) +
  scale_color_manual(NULL, values = c("Относительная влажность" = "grey30")) +
  labs(x = "Дата") +
  time_scale +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank())

final_plot <- p_ppfd / p_ta / p_ts / p_vpd / p_swc / p_rh_precip +
  plot_layout(heights = c(1, 1, 1, 1, 1, 1)) +
  plot_annotation(caption = caption_text)

print(final_plot)

ggsave(
  "Meteo_veg_raw_points_ma.png",
  plot = final_plot,
  width = 12,
  height = 18,
  dpi = 300
)

ggsave("PPFD_veg.png", plot = p_ppfd, width = 12, height = 4, dpi = 300)
ggsave("AirTemp_veg.png", plot = p_ta, width = 12, height = 4, dpi = 300)
ggsave("SoilTemp_veg.png", plot = p_ts, width = 12, height = 4, dpi = 300)
ggsave("VPD_veg.png", plot = p_vpd, width = 12, height = 4, dpi = 300)
ggsave("SWC_veg.png", plot = p_swc, width = 12, height = 4, dpi = 300)
ggsave("RH_Precip_veg.png", plot = p_rh_precip, width = 12, height = 4, dpi = 300)
