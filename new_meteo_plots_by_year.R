# ======================================================================
# Построение графиков погоды по годам (Москва)
# Москва 2013, Москва 2016, Москва 2023
# ======================================================================
#install.packages("signal")
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(stringr)
  library(patchwork)
  library(signal)  # Фильтр Савицкого-Голея
})

# ----------------------- Константы фаз -----------------------

B2013 <- list(
  Sowing="2013-05-14", Emergence="2013-05-17", Tillering="2013-06-03",
  StemElong="2013-06-27", Heading="2013-07-17", Flowering="2013-07-28",
  Ripening="2013-08-03", Harvesting="2013-08-14"
)
B2016 <- list(
  Sowing="2016-05-11", Emergence="2016-05-15", Tillering="2016-05-22",
  StemElong="2016-06-08", Heading="2016-06-22", Flowering="2016-06-30",
  Ripening="2016-08-12", Harvesting="2016-08-27"
)
B2023 <- list(
  Sowing="2023-05-18", Emergence="2023-05-26", Tillering="2023-06-07",
  StemElong="2023-06-29", Heading="2023-07-14", Flowering="2023-07-20",
  Ripening="2023-08-06", Harvesting="2023-08-31"
)

to_num <- function(x){
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_dt_guess <- function(x, tz = "UTC") {
  xch <- trimws(as.character(x))
  xch[xch == "" | xch == "NA"] <- NA_character_
  out <- suppressWarnings(ymd_hms(xch, tz = tz, quiet = TRUE))
  if (all(is.na(out))) out <- suppressWarnings(ymd_hm(xch, tz = tz, quiet = TRUE))
  if (all(is.na(out))) out <- suppressWarnings(dmy_hms(xch, tz = tz, quiet = TRUE))
  if (all(is.na(out))) out <- suppressWarnings(dmy_hm(xch, tz = tz, quiet = TRUE))
  if (all(is.na(out))) out <- suppressWarnings(ymd(xch, tz = tz, quiet = TRUE))
  if (all(is.na(out))) out <- suppressWarnings(dmy(xch, tz = tz, quiet = TRUE))
  out
}

calc_vpd_kpa <- function(tair_c, rh_pct) {
  rh_use <- rh_pct
  rh_med <- suppressWarnings(median(rh_use, na.rm = TRUE))
  if (is.finite(rh_med) && rh_med <= 1.5) rh_use <- rh_use * 100
  esat <- 0.6108 * exp((17.27 * tair_c) / (tair_c + 237.3))
  esat * (1 - rh_use / 100)
}

sum_or_na <- function(x) {
  if (all(!is.finite(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# фильтр Савицкого-Голея
sg_smooth <- function(x, p = 3, n = 15) {
  # p - порядок полинома, n - размер окна (должен быть нечетным)
  if (length(x) < n) {
    n <- max(3, floor(length(x) / 2) * 2 + 1)  # Уменьшаем окно если мало данных
  }
  if (n %% 2 == 0) n <- n + 1  # Окно должно быть нечетным
  if (n < p + 2) return(x)  # Недостаточно данных

  # Заменяем NA на интерполированные значения для фильтрации
  x_interp <- x
  na_idx <- is.na(x)
  if (all(na_idx)) return(x)
  if (any(na_idx)) {
    # Линейная интерполяция для NA
    x_interp <- approx(seq_along(x)[!na_idx], x[!na_idx],
                       xout = seq_along(x), rule = 2)$y
  }

  # Применяем фильтр
  result <- tryCatch(
    sgolayfilt(x_interp, p = p, n = n),
    error = function(e) x_interp
  )

  # Возвращаем NA на исходные позиции
  result[na_idx] <- NA
  result
}

# ----------------------- Загрузка данных Москва 2013 -----------------------
load_biomet_2013 <- function(path) {
  raw <- read_delim(path, delim = ";", show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  dt <- parse_dt_guess(paste(raw$DATE_1, raw$TIME_1))

  # Извлекаем переменные
  tair <- to_num(raw$AirTC_Avg)
  rh <- to_num(raw$RH)
  svp <- to_num(raw$SVP_kPa)
  vp <- to_num(raw$VP_kPa)
  vpd <- svp - vp
  vpd <- ifelse(vpd < 0, NA_real_, vpd)

  ppfd <- to_num(raw$PAR_Den_Avg)

  # Температура почвы - среднее по датчикам
  tsoil_cols <- grep("TSoil_\\d+_Avg", names(raw), value = TRUE)
  if (length(tsoil_cols) > 0) {
    tsoil_mat <- sapply(tsoil_cols, function(c) to_num(raw[[c]]))
    tsoil <- rowMeans(tsoil_mat, na.rm = TRUE)
    tsoil[!is.finite(tsoil)] <- NA_real_
  } else {
    tsoil <- NA_real_
  }

  # Влажность почвы - среднее по датчикам (m³/m³ -> %)
  swc_cols <- grep("VWC_\\d+_Avg", names(raw), value = TRUE)
  if (length(swc_cols) > 0) {
    swc_mat <- sapply(swc_cols, function(c) to_num(raw[[c]]))
    swc <- rowMeans(swc_mat, na.rm = TRUE) * 100  # Переводим m³/m³ в %
    swc[!is.finite(swc)] <- NA_real_
  } else {
    swc <- NA_real_
  }

  precip <- to_num(raw$Rain_mm_Tot)

  tibble(
    DateTime = dt,
    Date = as.Date(dt),
    Tair = tair,
    RH = rh,
    VPD = vpd,
    PPFD = ppfd,
    Tsoil = tsoil,
    SWC = swc,
    Precip = precip
  ) %>%
    dplyr::filter(!is.na(DateTime))
}

# ----------------------- Загрузка данных 2016 (Москва) -----------------------
load_biomet_2016 <- function(flux_path, precip_path, swc_biomet_path = NULL) {
  # Основные данные из файла с gap-filling
  raw <- read_csv(flux_path, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  dt_col <- if ("DateTime...1" %in% names(raw)) "DateTime...1" else "DateTime...2"
  dt <- parse_dt_guess(raw[[dt_col]])
  tair <- to_num(raw$Tair)
  rh <- to_num(raw$rH)
  vpd <- to_num(raw$VPD)

  vpd_med <- median(vpd, na.rm = TRUE)
  if (!is.na(vpd_med) && vpd_med > 20) vpd <- vpd / 1000

  ppfd <- to_num(raw$PPFD)
  tsoil <- to_num(raw$Tsoil)

  biomet <- tibble(
    DateTime = dt,
    Date = as.Date(dt),
    Tair = tair,
    RH = rh,
    VPD = vpd,
    PPFD = ppfd,
    Tsoil = tsoil,
    SWC = NA_real_,
    Precip = NA_real_
  ) %>%
    dplyr::filter(!is.na(DateTime))

  # Загрузка осадков (3-часовые данные -> дневные суммы)
  if (file.exists(precip_path)) {
    cat("  Загружаем осадки из 2016_precip.csv...\n")
    df_precip <- read.csv(precip_path, sep = ";", dec = ".")
    df_precip$Date_parsed <- dmy_hm(df_precip$Date)
    df_precip$Day <- as.Date(df_precip$Date_parsed)
    precip_data <- df_precip %>%
      group_by(Day) %>%
      summarise(Precip_daily = sum(RRR, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.na(Day)) %>%
      rename(Date = Day)
    cat(sprintf("    Найдено %d дней с осадками\n", sum(precip_data$Precip_daily > 0, na.rm = TRUE)))
    biomet <- biomet %>%
      left_join(precip_data, by = "Date") %>%
      mutate(Precip = coalesce(Precip_daily, Precip)) %>%
      select(-Precip_daily)
  }
  # Загрузка влажности почвы из 2016BiomB.csv
  if (!is.null(swc_biomet_path) && file.exists(swc_biomet_path)) {
    cat("  Загружаем влажность почвы из 2016BiomB.csv...\n")
    first_line <- read_lines(swc_biomet_path, n_max = 1)
    delim <- ifelse(str_detect(first_line, ";"), ";", ",")
    swc_raw <- read_delim(swc_biomet_path, delim = delim, show_col_types = FALSE)
    names(swc_raw) <- trimws(names(swc_raw))
    if ("DATE_1" %in% names(swc_raw) && "TIME_1" %in% names(swc_raw)) {
      swc_dt <- parse_dt_guess(paste(swc_raw$DATE_1, swc_raw$TIME_1))
    } else if ("TIMESTAMP" %in% names(swc_raw)) {
      swc_dt <- parse_dt_guess(swc_raw$TIMESTAMP)
    } else {
      swc_dt <- parse_dt_guess(swc_raw[[1]])
    }
    # Ищем столбцы VWC_число_Avg
    swc_cols <- grep("^VWC_\\d+_Avg$", names(swc_raw), value = TRUE)
    cat(sprintf("    Найдено %d столбцов VWC: %s\n", length(swc_cols), paste(swc_cols, collapse = ", ")))
    if (length(swc_cols) > 0) {
      # Извлекаем данные, заменяем NA/NAN на 0
      swc_mat <- sapply(swc_cols, function(c) {
        vals <- to_num(swc_raw[[c]])
        vals[is.na(vals) | !is.finite(vals)] <- 0
        vals
      })
      swc_mean <- rowMeans(swc_mat, na.rm = TRUE) * 100
      swc_data <- tibble(
        DateTime = swc_dt,
        SWC_ext = swc_mean
      ) %>%
        dplyr::filter(!is.na(DateTime))
      cat(sprintf("    Диапазон SWC: %.1f%% - %.1f%%\n",
                  min(swc_data$SWC_ext, na.rm = TRUE),
                  max(swc_data$SWC_ext, na.rm = TRUE)))
      biomet <- biomet %>%
        left_join(swc_data, by = "DateTime") %>%
        mutate(SWC = coalesce(SWC_ext, SWC)) %>%
        select(-SWC_ext)
    }
  }
  biomet
}

# ----------------------- Загрузка данных 2023 (Москва) - из общего файла -----------------------
load_biomet_2023_gapfilled <- function(path) {
  raw <- read_delim(path, delim = ";", show_col_types = FALSE)
  names(raw) <- trimws(names(raw))
  dt <- parse_dt_guess(raw$timestamp)
  tair <- to_num(raw$Tair_f)
  vpd <- to_num(raw$VPD_f)
  vpd_med <- median(vpd, na.rm = TRUE)
  if (!is.na(vpd_med) && vpd_med > 20) vpd <- vpd / 1000
  ppfd <- to_num(raw$PPFD_f)
  tsoil <- to_num(raw$Tsoil_f)
  # RH нет в файле, но можно вычислить из VPD и Tair
  # esat = 0.6108 * exp((17.27 * T) / (T + 237.3))
  # VPD = esat * (1 - RH/100) => RH = 100 * (1 - VPD/esat)
  esat <- 0.6108 * exp((17.27 * tair) / (tair + 237.3))
  rh <- 100 * (1 - vpd / esat)
  rh <- ifelse(rh < 0 | rh > 100, NA_real_, rh)
  tibble(
    DateTime = dt,
    Date = as.Date(dt),
    Tair = tair,
    RH = rh,
    VPD = vpd,
    PPFD = ppfd,
    Tsoil = tsoil,
    SWC = NA_real_,  # В другом файле
    Precip = NA_real_  # Осадки в отдельном файле
  ) %>%
    dplyr::filter(!is.na(DateTime))
}

# ----------------------- Загрузка данных 2023 из biomet файла -----------------------
load_biomet_2023 <- function(biomet_path, precip_path = NULL) {
  raw <- read_csv(biomet_path, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))
  dt <- parse_dt_guess(raw$DateTime)
  tair <- to_num(raw$TA_1_1_1)
  rh <- to_num(raw$RH_1_1_1)
  vpd <- calc_vpd_kpa(tair, rh)
  ppfd <- to_num(raw$PPFD_IN_1_1_1)
  # Температура почвы - среднее по датчикам
  tsoil_cols <- grep("TS_1_1_\\d+", names(raw), value = TRUE)
  if (length(tsoil_cols) > 0) {
    tsoil_mat <- sapply(tsoil_cols, function(c) to_num(raw[[c]]))
    tsoil <- rowMeans(tsoil_mat, na.rm = TRUE)
    tsoil[!is.finite(tsoil)] <- NA_real_
  } else {
    tsoil <- NA_real_
  }
  # Влажность почвы - среднее по датчикам
  swc_cols <- grep("SWC_1_1_\\d+", names(raw), value = TRUE)
  if (length(swc_cols) > 0) {
    swc_mat <- sapply(swc_cols, function(c) to_num(raw[[c]]))
    swc <- rowMeans(swc_mat, na.rm = TRUE)
    swc[!is.finite(swc)] <- NA_real_
  } else {
    swc <- NA_real_
  }
  precip <- to_num(raw$P_1_1_1)
  tibble(
    DateTime = dt,
    Date = as.Date(dt),
    Tair = tair,
    RH = rh,
    VPD = vpd,
    PPFD = ppfd,
    Tsoil = tsoil,
    SWC = swc,
    Precip = precip
  ) %>%
    dplyr::filter(!is.na(DateTime))
}
# Функция для объединения двух источников данных 2023
merge_biomet_2023 <- function(gapfilled, biomet) {
  if (is.null(gapfilled) || nrow(gapfilled) == 0) return(biomet)
  if (is.null(biomet) || nrow(biomet) == 0) return(gapfilled)
  # Объединяем по времени, приоритет отдаем gap-filled данным
  full_join(gapfilled, biomet, by = "DateTime", suffix = c("", "_bio")) %>%
    mutate(
      Date = coalesce(Date, Date_bio),
      Tair = coalesce(Tair, Tair_bio),
      RH = coalesce(RH, RH_bio),
      VPD = coalesce(VPD, VPD_bio),
      PPFD = coalesce(PPFD, PPFD_bio),
      Tsoil = coalesce(Tsoil, Tsoil_bio),
      SWC = coalesce(SWC, SWC_bio),
      Precip = coalesce(Precip, Precip_bio)
    ) %>%
    select(DateTime, Date, Tair, RH, VPD, PPFD, Tsoil, SWC, Precip) %>%
    arrange(DateTime)
}
# ----------------------- Загрузка осадков из Осадки_2023.csv -----------------------
load_precip_2023 <- function(precip_path, year = 2023) {
  if (!file.exists(precip_path)) return(NULL)
  dat <- read_delim(
    file = precip_path, delim = ";", skip = 1,
    locale = locale(encoding = "CP1251"),
    col_types = cols(.default = col_character()), trim_ws = TRUE
  )
  dat <- dat %>%
    rename(day = 1) %>%
    filter(str_detect(day, "^\\d+$")) %>%
    mutate(day = as.integer(day))
  rename_map <- c("Июн" = "Jun", "Июл" = "Jul", "Авг" = "Aug", "Сен" = "Sep")
  names(dat) <- recode(names(dat), !!!rename_map)
  veg_months <- 6:9
  months_keep <- intersect(month.abb[veg_months], names(dat))
  if (length(months_keep) == 0) return(NULL)
  precip_daily <- dat %>%
    pivot_longer(all_of(months_keep), names_to = "month", values_to = "mm") %>%
    mutate(
      mm = if_else(mm %in% c("-", "", NA_character_), "0", mm),
      mm = as.numeric(str_replace(mm, ",", ".")),
      month_num = match(month, month.abb),
      date = as.Date(sprintf("%d-%02d-%02d", year, month_num, day))
    ) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(Precip_ext = sum(mm, na.rm = TRUE), .groups = "drop")
  precip_daily %>%
    rename(Date = date) %>%
    arrange(Date)
}
# ----------------------- Функция построения графиков -----------------------
create_meteo_plot <- function(biomet, year, bounds, location = "Moscow",
                              sg_order = 2, sg_window = 81, ppfd_thresh = 10,
                              use_data_start = FALSE) {
  if (is.null(biomet) || nrow(biomet) == 0) {
    cat(sprintf("  !! Нет данных биомета для %d\n", year))
    return(NULL)
  }
  season_end <- as.POSIXct(as.Date(bounds$Harvesting) + 1, tz = "UTC")
  # Определяем начало сезона
  if (use_data_start) {
    # Используем начало доступных данных вместо даты посева
    data_start <- min(biomet$DateTime, na.rm = TRUE)
    sowing_date <- as.POSIXct(as.Date(bounds$Sowing), tz = "UTC")
    season_start <- max(data_start, sowing_date)
    cat(sprintf("  Начало данных: %s, начало сезона: %s\n",
                format(data_start, "%Y-%m-%d"), format(season_start, "%Y-%m-%d")))
  } else {
    season_start <- as.POSIXct(as.Date(bounds$Sowing), tz = "UTC")
  }
  # Фильтруем сезон и применяем фильтр
  biomet_season <- biomet %>%
    mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
    dplyr::filter(DateTime >= season_start & DateTime <= season_end) %>%
    arrange(DateTime) %>%
    mutate(
      PPFD_day = ifelse(is.finite(PPFD) & PPFD > ppfd_thresh, PPFD, NA_real_),
      Tair_sg = sg_smooth(Tair, p = sg_order, n = sg_window),
      RH_sg = sg_smooth(RH, p = sg_order, n = sg_window),
      VPD_sg = sg_smooth(VPD, p = sg_order, n = sg_window),
      PPFD_sg = sg_smooth(PPFD_day, p = sg_order, n = sg_window),
      Tsoil_sg = sg_smooth(Tsoil, p = sg_order, n = sg_window),
      SWC_sg = sg_smooth(SWC, p = sg_order, n = sg_window)
    )
  if (nrow(biomet_season) == 0) {
    cat(sprintf("  !! Нет данных в сезоне для %d\n", year))
    return(NULL)
  }
  # Ежедневные осадки
  precip_daily <- biomet_season %>%
    dplyr::filter(is.finite(Precip)) %>%
    mutate(DateOnly = as.Date(DateTime)) %>%
    group_by(DateOnly) %>%
    summarise(Precip_day = sum_or_na(Precip), .groups = "drop") %>%
    mutate(DateTime = as.POSIXct(DateOnly, tz = "UTC"))
  # Параметры графиков
  time_scale <- scale_x_datetime(
    date_breaks = "2 weeks",
    date_labels = "%d.%m",
    limits = c(season_start, season_end)
  )
  theme_meteo <- theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  plots_list <- list()
  have_precip <- nrow(precip_daily) > 0 && any(is.finite(precip_daily$Precip_day))
  have_rh <- sum(is.finite(biomet_season$RH)) > 10
  precip_plot <- NULL
  if (have_precip) {
    precip_plot <- ggplot(precip_daily, aes(x = DateTime, y = Precip_day)) +
      geom_col(fill = "#1f77b4", color = "#1f77b4", alpha = 0.7,
               width = 24 * 60 * 60 * 0.9) +
      labs(title = sprintf("Daily precipitation totals %d", year),
           x = "Date", y = "Precipitation (mm)") +
      time_scale +
      theme_meteo
    ggsave(sprintf("Meteo_precip_%d_%s_en.png", year, location),
           precip_plot, width = 14, height = 6, dpi = 300, bg = "white")
    cat(sprintf("  -> Saved: Meteo_precip_%d_%s_en.png\n", year, location))
  }
  # 1. Air temperature
  if (sum(is.finite(biomet_season$Tair)) > 10) {
    plots_list$Tair <- ggplot(biomet_season %>% dplyr::filter(is.finite(Tair)),
                              aes(x = DateTime)) +
      geom_point(aes(y = Tair), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tair_sg), color = "#d62728", linewidth = 0.8, na.rm = TRUE) +
      labs(title = sprintf("Air temperature dynamics during growing season %d", year),
           x = "", y = "Air temperature (C)") +
      time_scale +
      theme_meteo
  }
  # 2. VPD
  if (sum(is.finite(biomet_season$VPD)) > 10) {
    plots_list$VPD <- ggplot(biomet_season %>% dplyr::filter(is.finite(VPD)),
                             aes(x = DateTime)) +
      geom_point(aes(y = VPD), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = VPD_sg), color = "#ff7f0e", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = "VPD (kPa)") +
      time_scale +
      theme_meteo
  }
  # 3. PPFD
  if (sum(is.finite(biomet_season$PPFD_day)) > 10) {
    plots_list$PPFD <- ggplot(biomet_season %>% dplyr::filter(is.finite(PPFD_day)),
                              aes(x = DateTime)) +
      geom_point(aes(y = PPFD_day), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = PPFD_sg), color = "#9467bd", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = "PPFD (umol m-2 s-1)") +
      time_scale +
      theme_meteo
  }
  # 4. Soil temperature
  if (sum(is.finite(biomet_season$Tsoil)) > 10) {
    plots_list$Tsoil <- ggplot(biomet_season %>% dplyr::filter(is.finite(Tsoil)),
                               aes(x = DateTime)) +
      geom_point(aes(y = Tsoil), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tsoil_sg), color = "#8c564b", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "Date", y = "Soil temperature (C)") +
      time_scale +
      theme_meteo
  }
  # 5. Soil water content
  if (sum(is.finite(biomet_season$SWC)) > 10) {
    plots_list$SWC <- ggplot(biomet_season %>% dplyr::filter(is.finite(SWC)),
                             aes(x = DateTime)) +
      geom_point(aes(y = SWC), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = SWC_sg), color = "#17becf", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "Date", y = "Soil water content (%)") +
      time_scale +
      theme_meteo
  }
  # 6. Precipitation + Relative humidity
  if (have_precip && have_rh) {
    rh_max <- max(biomet_season$RH, na.rm = TRUE)
    precip_max <- max(precip_daily$Precip_day, na.rm = TRUE)
    rh_sf <- if (is.finite(rh_max) && rh_max > 0 && is.finite(precip_max) && precip_max > 0) {
      precip_max / rh_max
    } else {
      1
    }
    plots_list$RH_Precip <- ggplot() +
      geom_col(data = precip_daily, aes(x = DateTime, y = Precip_day),
               fill = "#1f77b4", color = "#1f77b4", alpha = 0.6,
               width = 24 * 60 * 60 * 0.9) +
      geom_point(data = biomet_season %>% dplyr::filter(is.finite(RH)),
                 aes(x = DateTime, y = RH * rh_sf),
                 color = "grey40", size = 0.35, alpha = 0.35) +
      geom_line(data = biomet_season,
                aes(x = DateTime, y = RH_sg * rh_sf),
                color = "grey40", linewidth = 0.8, na.rm = TRUE) +
      scale_y_continuous(name = "Precipitation (mm)",
                         sec.axis = sec_axis(~ . / rh_sf, name = "Relative humidity (%)")) +
      labs(x = "") +
      time_scale +
      theme_meteo
  } else if (have_precip) {
    plots_list$Precipitation <- ggplot(precip_daily, aes(x = DateTime)) +
      geom_col(aes(y = Precip_day), fill = "#1f77b4", color = "#1f77b4", alpha = 0.6,
               width = 24 * 60 * 60 * 0.9) +
      labs(x = "", y = "Precipitation (mm)") +
      time_scale +
      theme_meteo
  } else if (have_rh) {
    plots_list$RH <- ggplot(biomet_season %>% dplyr::filter(is.finite(RH)),
                            aes(x = DateTime)) +
      geom_point(aes(y = RH), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = RH_sg), color = "#2ca02c", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = "Relative humidity (%)") +
      time_scale +
      theme_meteo
  }

  if (length(plots_list) == 0) {
    cat(sprintf("  !! No valid meteorological data for %d\n", year))
    return(NULL)
  }
  # Arrange panels in requested order.
  rh_panel <- if ("RH_Precip" %in% names(plots_list)) {
    plots_list$RH_Precip
  } else if ("Precipitation" %in% names(plots_list)) {
    plots_list$Precipitation
  } else {
    plots_list$RH
  }

  ordered_plots <- list(
    plots_list$Tair, plots_list$PPFD,
    plots_list$VPD, rh_panel,
    plots_list$Tsoil, plots_list$SWC
  )
  ordered_plots <- ordered_plots[!vapply(ordered_plots, is.null, logical(1))]

  wrap_plots(ordered_plots, ncol = 2)
}
# ----------------------- Run plotting -----------------------
cat("\n=== Meteo plots by year (English labels) ===\n\n")

biomet_2013_path <- "biomet2013.csv"
biomet_2016_path <- "Moscow_2016_verFin.csv"
precip_2016_path <- "2016_precip.csv"
swc_2016_path <- "2016BiomB.csv"
gapfilled_2023_path <- "ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv"
biomet_2023_path <- "Anal11_biomet.csv"
precip_2023_path <- "Осадки_2023.csv"

cat("Loading 2013 biomet...\n")
biomet_2013 <- load_biomet_2013(biomet_2013_path)
cat(sprintf("  Loaded %d rows: %s - %s\n",
            nrow(biomet_2013),
            min(biomet_2013$Date, na.rm = TRUE),
            max(biomet_2013$Date, na.rm = TRUE)))

cat("\nLoading 2016 biomet...\n")
biomet_2016 <- load_biomet_2016(biomet_2016_path, precip_2016_path, swc_2016_path)
cat(sprintf("  Loaded %d rows: %s - %s\n",
            nrow(biomet_2016),
            min(biomet_2016$Date, na.rm = TRUE),
            max(biomet_2016$Date, na.rm = TRUE)))

cat("\nLoading 2023 biomet...\n")
biomet_2023_gf <- NULL
if (file.exists(gapfilled_2023_path)) {
  cat("  Loading gap-filled data...\n")
  biomet_2023_gf <- load_biomet_2023_gapfilled(gapfilled_2023_path)
}
biomet_2023_bio <- NULL
if (file.exists(biomet_2023_path)) {
  cat("  Loading biomet data...\n")
  biomet_2023_bio <- load_biomet_2023(biomet_2023_path, precip_2023_path)
}
biomet_2023 <- merge_biomet_2023(biomet_2023_gf, biomet_2023_bio)
cat(sprintf("  Combined %d rows: %s - %s\n",
            nrow(biomet_2023),
            min(biomet_2023$Date, na.rm = TRUE),
            max(biomet_2023$Date, na.rm = TRUE)))

if (file.exists(precip_2023_path)) {
  cat("  Loading external precipitation...\n")
  precip_2023 <- load_precip_2023(precip_2023_path, year = 2023)
  if (!is.null(precip_2023) && nrow(precip_2023) > 0) {
    biomet_2023 <- biomet_2023 %>%
      left_join(precip_2023, by = "Date") %>%
      mutate(Precip = coalesce(Precip_ext, Precip)) %>%
      select(-Precip_ext)
  }
}

cat("\n2013...\n")
p2013 <- create_meteo_plot(biomet_2013, 2013, B2013, location = "Moscow")
if (!is.null(p2013)) {
  ggsave("Meteo_dynamics_2013_Moscow_en.png", p2013,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Saved: Meteo_dynamics_2013_Moscow_en.png\n")
}

cat("\n2016...\n")
p2016 <- create_meteo_plot(biomet_2016, 2016, B2016, location = "Moscow")
if (!is.null(p2016)) {
  ggsave("Meteo_dynamics_2016_Moscow_en.png", p2016,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Saved: Meteo_dynamics_2016_Moscow_en.png\n")
}

cat("\n2023...\n")
p2023 <- create_meteo_plot(biomet_2023, 2023, B2023, location = "Moscow", use_data_start = TRUE)
if (!is.null(p2023)) {
  ggsave("Meteo_dynamics_2023_Moscow_en.png", p2023,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Saved: Meteo_dynamics_2023_Moscow_en.png\n")
}