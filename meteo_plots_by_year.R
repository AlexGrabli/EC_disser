# ======================================================================
# Построение графиков метеоусловий по годам
# Курск 2013, Москва 2013, Москва 2016, Москва 2023
# Референс: 6-панельный график с температурой воздуха, VPD, PPFD,
#           температурой почвы, влажностью почвы, осадками/влажностью
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(stringr)
  library(patchwork)
})

# ----------------------- Константы фаз -----------------------
# Курск 2013 (яровая пшеница)
B2013_Kursk <- list(
  Sowing="2013-05-14", Emergence="2013-05-17", Tillering="2013-06-03",
  StemElong="2013-06-27", Heading="2013-07-17", Flowering="2013-07-28",
  Ripening="2013-08-03", Harvesting="2013-08-14"
)
# Москва 2013 (используем те же даты, если нужны другие - скажите)
B2013_Moscow <- list(
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

# ----------------------- Утилиты -----------------------
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

roll_mean_index <- function(x, idx, days_window) {
  if (!requireNamespace("slider", quietly = TRUE)) {
    return(rep(NA_real_, length(x)))
  }
  slider::slide_index_dbl(
    x,
    idx,
    ~mean(.x, na.rm = TRUE),
    .before = lubridate::days(days_window - 1),
    .complete = FALSE
  )
}

# ----------------------- Загрузка данных Курск 2013 -----------------------
load_biomet_kursk_2013 <- function(path) {
  # Kursk_data_half_our.csv - CSV формат
  raw <- read_csv(path, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  # Парсим дату
  dt <- parse_dt_guess(raw$DateTime)

  # Извлекаем переменные
  tair <- to_num(raw$Ta_Avg)
  rh <- to_num(raw$RH_Avg)

  # VPD вычисляем из температуры и RH
  vpd <- calc_vpd_kpa(tair, rh)

  ppfd <- to_num(raw$PAR_Den_Avg)

  # Температура почвы
  tsoil <- to_num(raw$Tsoil)

  # Влажность почвы - среднее по глубинам (m³/m³ -> %)
  swc_10 <- to_num(raw$SWC_avg_10cm_Avg)
  swc_20 <- to_num(raw$SWC_avg_20cm_Avg)
  swc_50 <- to_num(raw$SWC_avg_50cm_Avg)
  swc_mat <- cbind(swc_10, swc_20, swc_50)
  swc <- rowMeans(swc_mat, na.rm = TRUE) * 100  # Переводим m³/m³ в %
  swc[!is.finite(swc)] <- NA_real_

  # Осадки
  precip <- to_num(raw$P_Tot)

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
    filter(!is.na(DateTime))
}

# ----------------------- Загрузка данных Москва 2013 -----------------------
load_biomet_moscow_2013 <- function(path) {
  # biomet2013.csv - точка с запятой разделитель
  raw <- read_delim(path, delim = ";", show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  # Парсим дату и время
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

  # Осадки
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
    filter(!is.na(DateTime))
}

# ----------------------- Загрузка данных 2016 (Москва) -----------------------
load_biomet_2016 <- function(flux_path, precip_path, swc_biomet_path = NULL) {
  # Основные данные из файла с gap-filling
  raw <- read_csv(flux_path, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  # Парсим дату
  dt_col <- if ("DateTime...1" %in% names(raw)) "DateTime...1" else "DateTime...2"
  dt <- parse_dt_guess(raw[[dt_col]])

  # Извлекаем переменные
  tair <- to_num(raw$Tair)
  rh <- to_num(raw$rH)
  vpd <- to_num(raw$VPD)

  # VPD может быть в Па вместо кПа
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
    filter(!is.na(DateTime))

  # Загрузка осадков
  if (file.exists(precip_path)) {
    precip_raw <- read_delim(precip_path, delim = ";", show_col_types = FALSE,
                             col_types = cols(.default = col_character()))
    names(precip_raw) <- trimws(names(precip_raw))

    precip_data <- precip_raw %>%
      transmute(
        DateTime = parse_dt_guess(Date),
        Precip_ext = to_num(RRR)
      ) %>%
      filter(!is.na(DateTime)) %>%
      mutate(Precip_ext = ifelse(is.na(Precip_ext), 0, Precip_ext))

    # Присоединяем осадки (они 3-часовые, нужно подогнать к получасовым данным)
    biomet <- biomet %>%
      left_join(precip_data, by = "DateTime") %>%
      mutate(Precip = coalesce(Precip_ext, Precip)) %>%
      select(-Precip_ext)
  }

  # Загрузка влажности почвы из 2016BiomB.csv
  if (!is.null(swc_biomet_path) && file.exists(swc_biomet_path)) {
    cat("  Загружаем влажность почвы из 2016BiomB.csv...\n")

    # Определяем разделитель
    first_line <- read_lines(swc_biomet_path, n_max = 1)
    delim <- ifelse(str_detect(first_line, ";"), ";", ",")

    swc_raw <- read_delim(swc_biomet_path, delim = delim, show_col_types = FALSE)
    names(swc_raw) <- trimws(names(swc_raw))

    # Парсим дату (ищем столбцы DATE_1/TIME_1 или TIMESTAMP)
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

      # Вычисляем среднее и переводим в % (м³/м³ * 100)
      swc_mean <- rowMeans(swc_mat, na.rm = TRUE) * 100

      swc_data <- tibble(
        DateTime = swc_dt,
        SWC_ext = swc_mean
      ) %>%
        filter(!is.na(DateTime))

      cat(sprintf("    Диапазон SWC: %.1f%% - %.1f%%\n",
                  min(swc_data$SWC_ext, na.rm = TRUE),
                  max(swc_data$SWC_ext, na.rm = TRUE)))

      # Присоединяем к основным данным
      biomet <- biomet %>%
        left_join(swc_data, by = "DateTime") %>%
        mutate(SWC = coalesce(SWC_ext, SWC)) %>%
        select(-SWC_ext)
    }
  }

  biomet
}

# ----------------------- Загрузка данных 2023 (Москва) - из gap-filled файла -----------------------
load_biomet_2023_gapfilled <- function(path) {
  raw <- read_delim(path, delim = ";", show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  # Парсим дату (формат dd.mm.yyyy или dd.mm.yyyy HH:MM)
  dt <- parse_dt_guess(raw$timestamp)

  # Извлекаем gap-filled переменные
  tair <- to_num(raw$Tair_f)
  vpd <- to_num(raw$VPD_f)

  # VPD может быть в Па вместо кПа
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
    SWC = NA_real_,  # Нет данных влажности почвы в этом файле
    Precip = NA_real_  # Осадки не включены в gap-filled файл
  ) %>%
    filter(!is.na(DateTime))
}

# ----------------------- Загрузка данных 2023 из biomet файла -----------------------
load_biomet_2023 <- function(biomet_path, precip_path = NULL) {
  raw <- read_csv(biomet_path, show_col_types = FALSE)
  names(raw) <- trimws(names(raw))

  # Парсим дату
  dt <- parse_dt_guess(raw$DateTime)

  # Извлекаем переменные
  tair <- to_num(raw$TA_1_1_1)
  rh <- to_num(raw$RH_1_1_1)

  # VPD вычисляем
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

  # Осадки из биомета
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
    filter(!is.na(DateTime))
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

# ----------------------- Загрузка осадков из Осадки.csv для 2023 -----------------------
load_precip_2023 <- function(precip_path, year = 2023) {
  if (!file.exists(precip_path)) return(NULL)

  # Файл в кодировке cp1251, столбцы: Число, Апр, Май, Июн, Июл, Авг, Сен
  raw <- read_delim(precip_path, delim = ";", skip = 1, show_col_types = FALSE,
                    locale = locale(encoding = "cp1251"))
  names(raw) <- trimws(names(raw))

  # Первый столбец - день месяца
  day_col <- names(raw)[1]
  month_cols <- names(raw)[-1]

  # Сопоставляем русские названия месяцев с номерами
  month_map <- c("Янв" = 1, "Фев" = 2, "Мар" = 3, "Апр" = 4, "Май" = 5, "Июн" = 6,
                 "Июл" = 7, "Авг" = 8, "Сен" = 9, "Окт" = 10, "Ноя" = 11, "Дек" = 12)

  precip_data <- NULL

  for (mcol in month_cols) {
    # Определяем номер месяца
    month_num <- NA
    for (mname in names(month_map)) {
      if (grepl(mname, mcol, ignore.case = TRUE)) {
        month_num <- month_map[[mname]]
        break
      }
    }
    if (is.na(month_num)) next

    # Извлекаем данные
    days <- to_num(raw[[day_col]])
    precip_vals <- raw[[mcol]]
    # "-" = 0, пустая строка = 0
    precip_vals <- ifelse(precip_vals == "-" | is.na(precip_vals) | precip_vals == "", "0", precip_vals)
    precip_vals <- to_num(precip_vals)
    precip_vals[is.na(precip_vals)] <- 0

    # Создаем даты
    valid_days <- !is.na(days) & days >= 1 & days <= 31
    dates <- as.Date(paste(year, month_num, days[valid_days], sep = "-"))

    month_data <- tibble(
      Date = dates,
      Precip_ext = precip_vals[valid_days]
    )

    if (is.null(precip_data)) {
      precip_data <- month_data
    } else {
      precip_data <- bind_rows(precip_data, month_data)
    }
  }

  if (!is.null(precip_data)) {
    precip_data <- precip_data %>%
      filter(!is.na(Date)) %>%
      group_by(Date) %>%
      summarise(Precip_ext = sum(Precip_ext, na.rm = TRUE), .groups = "drop") %>%
      arrange(Date)
  }

  precip_data
}

# ----------------------- Функция построения графиков -----------------------
create_meteo_plot <- function(biomet, year, bounds, location = "Курск",
                              roll_window_days = 7, ppfd_thresh = 10,
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

  # Фильтруем сезон и добавляем скользящие средние
  biomet_season <- biomet %>%
    mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
    filter(DateTime >= season_start & DateTime <= season_end) %>%
    arrange(DateTime) %>%
    mutate(
      PPFD_day = ifelse(is.finite(PPFD) & PPFD > ppfd_thresh, PPFD, NA_real_),
      Tair_roll = roll_mean_index(Tair, DateTime, roll_window_days),
      RH_roll = roll_mean_index(RH, DateTime, roll_window_days),
      VPD_roll = roll_mean_index(VPD, DateTime, roll_window_days),
      PPFD_roll = roll_mean_index(PPFD_day, DateTime, roll_window_days),
      Tsoil_roll = roll_mean_index(Tsoil, DateTime, roll_window_days),
      SWC_roll = roll_mean_index(SWC, DateTime, roll_window_days)
    )

  if (nrow(biomet_season) == 0) {
    cat(sprintf("  !! Нет данных в сезоне для %d\n", year))
    return(NULL)
  }

  # Ежедневные осадки
  precip_daily <- biomet_season %>%
    filter(is.finite(Precip)) %>%
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

  have_precip <- nrow(precip_daily) > 0 && any(is.finite(precip_daily$Precip_day) & precip_daily$Precip_day > 0)
  have_rh <- sum(is.finite(biomet_season$RH)) > 10

  # 1. Температура воздуха
  if (sum(is.finite(biomet_season$Tair)) > 10) {
    plots_list$Tair <- ggplot(biomet_season %>% filter(is.finite(Tair)),
                              aes(x = DateTime)) +
      geom_point(aes(y = Tair), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tair_roll), color = "#d62728", linewidth = 0.8, na.rm = TRUE) +
      labs(title = sprintf("Динамика метеоусловий за вегетационный сезон %d", year),
           x = "", y = "Температура воздуха (°C)") +
      time_scale +
      theme_meteo
  }

  # 2. VPD
  if (sum(is.finite(biomet_season$VPD)) > 10) {
    plots_list$VPD <- ggplot(biomet_season %>% filter(is.finite(VPD)),
                             aes(x = DateTime)) +
      geom_point(aes(y = VPD), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = VPD_roll), color = "#ff7f0e", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = "VPD (кПа)") +
      time_scale +
      theme_meteo
  }

  # 3. PPFD
  if (sum(is.finite(biomet_season$PPFD_day)) > 10) {
    plots_list$PPFD <- ggplot(biomet_season %>% filter(is.finite(PPFD_day)),
                              aes(x = DateTime)) +
      geom_point(aes(y = PPFD_day), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = PPFD_roll), color = "#9467bd", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = expression(PPFD~(мкмоль~м^{-2}~с^{-1}))) +
      time_scale +
      theme_meteo
  }

  # 4. Температура почвы
  if (sum(is.finite(biomet_season$Tsoil)) > 10) {
    plots_list$Tsoil <- ggplot(biomet_season %>% filter(is.finite(Tsoil)),
                               aes(x = DateTime)) +
      geom_point(aes(y = Tsoil), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tsoil_roll), color = "#8c564b", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "", y = "Температура почвы (°C)") +
      time_scale +
      theme_meteo
  }

  # 5. Влажность почвы
  if (sum(is.finite(biomet_season$SWC)) > 10) {
    plots_list$SWC <- ggplot(biomet_season %>% filter(is.finite(SWC)),
                             aes(x = DateTime)) +
      geom_point(aes(y = SWC), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = SWC_roll), color = "#17becf", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "Дата", y = "Влажность почвы (%)") +
      time_scale +
      theme_meteo
  }

  # 6. Осадки + Относительная влажность
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
      geom_point(data = biomet_season %>% filter(is.finite(RH)),
                 aes(x = DateTime, y = RH * rh_sf),
                 color = "grey40", size = 0.35, alpha = 0.35) +
      geom_line(data = biomet_season,
                aes(x = DateTime, y = RH_roll * rh_sf),
                color = "grey40", linewidth = 0.8, na.rm = TRUE) +
      scale_y_continuous(name = "Осадки (мм)",
                         sec.axis = sec_axis(~ . / rh_sf, name = "Относительная влажность (%)")) +
      labs(x = "Дата") +
      time_scale +
      theme_meteo
  } else if (have_precip) {
    plots_list$Precipitation <- ggplot(precip_daily, aes(x = DateTime)) +
      geom_col(aes(y = Precip_day), fill = "#1f77b4", color = "#1f77b4", alpha = 0.6,
               width = 24 * 60 * 60 * 0.9) +
      labs(x = "Дата", y = "Осадки (мм)") +
      time_scale +
      theme_meteo
  } else if (have_rh) {
    plots_list$RH <- ggplot(biomet_season %>% filter(is.finite(RH)),
                            aes(x = DateTime)) +
      geom_point(aes(y = RH), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = RH_roll), color = "#2ca02c", linewidth = 0.8, na.rm = TRUE) +
      labs(x = "Дата", y = "Относительная влажность (%)") +
      time_scale +
      theme_meteo
  }

  if (length(plots_list) == 0) {
    cat(sprintf("  !! Нет данных для графиков %d\n", year))
    return(NULL)
  }

  # Собираем в один график
  wrap_plots(plots_list, ncol = 2)
}

# ----------------------- ОСНОВНОЙ КОД -----------------------
cat("\n=== Построение графиков метеоусловий по годам ===\n\n")

# Пути к файлам
# Курск 2013
kursk_2013_path <- "Kursk_data_half_our.csv"
# Москва 2013
moscow_2013_path <- "biomet2013.csv"
# Москва 2016
biomet_2016_path <- "Moscow_2016_verFin.csv"
precip_2016_path <- "2016_precip.csv"
swc_2016_path <- "2016BiomB.csv"
# Москва 2023
gapfilled_2023_path <- "ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv"
biomet_2023_path <- "Anal11_biomet.csv"
precip_2023_path <- "Осадки.csv"

# Загрузка данных Курск 2013
cat("Загрузка данных 2013 (Курск)...\n")
biomet_kursk_2013 <- load_biomet_kursk_2013(kursk_2013_path)
cat(sprintf("  Загружено %d записей, диапазон: %s - %s\n",
            nrow(biomet_kursk_2013),
            min(biomet_kursk_2013$Date, na.rm = TRUE),
            max(biomet_kursk_2013$Date, na.rm = TRUE)))

# Загрузка данных Москва 2013
cat("\nЗагрузка данных 2013 (Москва)...\n")
biomet_moscow_2013 <- load_biomet_moscow_2013(moscow_2013_path)
cat(sprintf("  Загружено %d записей, диапазон: %s - %s\n",
            nrow(biomet_moscow_2013),
            min(biomet_moscow_2013$Date, na.rm = TRUE),
            max(biomet_moscow_2013$Date, na.rm = TRUE)))

cat("\nЗагрузка данных 2016 (Москва)...\n")
biomet_2016 <- load_biomet_2016(biomet_2016_path, precip_2016_path, swc_2016_path)
cat(sprintf("  Загружено %d записей, диапазон: %s - %s\n",
            nrow(biomet_2016),
            min(biomet_2016$Date, na.rm = TRUE),
            max(biomet_2016$Date, na.rm = TRUE)))

cat("\nЗагрузка данных 2023 (Москва)...\n")
# Сначала загружаем gap-filled данные (с 18.05.2023)
biomet_2023_gf <- NULL
if (file.exists(gapfilled_2023_path)) {
  cat("  Загружаем gap-filled данные из ИТОГ2...\n")
  biomet_2023_gf <- load_biomet_2023_gapfilled(gapfilled_2023_path)
  cat(sprintf("    Загружено %d записей gap-filled, диапазон: %s - %s\n",
              nrow(biomet_2023_gf),
              min(biomet_2023_gf$Date, na.rm = TRUE),
              max(biomet_2023_gf$Date, na.rm = TRUE)))
}
# Затем загружаем biomet данные (с влажностью почвы и осадками)
biomet_2023_bio <- NULL
if (file.exists(biomet_2023_path)) {
  cat("  Загружаем biomet данные из Anal11...\n")
  biomet_2023_bio <- load_biomet_2023(biomet_2023_path, precip_2023_path)
  cat(sprintf("    Загружено %d записей biomet, диапазон: %s - %s\n",
              nrow(biomet_2023_bio),
              min(biomet_2023_bio$Date, na.rm = TRUE),
              max(biomet_2023_bio$Date, na.rm = TRUE)))
}
# Объединяем данные
biomet_2023 <- merge_biomet_2023(biomet_2023_gf, biomet_2023_bio)
cat(sprintf("  Итого объединено %d записей, диапазон: %s - %s\n",
            nrow(biomet_2023),
            min(biomet_2023$Date, na.rm = TRUE),
            max(biomet_2023$Date, na.rm = TRUE)))

# Загружаем осадки из отдельного файла Осадки.csv
if (file.exists(precip_2023_path)) {
  cat("  Загружаем осадки из Осадки.csv...\n")
  precip_2023 <- load_precip_2023(precip_2023_path, year = 2023)
  if (!is.null(precip_2023) && nrow(precip_2023) > 0) {
    cat(sprintf("    Загружено %d дней осадков\n", nrow(precip_2023)))
    # Добавляем осадки к основным данным (по дате)
    biomet_2023 <- biomet_2023 %>%
      left_join(precip_2023, by = "Date") %>%
      mutate(Precip = coalesce(Precip_ext, Precip)) %>%
      select(-Precip_ext)
  }
}

# Построение и сохранение графиков
cat("\n=== Построение графиков ===\n")

cat("\n2013 (Курск)...\n")
p_kursk_2013 <- create_meteo_plot(biomet_kursk_2013, 2013, B2013_Kursk, location = "Курск")
if (!is.null(p_kursk_2013)) {
  ggsave("Meteo_dynamics_2013_Kursk_ru.png", p_kursk_2013,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Сохранено: Meteo_dynamics_2013_Kursk_ru.png\n")
}

cat("\n2013 (Москва)...\n")
p_moscow_2013 <- create_meteo_plot(biomet_moscow_2013, 2013, B2013_Moscow, location = "Москва")
if (!is.null(p_moscow_2013)) {
  ggsave("Meteo_dynamics_2013_Moscow_ru.png", p_moscow_2013,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Сохранено: Meteo_dynamics_2013_Moscow_ru.png\n")
}

cat("\n2016 (Москва)...\n")
p2016 <- create_meteo_plot(biomet_2016, 2016, B2016, location = "Москва")
if (!is.null(p2016)) {
  ggsave("Meteo_dynamics_2016_Moscow_ru.png", p2016,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Сохранено: Meteo_dynamics_2016_Moscow_ru.png\n")
}

cat("\n2023 (Москва)...\n")
p2023 <- create_meteo_plot(biomet_2023, 2023, B2023, location = "Москва", use_data_start = TRUE)
if (!is.null(p2023)) {
  ggsave("Meteo_dynamics_2023_Moscow_ru.png", p2023,
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("  -> Сохранено: Meteo_dynamics_2023_Moscow_ru.png\n")
}

cat("\n=== Готово! ===\n")
