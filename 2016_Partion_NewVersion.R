# --------------------------------------------------------------------------
# 2016 год 
# Eddy Covariance: Gap-filling + u* + Lasslop-only (GL) flux partitioning
# --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(REddyProc)
  library(tidyverse)
  library(lubridate)
  library(tidyr)
  library(stringr)
})

# -------------------------- 0. Параметры/пути ------------------------------
eddypro_file_path <- "eddypro_2016B_full_output_2025-04-09T164002_adv.csv"
meteo_file_path   <- "2016BiomB.csv"
output_csv_path   <- "Moscow_2016_verFin_newVersion.csv"
plots_output_dir  <- "New_REddyProc_Plots_2016_Lasslop"
dir.create(plots_output_dir, showWarnings = FALSE, recursive = TRUE)

site_id     <- "2016-Barley"
lat_deg     <- 55 + 50/60 + 14/3600   # 55.83722
long_deg    <- 37 + 33/60 + 56/3600   # 37.56556
timezone_h  <- 3                      # Europe/Moscow = UTC+3
local_tz    <- "Europe/Moscow"
dts_per_day <- 48

# Время старта общей части сезона (в UTC-логике не опираемся; используем локальную зону)
common_start_datetime <- as.POSIXct("2016-05-10 00:30:00", tz = local_tz)

# -------------------------- 1. Загрузка -----------------------------------
message("Загрузка EddyPro...")
hdr_ep <- readLines(eddypro_file_path, n = 1, warn = FALSE)[1]
col_ep <- str_trim(gsub('^"|"$', '', strsplit(hdr_ep, ";")[[1]]))
ep_raw <- read_delim(
  eddypro_file_path, delim = ";", comment = "#", skip = 2,
  col_names = col_ep, col_types = cols(.default = "c"),
  na = c("-9999","-9999.0","NA","","--"),
  locale = locale(decimal_mark = ".")
)

message("Загрузка Meteo...")
hdr_met <- readLines(meteo_file_path, n = 1, warn = FALSE)[1]
col_met <- str_trim(gsub('^"|"$', '', strsplit(hdr_met, ";")[[1]]))
met_raw <- read_delim(
  meteo_file_path, delim = ";", comment = "#", skip = 3,
  col_names = col_met, col_types = cols(.default = "c"),
  na = c("-9999","-9999.0","NA","","--"),
  locale = locale(decimal_mark = ".")
)

# Удаляем полностью пустые строки
ep_raw  <- ep_raw[rowSums(is.na(ep_raw))  != ncol(ep_raw), ]
met_raw <- met_raw[rowSums(is.na(met_raw)) != ncol(met_raw), ]

# -------------------------- 2. Преобразования ------------------------------
# 2.1 Ustar: нормализуем имя
u_candidates <- intersect(c("Ustar","ustar","u*","u.star","u_star"), names(ep_raw))
if (length(u_candidates) > 0 && !"Ustar" %in% names(ep_raw)) {
  ep_raw <- ep_raw %>% rename(Ustar = all_of(u_candidates[1]))
}

# 2.2 EddyPro: приводим числа и время (локальная зона)
ep_num <- intersect(c("co2_flux","H","LE","Ustar","qc_co2_flux","qc_H","qc_LE"), names(ep_raw))
ep <- ep_raw %>%
  mutate(
    across(all_of(ep_num), ~ suppressWarnings(as.numeric(as.character(.)))),
    DateTime = dmy_hm(paste(.data$date, .data$time), tz = local_tz)
  ) %>%
  filter(!is.na(DateTime), DateTime >= common_start_datetime) %>%
  transmute(
    DateTime,
    NEE = co2_flux,
    H, LE, Ustar,
    qc_NEE = qc_co2_flux, qc_H, qc_LE
  ) %>%
  mutate(
    NEE = ifelse(qc_NEE == 2, NA_real_, NEE),
    H   = ifelse(qc_H   == 2, NA_real_, H),
    LE  = ifelse(qc_LE  == 2, NA_real_, LE)
  )

# 2.3 Meteo: TIMESTAMP -> локальная зона; расчёт переменных, единицы
parse_ts <- function(x) parse_date_time(x, orders = c("Ymd HM","Ymd HMS","dmY HM","dmY HMS"),
                                        tz = local_tz, quiet = TRUE)

if ("TIMESTAMP" %in% names(met_raw)) {
  met <- met_raw %>%
    mutate(
      TIMESTAMP = str_trim(as.character(TIMESTAMP)),
      DT0 = parse_ts(TIMESTAMP),
      DateTime = DT0 + minutes(30)
    ) %>%
    filter(!is.na(DateTime), DateTime >= common_start_datetime)
} else {
  stop("В meteo отсутствует TIMESTAMP — требуется корректный временной столбец.")
}

# Безопасное преобразование к numeric
cnv_num <- function(df, from, to) {
  if (from %in% names(df)) {
    v <- as.character(df[[from]])
    out <- suppressWarnings(as.numeric(v))
    df[[to]] <- out
  } else {
    df[[to]] <- NA_real_
  }
  df
}

met <- met %>%
  { cnv_num(., "SR01Up_Avg", "Rg")    } %>%
  { cnv_num(., "AirTC_Avg",  "Tair")  } %>%
  { cnv_num(., "RH",         "rH")    } %>%
  { cnv_num(., "NetRs_Avg",  "Rn")    }

# Средняя Tsoil по доступным глубинам
tsoil_cols <- paste0("TSoil_", 1:6, "_Avg")
have_ts   <- tsoil_cols[tsoil_cols %in% names(met)]
if (length(have_ts) > 0) {
  for (c in have_ts) met <- cnv_num(met, c, paste0("tmp_", c))
  met$Tsoil <- rowMeans(met[, paste0("tmp_", have_ts), drop = FALSE], na.rm = TRUE)
  met$Tsoil[is.nan(met$Tsoil)] <- NA_real_
  met <- met %>% select(-starts_with("tmp_TSoil_"))
} else {
  met$Tsoil <- NA_real_
}

# VPD в hPa (жёстко фиксируем единицы)
if (all(c("SVP_kPa","VP_kPa") %in% names(met))) {
  met <- met %>% { cnv_num(., "SVP_kPa", "SVP_kPa_num") } %>% { cnv_num(., "VP_kPa", "VP_kPa_num") }
  met$VPD <- (met$SVP_kPa_num - met$VP_kPa_num) * 10  # kPa -> hPa
  met <- met %>% select(-any_of(c("SVP_kPa_num","VP_kPa_num")))
} else if (all(c("Tair","rH") %in% names(met))) {
  met$VPD <- REddyProc::fCalcVPDfromRHandTair(met$rH, met$Tair) # hPa
} else {
  met$VPD <- NA_real_
}

# PPFD из Rg
if ("Rg" %in% names(met)) {
  vis <- REddyProc::fConvertGlobalToVisible(met$Rg)
  met$PPFD <- REddyProc::fConvertVisibleWm2toPhotons(vis)
} else met$PPFD <- NA_real_

met <- met %>% select(DateTime, Rg, Tair, rH, VPD, PPFD, Rn, Tsoil)

# ---------------- 3. УСТОЙЧИВАЯ НОРМАЛИЗАЦИЯ ВРЕМЕНИ + MERGE ----------------
message("Нормализация временных меток к 30-мин сетке...")

# Универсальная функция: округляет метки к ближайшим 30 минутам, агрегирует дубли
normalize_to_halfhour <- function(df, time_col = "DateTime", tz = "Europe/Moscow") {
  if (!time_col %in% names(df)) stop("В датафрейме нет столбца времени: ", time_col)
  dt <- df[[time_col]]

  # Приведение к POSIXct при необходимости
  if (!lubridate::is.POSIXct(dt)) {
    dt <- suppressWarnings(lubridate::parse_date_time(
      as.character(dt),
      orders = c("Ymd HMS","Ymd HM","dmY HMS","dmY HM","YmdHMS","YmdHM"),
      tz = tz, quiet = TRUE
    ))
  }

  # Удаляем строки с NA по времени
  keep <- !is.na(dt)
  df <- df[keep, , drop = FALSE]
  dt <- dt[keep]

  # Округляем ко ближайшему 30-мин бину
  bin <- lubridate::round_date(dt, unit = "30 minutes")
  dev_sec <- as.numeric(abs(dt - bin), units = "secs")

  # Диагностика отклонений
  n_tot <- length(dt)
  n_shift_gt10m <- sum(dev_sec > 600, na.rm = TRUE)
  n_shift_gt2m  <- sum(dev_sec > 120 & dev_sec <= 600, na.rm = TRUE)
  n_shift_le2m  <- sum(dev_sec <= 120, na.rm = TRUE)
  message(sprintf("Округление времени к 30-мин: всего %d; <=2 мин: %d; 2–10 мин: %d; >10 мин: %d",
                  n_tot, n_shift_le2m, n_shift_gt2m, n_shift_gt10m))
  if (n_shift_gt10m > 0)
    warning("Есть метки с отклонением >10 минут от 30-мин бинов. Проверьте источник/часовой пояс.")

  # Готовим к агрегированию по бинам
  df$.__bin__ <- bin

  # Числовые и прочие колонки,
  # ВАЖНО: исключаем исходный столбец времени и служебный .__bin__
  num_cols   <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols   <- setdiff(num_cols, c(time_col, ".__bin__"))
  other_cols <- setdiff(names(df),  c(num_cols, time_col, ".__bin__"))

  df_norm <- df %>%
    dplyr::group_by(.__bin__) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(num_cols), ~ suppressWarnings(mean(.x, na.rm = TRUE))),
      dplyr::across(dplyr::all_of(other_cols), ~ dplyr::first(.x)),
      .groups = "drop"
    ) %>%
    dplyr::rename(DateTime = .__bin__) %>%
    dplyr::arrange(DateTime)

  return(df_norm)
}

# Нормализуем обе таблицы на 30-мин бины в локальной зоне
ep_norm  <- if (nrow(ep)  > 0) normalize_to_halfhour(ep,  "DateTime", tz = local_tz) else tibble(DateTime=as.POSIXct(character()))
met_norm <- if (nrow(met) > 0) normalize_to_halfhour(met, "DateTime", tz = local_tz) else tibble(DateTime=as.POSIXct(character()))

if (nrow(ep_norm) == 0 && nrow(met_norm) == 0) stop("После нормализации нет данных ни в EddyPro, ни в Meteo.")

# Полная 30-мин сетка по объединённым границам
min_dt <- suppressWarnings(min(c(ep_norm$DateTime, met_norm$DateTime), na.rm = TRUE))
max_dt <- suppressWarnings(max(c(ep_norm$DateTime, met_norm$DateTime), na.rm = TRUE))
time_index <- tibble(DateTime = seq(
  from = lubridate::floor_date(min_dt, "30 minutes"),
  to   = lubridate::ceiling_date(max_dt, "30 minutes"),
  by   = "30 mins"
))

# Слияние на полную сетку
merged <- time_index %>%
  dplyr::left_join(ep_norm,  by = "DateTime") %>%
  dplyr::left_join(met_norm, by = "DateTime") %>%
  dplyr::distinct(DateTime, .keep_all = TRUE) %>%
  dplyr::arrange(DateTime)

# Мягкие фильтры до MDS
merged <- merged %>%
  dplyr::mutate(
    VPD   = dplyr::if_else(VPD   < 0 | VPD   > 60, NA_real_, VPD),
    Ustar = dplyr::if_else(!is.na(Ustar) & (Ustar < 0 | Ustar > 5), NA_real_, Ustar),
    NEE   = dplyr::if_else(!is.na(NEE)   & (NEE   < -30 | NEE > 30), NA_real_, NEE)
  )

# Устойчивая проверка сетки: допускаем только 1800-сек шаг
dt_diff <- as.numeric(diff(merged$DateTime), units = "secs")
bad_ix  <- which(!(dt_diff %in% 1800))
if (length(bad_ix) > 0) {
  # Как крайняя мера — «жёсткая подгонка»: перестраиваем по полной сетке и просто оставляем NA там, где данных нет.
  warning("Обнаружены интервалы !=1800 сек после нормализации. Применяю жёсткую подгонку к полной сетке.")
  merged <- time_index %>%
    dplyr::left_join(merged %>% dplyr::select(-DateTime) %>% dplyr::mutate(DateTime = merged$DateTime), by = "DateTime") %>%
    dplyr::arrange(DateTime) %>%
    dplyr::distinct(DateTime, .keep_all = TRUE)
  dt_diff2 <- as.numeric(diff(merged$DateTime), units = "secs")
  if (!all(dt_diff2 %in% 1800)) stop("Даже жёсткая подгонка не помогла — проверьте исходные метки/формат времени.")
} else {
  message("Сетка после нормализации корректна: шаг = 1800 сек.")
}

# Обновляем финальный список колонок и типы
final_vars <- c("DateTime","NEE","Tair","rH","LE","H","Ustar","VPD","Rg","PPFD","Rn","Tsoil")
final_vars <- final_vars[final_vars %in% names(merged)]
if (!"DateTime" %in% final_vars) stop("Не найден DateTime после объединения.")

for (cn in setdiff(final_vars, "DateTime")) {
  if (!is.numeric(merged[[cn]])) merged[[cn]] <- suppressWarnings(as.numeric(as.character(merged[[cn]])))
}

# -------------------------- 4. sEddyProc и MDS -----------------------------
message("Инициализация sEddyProc...")
E <- sEddyProc$new(
  ID = site_id,
  Data = merged,
  ColNames = final_vars,
  ColPOSIXTime = "DateTime",
  DTS = dts_per_day,
  LatDeg = lat_deg,
  LongDeg = long_deg,
  TimeZoneHour = timezone_h
)

# Gap-fill драйверов + потенциальная радиация
E$sCalcPotRadiation(useSolartime = TRUE)
for (v in intersect(c("Rg","Rn","PPFD","Tsoil","Tair","VPD","LE","H"), names(E$sDATA))) {
  E$sMDSGapFill(v, FillAll = TRUE)
}

# -------------------------- 5. u* сезоны (авто: помесячно) ------------------
# Пока фенофазы неизвестны — используем начало каждого месяца, присутствующего в данных
edata <- E$sGetData()
tcol  <- if ("DateTime" %in% names(edata)) "DateTime" else "sDateTime"
if (is.null(tcol)) stop("Во внутреннем объекте нет времени.")

dt    <- edata[[tcol]]
yr    <- year(dt[!is.na(dt)])[1]
months_present <- sort(unique(month(dt)))
starts_doy <- yday(as.Date(paste(yr, months_present, 1, sep = "-")))
starts_tbl <- data.frame(V1 = starts_doy, V2 = yr)

seasonFactor <- usCreateSeasonFactorYdayYear(dt - 15*60, starts = starts_tbl)
E$sSetUStarSeasons(seasonFactor = seasonFactor)

# Порог u* и сценарии
E$sEstUstarThold(RgColName = if ("Rg_f" %in% names(E$sExportData())) "Rg_f" else "Rg")
E$sEstimateUstarScenarios(
  nSample = 100L,
  probs = c(0.05, 0.5, 0.95),
  seasonFactor = seasonFactor,
  UstarColName = "Ustar",
  NEEColName   = "NEE",
  TempColName  = if ("Tair_f" %in% names(E$sExportData())) "Tair_f" else "Tair",
  RgColName    = if ("Rg_f"   %in% names(E$sExportData())) "Rg_f"   else "Rg"
)

u_suf <- E$sGetUstarSuffixes()
if (length(u_suf) == 0) stop("Не удалось создать u*-сценарии.")
# Выбираем медианный (обычно 'U50')
med_suf <- if ("U50" %in% u_suf) "U50" else u_suf[ceiling(length(u_suf)/2)]

# --------------------------------------------------------------------------
# ШАГ u*: АДАПТИВНЫЕ СЕЗОНЫ + ВЕРСИОННО-АГНОСТИЧНЫЕ ВЫЗОВЫ REddyProc
# Предпосылки: объект E (sEddyProc) уже создан; драйверы MDS-заполнены.
# --------------------------------------------------------------------------

message("=== u* estimation: adaptive seasons + version-agnostic calls ===")

# Хелпер: вызвать функцию/метод только с поддерживаемыми аргументами
.call_supported <- function(fun, args_list) {
  # formals для RC-методов доступны: formals(E$sEstUstarThold)
  fml <- tryCatch(names(formals(fun)), error = function(e) NULL)
  if (is.null(fml)) {
    # если формалы недоступны, пробуем без фильтрации
    return(do.call(fun, args_list))
  }
  keep <- intersect(names(args_list), fml)
  do.call(fun, args_list[keep])
}

# 0) Имена колонок и базовые проверки
ED     <- E$sExportData()
E_DATA <- E$sGetData()

rg_col   <- if ("Rg_f"   %in% names(ED)) "Rg_f"   else if ("Rg"   %in% names(ED)) "Rg"   else NULL
tair_col <- if ("Tair_f" %in% names(ED)) "Tair_f" else if ("Tair" %in% names(ED)) "Tair" else NULL
vpd_col  <- if ("VPD_f"  %in% names(ED)) "VPD_f"  else if ("VPD"  %in% names(ED)) "VPD"  else NULL
if (is.null(rg_col))  stop("Не найден ни Rg_f, ни Rg в E$sExportData(). Заполните радиацию перед u*.")
if (!"Ustar" %in% names(E_DATA)) stop("Во внутренних данных нет Ustar.")
if (!"NEE"   %in% names(E_DATA)) stop("Во внутренних данных нет NEE.")

# 1) Подготовка времени и «ночных» точек для диагностики и подбора сезонов
tcol <- if ("DateTime" %in% names(E_DATA)) "DateTime" else if ("sDateTime" %in% names(E_DATA)) "sDateTime" else NULL
if (is.null(tcol)) stop("Во внутреннем объекте E нет временного столбца DateTime/sDateTime.")

dt     <- E_DATA[[tcol]]
yr     <- lubridate::year(dt[!is.na(dt)])[1]
swThr0 <- 20  # Вт·м^-2 — мягкий порог «ночи» для набора статистики (НЕ передаётся в метод, если он не поддерживает control)

rgv <- ED[[rg_col]]
ok_night <- !is.na(E_DATA$NEE) & !is.na(E_DATA$Ustar) & !is.na(E_DATA$Tair) &
            !is.na(rgv) & (rgv < swThr0)

# 2) Адаптивные сезоны: 3 → 2 → 1 (по наличию «ночных» точек)
starts3 <- sort(unique(c(lubridate::yday(as.Date(sprintf("%d-05-01", yr))),
                         lubridate::yday(as.Date(sprintf("%d-07-01", yr))),
                         lubridate::yday(as.Date(sprintf("%d-09-01", yr))))))
seasonFactor3 <- usCreateSeasonFactorYdayYear(dt - 15*60, starts = data.frame(V1 = starts3, V2 = yr))
cnt3 <- tapply(ok_night, seasonFactor3, sum, na.rm = TRUE)

uStarSeasonFactor <- NULL
if (length(cnt3) > 0 && all(cnt3 >= 300, na.rm = TRUE)) {
  uStarSeasonFactor <- seasonFactor3
  message("u*: использую 3 сезона (достаточно ночных точек).")
} else {
  starts2 <- sort(unique(c(lubridate::yday(as.Date(sprintf("%d-05-01", yr))),
                           lubridate::yday(as.Date(sprintf("%d-08-01", yr))))))
  seasonFactor2 <- usCreateSeasonFactorYdayYear(dt - 15*60, starts = data.frame(V1 = starts2, V2 = yr))
  cnt2 <- tapply(ok_night, seasonFactor2, sum, na.rm = TRUE)
  if (length(cnt2) > 0 && all(cnt2 >= 300, na.rm = TRUE)) {
    uStarSeasonFactor <- seasonFactor2
    message("u*: использую 2 сезона (достаточно ночных точек).")
  } else {
    start1 <- lubridate::yday(as.Date(sprintf("%d-01-01", yr)))
    seasonFactor1 <- usCreateSeasonFactorYdayYear(dt - 15*60, starts = data.frame(V1 = start1, V2 = yr))
    uStarSeasonFactor <- seasonFactor1
    message("u*: недостаточно ночных точек по сезонам — использую 1 сезон на год.")
  }
}
E$sSetUStarSeasons(seasonFactor = uStarSeasonFactor)

# Диагностика по выбранным сезонам (наша локальная переменная)
diag_u <- tibble(season = uStarSeasonFactor, night = ok_night) |>
  dplyr::group_by(season) |>
  dplyr::summarise(n_night = sum(night, na.rm = TRUE), .groups = "drop")
message("Ночные записи по выбранным сезонам:")
print(diag_u)

# 3) Оценка порога u* (без uStarControl, если не поддерживается)
message("Оценка порога u* (sEstUstarThold) c учётом различий версий...")
args_thold <- list(
  RgColName    = rg_col,
  UstarColName = "Ustar",
  NEEColName   = "NEE",
  TempColName  = tair_col,
  seasonFactor = uStarSeasonFactor # некоторые версии принимают, другие — игнорируют
)
# Вызовим только с теми аргументами, которые доступны в вашей версии:
res_thold <- tryCatch(.call_supported(E$sEstUstarThold, args_thold),
                      error = function(e) { stop("sEstUstarThold() не выполнился: ", e$message) })

# 4) Сценарии u* (5%, 50%, 95%) — также через «умный» вызов
message("Оценка сценариев u* (sEstimateUstarScenarios) с версионной совместимостью...")
args_scens <- list(
  nSample      = 100L,
  probs        = c(0.05, 0.5, 0.95),
  seasonFactor = uStarSeasonFactor,
  UstarColName = "Ustar",
  NEEColName   = "NEE",
  TempColName  = tair_col,
  RgColName    = rg_col
  # uStarControl отсутствует — ваша версия его не поддерживает
)
res_scens <- tryCatch(.call_supported(E$sEstimateUstarScenarios, args_scens),
                      error = function(e) { stop("sEstimateUstarScenarios() не выполнился: ", e$message) })

# 5) Проверка распределения порогов и выбор медианного суффикса
u_dist <- E$sGetEstimatedUstarThresholdDistribution()
message("Распределение порогов u* по агрегированиям:")
print(u_dist)

u_suf <- E$sGetUstarSuffixes()
if (length(u_suf) == 0) stop("Не удалось получить суффиксы u* сценариев (E$sGetUstarSuffixes() пуст).")
median_suffix <- if ("U50" %in% u_suf) "U50" else u_suf[ceiling(length(u_suf)/2)]
message(sprintf("Выбран медианный суффикс u*: %s", median_suffix))

# -------------------------- 6. Gap-fill NEE с учётом u* --------------------
E$sMDSGapFillUStarScens("NEE", FillAll = TRUE)
E$sGLFluxPartitionUStarScens(NEEVar = "NEE", TempVar = tair_col, VPDVar = vpd_col, RadVar = rg_col)
er0 <- E$sExportResults()
if (!any(grepl("^NEE_.*_f$", names(er0)))) stop("После MDS по u* не найден NEE_Uxx_f.")

# -------------------------- 7. Lasslop-only partitioning -------------------
t_var <- if ("Tair_f" %in% names(E$sExportData())) "Tair_f" else "Tair"
v_var <- if ("VPD_f"  %in% names(E$sExportData())) "VPD_f"  else "VPD"
r_var <- if ("Rg_f"   %in% names(E$sExportData())) "Rg_f"   else "Rg"

req_ok <- all(c(t_var, v_var, r_var) %in% names(E$sExportData()))
if (!req_ok) stop("Недостаточно драйверов для Lasslop: отсутствуют один или несколько из {Tair(_f), VPD(_f), Rg(_f)}.")

E$sGLFluxPartitionUStarScens(
  NEEVar = "NEE",
  TempVar = t_var,
  VPDVar  = v_var,
  RadVar  = r_var
)

# -------------------------- 8. Экспорт и финальная «дозаправка» -----------
edata  <- E$sExportData()
eres   <- E$sExportResults()

# Убираем дубли времени между частями и сшиваем
tcol_d <- if ("sDateTime" %in% names(edata)) "sDateTime" else "DateTime"
tcol_r <- if ("sDateTime" %in% names(eres))  "sDateTime" else if ("DateTime" %in% names(eres)) "DateTime" else NULL
if (!is.null(tcol_r)) eres <- eres %>% select(-all_of(tcol_r))

common_cols <- intersect(names(edata), names(eres))
eres <- if (length(common_cols) > 0) eres %>% select(-all_of(common_cols)) else eres

Filled <- bind_cols(edata, eres)
if (tcol_d %in% names(Filled)) Filled <- Filled %>% rename(timestamp = all_of(tcol_d))

# ------ Контроль заполненности и «последний резерв» (опционально) ---------
# Цель: довести ключевые ряды до >=95–100% при сохранении гладкости.
# Реализуем линейную интерполяцию по времени (с экстраполяцией на края) ДЛЯ
# производных/заполненных столбцов: *_f и GL-выходов (GPP_DT_*, Reco_DT_*).
lin_interp_fill <- function(x) {
  if (!is.numeric(x)) return(x)
  n <- length(x); idx <- which(!is.na(x))
  if (length(idx) == 0) return(x)
  approx(x = idx, y = x[idx], xout = seq_len(n), method = "linear", rule = 2)$y
}

fill_targets <- names(Filled)[
  grepl("_f$", names(Filled)) |
  grepl("^GPP_DT_",  names(Filled)) |
  grepl("^Reco_DT_", names(Filled))
]

for (nm in fill_targets) {
  before_na <- mean(is.na(Filled[[nm]]))
  if (is.finite(before_na) && before_na > 0) {
    Filled[[nm]] <- lin_interp_fill(Filled[[nm]])
  }
}

# Оценка заполненности ключевых полей
key_cols <- c(
  grep("^NEE_.*_f$", names(Filled), value = TRUE),
  grep("^GPP_DT_",  names(Filled), value = TRUE),
  grep("^Reco_DT_", names(Filled), value = TRUE),
  intersect(c("Rg_f","PPFD_f","Tair_f","VPD_f","Rn_f","Tsoil_f","LE_f","H_f"), names(Filled))
)
key_cols <- unique(key_cols)

coverage <- tibble(
  variable = key_cols,
  completeness = round(100 * (1 - colMeans(is.na(Filled[key_cols]))), 2)
)
message("Заполненность (ключевые):")
print(coverage)

# -------------------------- 9. Сохранение ---------------------------------
write.csv(Filled, output_csv_path, row.names = FALSE, na = "")
message(sprintf("Готово: %s", normalizePath(output_csv_path, winslash = "/")))

# -------------------------- 10. Мини-графики -------------------------------
# (опционально, только если есть данные)
if (nrow(Filled) > 0 && "timestamp" %in% names(Filled)) {
  # HHFluxes: минимальный набор
  vars_hh <- c(
    grep("^NEE_.*_f$", names(Filled), value = TRUE)[1],
    intersect(c("LE_f","H_f","Rg_f","Tair_f","VPD_f","PPFD_f","Rn_f","Tsoil_f"), names(Filled))
  ) %>% unique() %>% discard(is.na)
  for (v in vars_hh) {
    try(E$sPlotHHFluxes(Var.s = v, Dir.s = plots_output_dir, Format.s = "png"), silent = TRUE)
  }

  # Fingerprint по Tair_f (если есть)
  if ("Tair_f" %in% names(Filled)) {
    try(E$sPlotFingerprint(Var.s = "Tair_f", Dir.s = plots_output_dir, Format.s = "png"), silent = TRUE)
  }
}
# --------------------------------------------------------------------------
# Конец
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# ПЛОТЫ: NEE, GPP (Lasslop), Reco (Lasslop) — устойчиво к дубликатам имён
# --------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(lubridate)
})

# --- Общая утилита: безопасно добавить/привести timestamp без дубликатов ---
make_ts <- function(x) {
  if (is.null(x)) return(NULL)
  if (!lubridate::is.POSIXct(x)) {
    x <- suppressWarnings(lubridate::parse_date_time(
      as.character(x),
      orders = c("Ymd HMS","Ymd HM","dmY HMS","dmY HM","YmdHMS","YmdHM"),
      tz = "UTC", quiet = TRUE
    ))
  }
  as.POSIXct(x, tz = attr(x, "tzone") %||% "UTC")
}

safe_add_timestamp <- function(df, time_col, out_name = "timestamp") {
  if (is.na(time_col) || is.null(time_col) || !time_col %in% names(df)) {
    stop("Не найден временной столбец для построения '", out_name, "'.")
  }
  # Если целевой out_name уже существует и это не тот же столбец — перенесём его в backup
  if (out_name %in% names(df) && out_name != time_col) {
    backup <- paste0(out_name, "_orig")
    # гарантируем уникальность имени бэкапа
    while (backup %in% names(df)) backup <- paste0(backup, "_1")
    df <- df %>% rename(!!backup := all_of(out_name))
  }
  # Создаём/кастим целевой столбец
  if (out_name == time_col) {
    df[[out_name]] <- make_ts(df[[time_col]])
  } else {
    df[[out_name]] <- make_ts(df[[time_col]])
  }
  # Убираем дубликаты имён на всякий случай
  names(df) <- make.unique(names(df), sep = ".")
  df
}

# --- Папка для PNG ---
plots_output_dir <- get0("plots_output_dir", ifnotfound = "REddyProc_Plots_Season")
dir.create(plots_output_dir, showWarnings = FALSE, recursive = TRUE)

# --- 1) Забираем экспортированные таблицы из REddyProc ---
edata <- E$sExportData()
eres  <- E$sExportResults()

# --- 2) Определяем временные столбцы и безопасно формируем 'timestamp' ---
time_col_data <- dplyr::case_when(
  "timestamp" %in% names(edata) ~ "timestamp",
  "sDateTime" %in% names(edata) ~ "sDateTime",
  "DateTime"  %in% names(edata) ~ "DateTime",
  TRUE ~ NA_character_
)
if (is.na(time_col_data)) {
  # fallback: берём время из внутренних данных
  gd <- E$sGetData()
  tgd <- if ("DateTime" %in% names(gd)) "DateTime" else if ("sDateTime" %in% names(gd)) "sDateTime" else NA_character_
  stopifnot(!is.na(tgd))
  edata <- cbind(gd[tgd], edata)
  names(edata)[1] <- tgd
  time_col_data <- tgd
}
edata <- safe_add_timestamp(edata, time_col = time_col_data, out_name = "timestamp")
# Удалим возможные дубликаты по времени
edata <- edata %>% arrange(timestamp) %>% distinct(timestamp, .keep_all = TRUE)

# Для результатов сделаем отдельный столбец времени (если есть)
time_col_res <- dplyr::case_when(
  "timestamp" %in% names(eres) ~ "timestamp",
  "sDateTime" %in% names(eres) ~ "sDateTime",
  "DateTime"  %in% names(eres) ~ "DateTime",
  TRUE ~ NA_character_
)
if (!is.na(time_col_res)) {
  eres <- safe_add_timestamp(eres, time_col = time_col_res, out_name = "timestamp_res")
  eres <- eres %>% arrange(timestamp_res) %>% distinct(timestamp_res, .keep_all = TRUE)
} else {
  eres$timestamp_res <- NA
}

# --- 3) Сшивка: предпочтительно по времени, иначе по порядку (если размеры равны) ---
can_join_by_time <- any(!is.na(eres$timestamp_res)) && any(!is.na(edata$timestamp))
if (can_join_by_time) {
  # убираем из eres его внутренние временные имена
  eres <- eres %>% select(-any_of(c("timestamp","sDateTime","DateTime")))
  merged <- edata %>% left_join(eres, by = c("timestamp" = "timestamp_res"))
} else {
  if (nrow(edata) != nrow(eres)) {
    stop("Не удалось склеить edata и eres: нет общих временных меток и разное число строк.")
  }
  # удалим лишние временные столбцы из eres перед bind_cols
  eres <- eres %>% select(-any_of(c("timestamp","sDateTime","DateTime","timestamp_res")))
  merged <- bind_cols(edata, eres)
}

# --- 4) Определяем u*-суффикс (медианный или угадываем по именам) ---
u_suf <- tryCatch(E$sGetUstarSuffixes(), error = function(e) character(0))
median_suffix <- if (length(u_suf) == 0) {
  cand <- grep("^NEE_.*_f$", names(merged), value = TRUE)
  if (length(cand) == 0) stop("Не найден ни один столбец NEE_*_f. Запустите E$sMDSGapFillUStarScens('NEE').")
  sub("^NEE_(.+)_f$", "\\1", cand[1])
} else if ("U50" %in% u_suf) "U50" else u_suf[ceiling(length(u_suf)/2)]

# --- 5) Имена целевых столбцов и проверка наличия ---
nee_col  <- paste0("NEE_",     median_suffix, "_f")
gpp_col  <- paste0("GPP_DT_",  median_suffix)
reco_col <- paste0("Reco_DT_", median_suffix)

need <- c("timestamp", nee_col, gpp_col, reco_col)
miss <- setdiff(need, names(merged))
if (length(miss) > 0) {
  stop("Отсутствуют необходимые столбцы: ", paste(miss, collapse = ", "),
       "\nПроверьте, что выполнены: E$sMDSGapFillUStarScens('NEE') и E$sGLFluxPartitionUStarScens(...).")
}

# --- 6) Построение графиков ---
season_start <- min(merged$timestamp, na.rm = TRUE)
season_end   <- max(merged$timestamp, na.rm = TRUE)
season_tag   <- paste(format(season_start, "%Y-%m-%d"), "—", format(season_end, "%Y-%m-%d"))

plot_ts <- function(df, x, y, ylab, title, file) {
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_line(na.rm = TRUE) +
    labs(title = paste0(title, " (", season_tag, ")"),
         x = "Время", y = ylab) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = file.path(plots_output_dir, file), plot = p, width = 12, height = 5, dpi = 150)
  message("Сохранено: ", file.path(plots_output_dir, file))
}

merged <- arrange(merged, timestamp)

plot_ts(merged, "timestamp", nee_col,
        expression("NEE ("*mu*"mol CO"[2]*" m"^-2*" s"^-1*")"),
        paste0("Динамика NEE (", median_suffix, ")"),
        paste0("Season_NEE_", median_suffix, ".png"))

plot_ts(merged, "timestamp", gpp_col,
        expression("GPP ("*mu*"mol CO"[2]*" m"^-2*" s"^-1*")"),
        paste0("Динамика GPP (Lasslop, ", median_suffix, ")"),
        paste0("Season_GPP_GL_", median_suffix, ".png"))

plot_ts(merged, "timestamp", reco_col,
        expression("Reco ("*mu*"mol CO"[2]*" m"^-2*" s"^-1*")"),
        paste0("Динамика Reco (Lasslop, ", median_suffix, ")"),
        paste0("Season_Reco_GL_", median_suffix, ".png"))

# --- 7) Sanity-check ---
stopifnot(lubridate::is.POSIXct(merged$timestamp))
message("NEE: ", sum(!is.na(merged[[nee_col]])),
        " | GPP: ", sum(!is.na(merged[[gpp_col]])),
        " | Reco: ", sum(!is.na(merged[[reco_col]])))
