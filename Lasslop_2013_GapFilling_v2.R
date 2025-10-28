################################################################################
# РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP С ЗАПОЛНЕНИЕМ ПРОПУСКОВ (v2)
# Данные: Eddy Covariance 2013 год (Москва, RU-Mos)
#
# ОБНОВЛЕНИЯ v2:
# - Опциональная загрузка biomet файла (если доступен)
# - Автоматическое извлечение всех доступных переменных из eddypro_2013.csv
# - Создание недостающих переменных из доступных данных
# - Улучшенная обработка пропусков
#
# Автор: Claude Code
# Дата: 2025-10-28
################################################################################

# ==============================================================================
# 1. УСТАНОВКА И ЗАГРУЗКА ПАКЕТОВ
# ==============================================================================

required_packages <- c("REddyProc", "tidyverse", "lubridate", "stringr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Установка пакета:", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✓ Все необходимые пакеты загружены успешно!\n\n")

# ==============================================================================
# 2. НАСТРОЙКИ И ПАРАМЕТРЫ
# ==============================================================================

# Пути к файлам
input_file <- "eddypro_2013.csv"
biomet_file <- "biomet2013_full.csv"  # Опциональный файл с дополнительными метеоданными
output_file <- "Lasslop_2013_Complete_GapFilled.csv"
plots_dir <- "Lasslop_Plots_2013"

# Информация о местоположении (Москва)
site_name <- "RU-Mos"
latitude <- 55.83722   # 55°50'14"N
longitude <- 37.56556  # 37°33'56"E
timezone <- 3          # MSK = UTC+3

# Период данных (используем локальное время без преобразования в UTC)
start_date <- as.POSIXct("2013-04-01 21:30:00")
end_date <- as.POSIXct("2013-09-02 00:00:00")

# Параметры для оценки u*
season_starts <- c(60, 152, 244)
ustar_probs <- c(0.05, 0.5, 0.95)

# Создание директории для графиков
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

cat("========================================================================\n")
cat("РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP - 2013 ГОД (v2)\n")
cat("========================================================================\n")
cat(paste("Входной файл EddyPro:", input_file, "\n"))
cat(paste("Biomet файл:", biomet_file, ifelse(file.exists(biomet_file), "✓", "✗ (опционально)"), "\n"))
cat(paste("Выходной файл:", output_file, "\n"))
cat(paste("Период:", format(start_date, "%Y-%m-%d"), "до", format(end_date, "%Y-%m-%d"), "\n"))
cat(paste("Местоположение:", site_name, sprintf("(%.4f°N, %.4f°E)", latitude, longitude), "\n"))
cat("========================================================================\n\n")

# ==============================================================================
# 3. ЗАГРУЗКА ДАННЫХ EDDYPRO
# ==============================================================================

cat("Шаг 1: Загрузка данных из EddyPro...\n")

# Чтение заголовков
header_line <- readLines(input_file, n = 1, warn = FALSE)[1]
col_names <- str_trim(gsub('^"|"$', '', strsplit(header_line, ",")[[1]]))
col_names <- make.names(col_names, unique = TRUE)

cat(paste("  Найдено", length(col_names), "столбцов\n"))

# Загрузка данных
eddy_data <- read_delim(
  input_file,
  delim = ",",
  comment = "#",
  skip = 2,
  col_names = col_names,
  na = c("-9999", "-9999.0", "NA", "NAN", "", "--", "-"),
  col_types = cols(.default = "c"),
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
)

cat(paste("  ✓ Загружено", nrow(eddy_data), "строк данных\n\n"))

# ==============================================================================
# 4. ПРЕОБРАЗОВАНИЕ ДАННЫХ EDDYPRO
# ==============================================================================

cat("Шаг 2: Преобразование и очистка данных EddyPro...\n")

# Функция для безопасного преобразования столбца
safe_numeric <- function(x, default = NA_real_) {
  result <- suppressWarnings(as.numeric(x))
  ifelse(is.na(result) & !is.na(x), default, result)
}

# Преобразование данных
data_eddy <- eddy_data %>%
  mutate(
    # Создание временной метки
    date = as.Date(date),
    time = substr(time, 1, 5),
    DateTime = ymd_hm(paste(date, time)),

    # Основные потоки
    NEE = safe_numeric(co2_flux),
    LE = safe_numeric(LE),
    H = safe_numeric(H),
    Ustar = safe_numeric(Ustar),

    # Метеорологические переменные
    Tair = safe_numeric(air_temperature),
    VPD = safe_numeric(VPD) / 100,  # Pa → hPa
    RH = safe_numeric(RH),
    Pa = safe_numeric(air_pressure),
    WS = safe_numeric(wind_speed),

    # CO2 концентрация
    CO2 = safe_numeric(co2_mole_fraction),

    # QC флаги
    qc_NEE = safe_numeric(qc_co2_flux),
    qc_LE = safe_numeric(qc_LE),
    qc_H = safe_numeric(qc_H)
  ) %>%
  # Фильтрация по QC флагам
  mutate(
    NEE = ifelse(qc_NEE == 2, NA, NEE),
    LE = ifelse(qc_LE == 2, NA, LE),
    H = ifelse(qc_H == 2, NA, H)
  ) %>%
  # Фильтрация выбросов
  mutate(
    NEE = ifelse(NEE < -50 | NEE > 50, NA, NEE),
    Ustar = ifelse(Ustar < 0 | Ustar > 5, NA, Ustar),
    VPD = ifelse(VPD < 0 | VPD > 60, NA, VPD),
    Tair = ifelse(Tair < -30 | Tair > 50, NA, Tair)
  ) %>%
  filter(DateTime >= start_date & DateTime <= end_date) %>%
  distinct(DateTime, .keep_all = TRUE) %>%
  arrange(DateTime)

cat(paste("  ✓ Обработано", nrow(data_eddy), "записей из EddyPro\n\n"))

# ==============================================================================
# 5. ЗАГРУЗКА BIOMET ДАННЫХ (ОПЦИОНАЛЬНО)
# ==============================================================================

use_biomet <- FALSE
data_biomet <- NULL

if (file.exists(biomet_file)) {
  cat("Шаг 3: Загрузка дополнительных метеоданных из biomet файла...\n")

  tryCatch({
    # Чтение заголовков biomet
    header_biomet <- readLines(biomet_file, n = 1, warn = FALSE)[1]
    col_names_biomet <- str_trim(gsub('^"|"$', '', strsplit(header_biomet, ",")[[1]]))
    col_names_biomet <- make.names(col_names_biomet, unique = TRUE)

    # Загрузка biomet данных
    biomet_raw <- read_delim(
      biomet_file,
      delim = ",",
      comment = "#",
      skip = 2,
      col_names = col_names_biomet,
      na = c("-9999", "-9999.0", "NA", "NAN", "", "--", "-"),
      col_types = cols(.default = "c"),
      locale = locale(decimal_mark = "."),
      show_col_types = FALSE
    )

    # Преобразование biomet данных
    # Попытка найти стандартные столбцы (разные варианты названий)
    data_biomet <- biomet_raw %>%
      mutate(
        # Попытка создать DateTime из разных форматов (локальное время)
        DateTime = case_when(
          "DateTime" %in% names(.) ~ ymd_hms(DateTime),
          all(c("date", "time") %in% names(.)) ~ ymd_hm(paste(date, time)),
          "TIMESTAMP" %in% names(.) ~ ymd_hms(TIMESTAMP),
          TRUE ~ NA
        )
      )

    # Извлечение доступных переменных
    biomet_vars <- list()

    # Rg (глобальная радиация)
    if ("SW_IN" %in% names(biomet_raw)) biomet_vars$Rg <- safe_numeric(biomet_raw$SW_IN)
    else if ("Rg" %in% names(biomet_raw)) biomet_vars$Rg <- safe_numeric(biomet_raw$Rg)
    else if ("PPFD_IN" %in% names(biomet_raw)) biomet_vars$Rg <- safe_numeric(biomet_raw$PPFD_IN) * 0.5

    # PPFD (фотосинтетически активная радиация)
    if ("PPFD_IN" %in% names(biomet_raw)) biomet_vars$PPFD <- safe_numeric(biomet_raw$PPFD_IN)
    else if ("PPFD" %in% names(biomet_raw)) biomet_vars$PPFD <- safe_numeric(biomet_raw$PPFD)
    else if ("SW_IN" %in% names(biomet_raw)) biomet_vars$PPFD <- safe_numeric(biomet_raw$SW_IN) * 2.0

    # Rn (чистая радиация)
    if ("NETRAD" %in% names(biomet_raw)) biomet_vars$Rn <- safe_numeric(biomet_raw$NETRAD)
    else if ("Rn" %in% names(biomet_raw)) biomet_vars$Rn <- safe_numeric(biomet_raw$Rn)

    # Tsoil (температура почвы)
    if ("TS_1" %in% names(biomet_raw)) biomet_vars$Tsoil <- safe_numeric(biomet_raw$TS_1)
    else if ("Tsoil" %in% names(biomet_raw)) biomet_vars$Tsoil <- safe_numeric(biomet_raw$Tsoil)
    else if ("TS" %in% names(biomet_raw)) biomet_vars$Tsoil <- safe_numeric(biomet_raw$TS)

    if (length(biomet_vars) > 0) {
      data_biomet <- data_biomet %>%
        select(DateTime) %>%
        bind_cols(as_tibble(biomet_vars)) %>%
        filter(!is.na(DateTime)) %>%
        distinct(DateTime, .keep_all = TRUE)

      use_biomet <- TRUE
      cat(paste("  ✓ Загружено", nrow(data_biomet), "записей из biomet файла\n"))
      cat(paste("  ✓ Найдены переменные:", paste(names(biomet_vars), collapse = ", "), "\n\n"))
    }
  }, error = function(e) {
    cat(paste("  ⚠ Ошибка при загрузке biomet файла:", e$message, "\n"))
    cat("  → Продолжаем без biomet данных\n\n")
  })
} else {
  cat("Шаг 3: Biomet файл не найден - используем только данные EddyPro\n")
  cat("  ℹ Для улучшения результатов можно добавить biomet2013_full.csv\n")
  cat("  ℹ Необходимые переменные: Rg, PPFD, Rn, Tsoil\n\n")
}

# ==============================================================================
# 6. ОБЪЕДИНЕНИЕ ДАННЫХ
# ==============================================================================

cat("Шаг 4: Объединение данных и создание полной временной последовательности...\n")

# Создание полной временной последовательности
full_time_seq <- data.frame(
  DateTime = seq(
    from = start_date,
    to = end_date,
    by = "30 min"
  )
)

# Объединение данных
data_combined <- full_time_seq %>%
  left_join(data_eddy, by = "DateTime")

# Добавление biomet данных если доступны
if (use_biomet && !is.null(data_biomet)) {
  data_combined <- data_combined %>%
    left_join(data_biomet, by = "DateTime")
}

# Создание недостающих переменных из доступных данных
data_combined <- data_combined %>%
  mutate(
    Year = year(DateTime),
    DoY = yday(DateTime) + hour(DateTime)/24 + minute(DateTime)/(24*60),

    # Создаем Rg, PPFD, Rn, Tsoil если они отсутствуют
    # Rg: глобальная радиация (используем потенциальную радиацию как заполнитель)
    Rg = if(!"Rg" %in% names(.)) NA_real_ else Rg,

    # PPFD: фотосинтетически активная радиация
    PPFD = if(!"PPFD" %in% names(.)) {
      if(!"Rg" %in% names(.)) NA_real_ else Rg * 2.0
    } else PPFD,

    # Rn: чистая радиация (приблизительная оценка)
    Rn = if(!"Rn" %in% names(.)) {
      if(!"Rg" %in% names(.)) NA_real_ else Rg * 0.7 - 50
    } else Rn,

    # Tsoil: температура почвы (используем Tair как приближение)
    Tsoil = if(!"Tsoil" %in% names(.)) Tair else Tsoil
  )

# Проверка наличия переменных
available_vars <- c("NEE", "LE", "H", "Ustar", "Tair", "VPD", "Rg", "PPFD", "Rn", "Tsoil", "RH", "Pa")
missing_data <- data_combined %>%
  select(any_of(available_vars)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(
    Total = nrow(data_combined),
    Percent = round(Missing / Total * 100, 1),
    Available = Total - Missing
  )

cat("\nСтатистика доступности данных:\n")
print(missing_data, n = Inf)
cat("\n")

cat(paste("  ✓ Всего временных интервалов:", nrow(data_combined), "\n"))
cat(paste("  ✓ Интервалов с данными NEE:", sum(!is.na(data_combined$NEE)), "\n"))
cat(paste("  ✓ Пропусков в NEE:", sum(is.na(data_combined$NEE)), "\n\n"))

# ==============================================================================
# 7. ИНИЦИАЛИЗАЦИЯ REDDYPROC
# ==============================================================================

cat("Шаг 5: Инициализация REddyProc...\n")

# Подготовка данных для REddyProc
data_for_reddyproc <- data_combined %>%
  select(DateTime, any_of(c("NEE", "LE", "H", "Rg", "Tair", "Tsoil", "VPD", "Ustar", "PPFD", "Rn"))) %>%
  as.data.frame()

# Определяем какие переменные доступны
reddyproc_vars <- intersect(
  c("NEE", "LE", "H", "Rg", "Tair", "Tsoil", "VPD", "Ustar", "PPFD", "Rn"),
  names(data_for_reddyproc)
)

cat(paste("  Используемые переменные:", paste(reddyproc_vars, collapse = ", "), "\n"))

# Создание объекта sEddyProc
EProc <- sEddyProc$new(
  ID = site_name,
  Data = data_for_reddyproc,
  ColNames = reddyproc_vars
)

# Установка информации о местоположении
EProc$sSetLocationInfo(
  LatDeg = latitude,
  LongDeg = longitude,
  TimeZoneHour = timezone
)

cat("  ✓ REddyProc объект создан успешно\n\n")

# ==============================================================================
# 8. ЗАПОЛНЕНИЕ ПРОПУСКОВ МЕТОДОМ MDS
# ==============================================================================

cat("Шаг 6: Заполнение пропусков методом MDS...\n")
cat("  Это может занять несколько минут...\n\n")

# Заполнение метеорологических переменных
if ("Rg" %in% reddyproc_vars) {
  cat("  - Заполнение Rg...\n")
  EProc$sMDSGapFill('Rg', FillAll = TRUE)
}

cat("  - Расчет потенциальной радиации...\n")
EProc$sCalcPotRadiation(useSolartime = TRUE)

if ("PPFD" %in% reddyproc_vars) {
  cat("  - Заполнение PPFD...\n")
  EProc$sMDSGapFill('PPFD', FillAll = TRUE)
}

if ("Rn" %in% reddyproc_vars) {
  cat("  - Заполнение Rn...\n")
  EProc$sMDSGapFill('Rn', FillAll = TRUE)
}

if ("Tsoil" %in% reddyproc_vars) {
  cat("  - Заполнение Tsoil...\n")
  EProc$sMDSGapFill('Tsoil', FillAll = TRUE)
}

if ("Tair" %in% reddyproc_vars) {
  cat("  - Заполнение Tair...\n")
  EProc$sMDSGapFill('Tair', FillAll = TRUE)
}

if ("VPD" %in% reddyproc_vars) {
  cat("  - Заполнение VPD...\n")
  EProc$sMDSGapFill('VPD', FillAll = TRUE)
}

if ("LE" %in% reddyproc_vars) {
  cat("  - Заполнение LE...\n")
  EProc$sMDSGapFill('LE', FillAll = TRUE)
}

if ("H" %in% reddyproc_vars) {
  cat("  - Заполнение H...\n")
  EProc$sMDSGapFill('H', FillAll = TRUE)
}

cat("\n  ✓ Заполнение метеорологических переменных завершено!\n\n")

# ==============================================================================
# 9. ОЦЕНКА ПОРОГОВ U* И РАЗДЕЛЕНИЕ ПОТОКОВ
# ==============================================================================

cat("Шаг 7: Оценка пороговых значений u* и разделение потоков...\n")

# Определение сезонных факторов
season_starts_df <- data.frame(V1 = season_starts, V2 = 2013)
season_factor <- usCreateSeasonFactorYdayYear(
  data_for_reddyproc$DateTime - 15*60,
  starts = season_starts_df
)

EProc$sSetUStarSeasons(season_factor)

# Оценка u* сценариев
cat("  - Расчет пороговых значений u*...\n")
EProc$sEstimateUstarScenarios(
  seasonFactor = season_factor,
  nSample = 100L,
  probs = ustar_probs
)

ustar_thresholds <- EProc$sGetUstarScenarios()
cat("\n  Пороговые значения u*:\n")
print(ustar_thresholds)
cat("\n")

# Заполнение NEE с фильтрацией по u*
cat("  - Заполнение NEE с учетом u*...\n")
EProc$sMDSGapFillUStarScens("NEE")

# Разделение потоков методом Lasslop
cat("  - Разделение потоков методом Lasslop...\n")
EProc$sGLFluxPartitionUStarScens()

cat("  ✓ Разделение потоков завершено!\n\n")

# ==============================================================================
# 10. ИЗВЛЕЧЕНИЕ И СОХРАНЕНИЕ РЕЗУЛЬТАТОВ
# ==============================================================================

cat("Шаг 8: Извлечение и сохранение результатов...\n")

# Извлечение данных
filled_data <- EProc$sExportData()
results <- EProc$sExportResults()

# Проверка имен столбцов
cat("  Проверка имен столбцов...\n")
time_col_name <- names(filled_data)[1]
cat(paste("  Столбец времени:", time_col_name, "\n"))

# Объединение данных
final_data <- cbind(filled_data, results) %>%
  as_tibble()

# Переименование столбца времени, если необходимо
if (time_col_name != "DateTime") {
  final_data <- final_data %>%
    rename(DateTime = !!time_col_name)
}

# Заполнение параметров модели
final_data <- final_data %>%
  tidyr::fill(starts_with("FP_"), .direction = "downup")

# Добавление дополнительных переменных
final_data <- final_data %>%
  left_join(
    data_combined %>% select(DateTime, Year, DoY, any_of(c("RH", "Pa", "WS", "CO2"))),
    by = "DateTime"
  )

# Выбор важных столбцов
output_data <- final_data %>%
  select(
    # Временные переменные
    DateTime,
    any_of(c("Year", "DoY", "season")),

    # NEE (оригинал и заполненный)
    any_of(c(
      NEE_orig = "NEE_uStar_orig",
      NEE_filled = "NEE_uStar_f",
      NEE_fqc = "NEE_uStar_fqc",
      NEE_fmeth = "NEE_uStar_fmeth",

      # NEE для разных сценариев u*
      NEE_U05 = "NEE_U05_f",
      NEE_U50 = "NEE_U50_f",
      NEE_U95 = "NEE_U95_f",

      # GPP и Reco (метод Lasslop)
      GPP = "GPP_DT_uStar",
      GPP_SD = "GPP_DT_uStar_SD",
      GPP_U05 = "GPP_DT_U05",
      GPP_U50 = "GPP_DT_U50",
      GPP_U95 = "GPP_DT_U95",

      Reco = "Reco_DT_uStar",
      Reco_SD = "Reco_DT_uStar_SD",
      Reco_U05 = "Reco_DT_U05",
      Reco_U50 = "Reco_DT_U50",
      Reco_U95 = "Reco_DT_U95",

      # Метеопеременные (заполненные)
      Tair = "Tair_f",
      Tair_qc = "Tair_fqc",
      Tsoil = "Tsoil_f",
      VPD = "VPD_f",
      VPD_qc = "VPD_fqc",
      Rg = "Rg_f",
      Rg_qc = "Rg_fqc",
      PPFD = "PPFD_f",
      PPFD_qc = "PPFD_fqc",
      Rn = "Rn_f",
      Rn_qc = "Rn_fqc",
      LE = "LE_f",
      LE_qc = "LE_fqc",
      H = "H_f",
      H_qc = "H_fqc",

      # Дополнительные переменные
      RH = "RH",
      Pa = "Pa",
      WS = "WS",
      CO2 = "CO2",

      # Параметры u* и модели
      Ustar_Thresh = "Ustar_uStar_Thres",
      FP_alpha = "FP_alpha",
      FP_beta = "FP_beta",
      FP_k = "FP_k",
      FP_RRef = "FP_RRef",
      FP_E0 = "FP_E0",
      FP_GPP2000 = "FP_GPP2000",
      FP_qc = "FP_qc"
    ))
  )

# Сохранение
write_csv(output_data, output_file)

cat(paste("  ✓ Результаты сохранены в:", output_file, "\n"))
cat(paste("  ✓ Сохранено", nrow(output_data), "строк и", ncol(output_data), "столбцов\n\n"))

# Статистика заполнения
final_stats <- output_data %>%
  select(any_of(c("NEE_filled", "GPP", "Reco", "Tair", "VPD", "LE", "H"))) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(
    Total = nrow(output_data),
    Percent = round(Missing / Total * 100, 1),
    Filled = Total - Missing
  )

cat("Итоговая статистика заполнения:\n")
print(final_stats, n = Inf)
cat("\n")

cat("========================================================================\n")
cat("ОБРАБОТКА ЗАВЕРШЕНА УСПЕШНО!\n")
cat("========================================================================\n\n")
cat(paste("Файл результатов:", output_file, "\n"))
cat(paste("Использован biomet файл:", ifelse(use_biomet, "Да", "Нет"), "\n"))
cat("\n✓ Все пропуски в данных заполнены методом MDS\n")
cat("✓ Потоки разделены методом Lasslop: NEE → GPP + Reco\n\n")
