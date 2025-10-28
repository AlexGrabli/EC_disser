################################################################################
# РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP С ДАННЫМИ BIOMET
# Данные: Eddy Covariance 2013 год (Москва, RU-Mos)
#
# Этот скрипт объединяет данные EddyPro и Biomet для полного анализа:
# 1. Загрузка eddypro_2013.csv (потоки CO2, H, LE)
# 2. Загрузка biomet2013.csv (радиация, температура, влажность)
# 3. Заполнение пропусков методом MDS
# 4. Разделение потоков методом Lasslop: NEE → GPP + Reco
# 5. Оценка пороговых значений u*
#
# Автор: Claude Code
# Дата: 2025-10-28
# Версия: 3.0 (с обязательным biomet)
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
eddypro_file <- "eddypro_2013.csv"
biomet_file <- "biomet2013.csv"
output_file <- "Lasslop_2013_Complete_GapFilled.csv"
plots_dir <- "Lasslop_Plots_2013"

# Информация о местоположении (Москва)
site_name <- "RU-Mos"
latitude <- 55.83722   # 55°50'14"N
longitude <- 37.56556  # 37°33'56"E
timezone <- 3          # MSK = UTC+3

# Период данных
start_date <- as.POSIXct("2013-04-01 21:30:00", tz = "UTC")
end_date <- as.POSIXct("2013-09-02 00:00:00", tz = "UTC")

# Параметры для оценки u*
season_starts <- c(60, 152, 244)
ustar_probs <- c(0.05, 0.5, 0.95)

# Создание директории для графиков
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

cat("========================================================================\n")
cat("РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP - 2013 ГОД\n")
cat("С данными BIOMET (радиация, температура, влажность почвы)\n")
cat("========================================================================\n")
cat(paste("Файл EddyPro:", eddypro_file, "\n"))
cat(paste("Файл Biomet:", biomet_file, "\n"))
cat(paste("Выходной файл:", output_file, "\n"))
cat(paste("Период:", format(start_date, "%Y-%m-%d"), "до", format(end_date, "%Y-%m-%d"), "\n"))
cat("========================================================================\n\n")

# ==============================================================================
# 3. ПРОВЕРКА НАЛИЧИЯ ФАЙЛОВ
# ==============================================================================

if (!file.exists(eddypro_file)) {
  stop(paste("ОШИБКА: Файл EddyPro не найден:", eddypro_file))
}

if (!file.exists(biomet_file)) {
  stop(paste("ОШИБКА: Файл Biomet не найден:", biomet_file,
             "\nУбедитесь, что файл biomet2013.csv находится в рабочей директории!"))
}

cat("✓ Оба файла данных найдены\n\n")

# ==============================================================================
# 4. ЗАГРУЗКА ДАННЫХ EDDYPRO
# ==============================================================================

cat("Шаг 1: Загрузка данных EddyPro...\n")

# Чтение заголовков
header_eddy <- readLines(eddypro_file, n = 1, warn = FALSE)[1]
col_names_eddy <- str_trim(gsub('^"|"$', '', strsplit(header_eddy, ",")[[1]]))
col_names_eddy <- make.names(col_names_eddy, unique = TRUE)

# Загрузка данных
eddy_data <- read_delim(
  eddypro_file,
  delim = ",",
  comment = "#",
  skip = 2,
  col_names = col_names_eddy,
  na = c("-9999", "-9999.0", "NA", "NAN", "", "--", "-"),
  col_types = cols(.default = "c"),
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
)

cat(paste("  ✓ Загружено", nrow(eddy_data), "строк из EddyPro\n\n"))

# ==============================================================================
# 5. ЗАГРУЗКА ДАННЫХ BIOMET
# ==============================================================================

cat("Шаг 2: Загрузка данных Biomet...\n")

# Чтение заголовков (первая строка)
header_biomet <- readLines(biomet_file, n = 1, warn = FALSE)[1]
col_names_biomet <- str_trim(strsplit(header_biomet, ";")[[1]])
col_names_biomet <- make.names(col_names_biomet, unique = TRUE)

cat(paste("  Найдено", length(col_names_biomet), "столбцов в Biomet\n"))

# Загрузка данных (пропускаем 2 строки: заголовок и единицы измерения)
biomet_data <- read_delim(
  biomet_file,
  delim = ";",
  skip = 2,  # Пропускаем заголовок и единицы
  col_names = col_names_biomet,
  na = c("NAN", "NA", ""),
  col_types = cols(.default = "c"),
  locale = locale(decimal_mark = "."),
  show_col_types = FALSE
)

cat(paste("  ✓ Загружено", nrow(biomet_data), "строк из Biomet\n"))
cat(paste("  Период данных Biomet: ", min(biomet_data$DATE_1, na.rm = TRUE),
          "до", max(biomet_data$DATE_1, na.rm = TRUE), "\n\n"))

# ==============================================================================
# 6. ПРЕОБРАЗОВАНИЕ ДАННЫХ EDDYPRO
# ==============================================================================

cat("Шаг 3: Обработка данных EddyPro...\n")

# Функция безопасного преобразования
safe_numeric <- function(x) {
  result <- suppressWarnings(as.numeric(x))
  result
}

# Преобразование EddyPro данных
data_eddy <- eddy_data %>%
  mutate(
    # Создание временной метки
    date = as.Date(date),
    time = substr(time, 1, 5),
    DateTime = ymd_hm(paste(date, time), tz = "UTC"),

    # Основные потоки
    NEE = safe_numeric(co2_flux),
    LE = safe_numeric(LE),
    H = safe_numeric(H),
    Ustar = safe_numeric(Ustar),

    # CO2 концентрация
    CO2 = safe_numeric(co2_mole_fraction),
    WS = safe_numeric(wind_speed),

    # QC флаги
    qc_NEE = safe_numeric(qc_co2_flux),
    qc_LE = safe_numeric(qc_LE),
    qc_H = safe_numeric(qc_H)
  ) %>%
  # Применение QC флагов
  mutate(
    NEE = ifelse(qc_NEE == 2, NA, NEE),
    LE = ifelse(qc_LE == 2, NA, LE),
    H = ifelse(qc_H == 2, NA, H)
  ) %>%
  # Фильтрация выбросов
  mutate(
    NEE = ifelse(NEE < -50 | NEE > 50, NA, NEE),
    Ustar = ifelse(Ustar < 0 | Ustar > 5, NA, Ustar)
  ) %>%
  # Выбор нужных столбцов
  select(DateTime, NEE, LE, H, Ustar, WS, CO2) %>%
  filter(DateTime >= start_date & DateTime <= end_date) %>%
  distinct(DateTime, .keep_all = TRUE) %>%
  arrange(DateTime)

cat(paste("  ✓ Обработано", nrow(data_eddy), "записей EddyPro\n\n"))

# ==============================================================================
# 7. ПРЕОБРАЗОВАНИЕ ДАННЫХ BIOMET
# ==============================================================================

cat("Шаг 4: Обработка данных Biomet...\n")

# Преобразование Biomet данных
data_biomet <- biomet_data %>%
  mutate(
    # Создание DateTime из DATE_1 и TIME_1
    # Формат: DD.MM.YYYY и HH:MM:SS
    DateTime = dmy_hms(paste(DATE_1, TIME_1), tz = "UTC"),

    # Метеорологические переменные
    Tair = safe_numeric(AirTC_Avg),         # Температура воздуха (°C)
    RH = safe_numeric(RH),                  # Относительная влажность (%)

    # Радиация
    Rg = safe_numeric(SR01Dn_Avg),          # Глобальная радиация (W/m²)
    PPFD = safe_numeric(PAR_Den_Avg),       # PPFD (µmol/s/m²)
    Rn = safe_numeric(NetTot_Avg),          # Чистая радиация (W/m²)

    # Давление пара
    VPD_raw = safe_numeric(VP_kPa),         # Давление пара (kPa)
    SVP = safe_numeric(SVP_kPa),            # Насыщенное давление пара (kPa)

    # Температура почвы (усредним по всем датчикам)
    TSoil_1 = safe_numeric(TSoil_1_Avg),
    TSoil_2 = safe_numeric(TSoil_2_Avg),
    TSoil_3 = safe_numeric(TSoil_3_Avg),
    TSoil_4 = safe_numeric(TSoil_4_Avg),
    TSoil_5 = safe_numeric(TSoil_5_Avg),
    TSoil_6 = safe_numeric(TSoil_6_Avg),

    # Влажность почвы (усредним по всем датчикам)
    VWC_1 = safe_numeric(VWC_1_Avg),
    VWC_2 = safe_numeric(VWC_2_Avg),
    VWC_3 = safe_numeric(VWC_3_Avg),
    VWC_4 = safe_numeric(VWC_4_Avg),
    VWC_5 = safe_numeric(VWC_5_Avg),
    VWC_6 = safe_numeric(VWC_6_Avg),

    # Атмосферное давление (оценка, стандартное для Москвы)
    Pa = 101.3  # kPa, можно уточнить если есть данные барометра
  ) %>%
  # Расчет средних значений для почвы
  rowwise() %>%
  mutate(
    # Средняя температура почвы (все датчики)
    Tsoil = mean(c(TSoil_1, TSoil_2, TSoil_3, TSoil_4, TSoil_5, TSoil_6), na.rm = TRUE),

    # Средняя влажность почвы (все датчики)
    SWC = mean(c(VWC_1, VWC_2, VWC_3, VWC_4, VWC_5, VWC_6), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Расчет VPD из давления пара
  mutate(
    # VPD = SVP - VP, преобразуем kPa в hPa
    VPD = (SVP - VPD_raw) * 10  # kPa → hPa
  ) %>%
  # Выбор финальных столбцов
  select(DateTime, Tair, RH, Rg, PPFD, Rn, VPD, Pa, Tsoil, SWC,
         TSoil_1, TSoil_2, TSoil_3, TSoil_4, TSoil_5, TSoil_6,
         VWC_1, VWC_2, VWC_3, VWC_4, VWC_5, VWC_6) %>%
  filter(!is.na(DateTime)) %>%
  filter(DateTime >= start_date & DateTime <= end_date) %>%
  distinct(DateTime, .keep_all = TRUE) %>%
  arrange(DateTime)

cat(paste("  ✓ Обработано", nrow(data_biomet), "записей Biomet\n"))

# Статистика по переменным Biomet
biomet_stats <- data_biomet %>%
  summarise(
    across(c(Tair, RH, Rg, PPFD, Rn, VPD, Tsoil, SWC),
           list(
             available = ~sum(!is.na(.)),
             missing = ~sum(is.na(.))
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count") %>%
  separate(Variable, into = c("Var", "Stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Stat, values_from = Count) %>%
  mutate(
    Total = nrow(data_biomet),
    Percent_Available = round(available / Total * 100, 1)
  )

cat("\nДоступность данных Biomet:\n")
print(biomet_stats, n = Inf)
cat("\n")

# ==============================================================================
# 8. ОБЪЕДИНЕНИЕ ДАННЫХ
# ==============================================================================

cat("Шаг 5: Объединение данных EddyPro и Biomet...\n")

# Создание полной временной последовательности
full_time_seq <- data.frame(
  DateTime = seq(from = start_date, to = end_date, by = "30 min")
)

# Объединение всех данных
data_combined <- full_time_seq %>%
  left_join(data_eddy, by = "DateTime") %>%
  left_join(data_biomet, by = "DateTime") %>%
  mutate(
    Year = year(DateTime),
    DoY = yday(DateTime) + hour(DateTime)/24 + minute(DateTime)/(24*60)
  )

cat(paste("  ✓ Создана полная временная последовательность:", nrow(data_combined), "интервалов\n"))

# Статистика объединенных данных
combined_stats <- data_combined %>%
  select(NEE, LE, H, Ustar, Tair, RH, Rg, PPFD, Rn, VPD, Tsoil, SWC) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(
    Total = nrow(data_combined),
    Percent_Missing = round(Missing / Total * 100, 1),
    Available = Total - Missing
  ) %>%
  arrange(desc(Percent_Missing))

cat("\nСтатистика пропусков в объединенных данных:\n")
print(combined_stats, n = Inf)
cat("\n")

# ==============================================================================
# 9. ИНИЦИАЛИЗАЦИЯ REDDYPROC
# ==============================================================================

cat("Шаг 6: Инициализация REddyProc...\n")

# Подготовка данных для REddyProc
data_for_reddyproc <- data_combined %>%
  select(DateTime, NEE, LE, H, Rg, Tair, Tsoil, VPD, Ustar, PPFD, Rn) %>%
  as.data.frame()

# Создание объекта REddyProc
EProc <- sEddyProc$new(
  ID = site_name,
  Data = data_for_reddyproc,
  ColNames = c("NEE", "LE", "H", "Rg", "Tair", "Tsoil", "VPD", "Ustar", "PPFD", "Rn")
)

EProc$sSetLocationInfo(
  LatDeg = latitude,
  LongDeg = longitude,
  TimeZoneHour = timezone
)

cat("  ✓ REddyProc объект создан успешно\n\n")

# ==============================================================================
# 10. ЗАПОЛНЕНИЕ ПРОПУСКОВ МЕТОДОМ MDS
# ==============================================================================

cat("Шаг 7: Заполнение пропусков методом MDS...\n")
cat("  (Это может занять несколько минут)\n\n")

# Заполнение метеорологических переменных
cat("  - Заполнение Rg (глобальная радиация)...\n")
EProc$sMDSGapFill('Rg', FillAll = TRUE)

cat("  - Расчет потенциальной радиации...\n")
EProc$sCalcPotRadiation(useSolartime = TRUE)

cat("  - Заполнение PPFD (фотосинтетически активная радиация)...\n")
EProc$sMDSGapFill('PPFD', FillAll = TRUE)

cat("  - Заполнение Rn (чистая радиация)...\n")
EProc$sMDSGapFill('Rn', FillAll = TRUE)

cat("  - Заполнение Tsoil (температура почвы)...\n")
EProc$sMDSGapFill('Tsoil', FillAll = TRUE)

cat("  - Заполнение Tair (температура воздуха)...\n")
EProc$sMDSGapFill('Tair', FillAll = TRUE)

cat("  - Заполнение VPD (дефицит давления пара)...\n")
EProc$sMDSGapFill('VPD', FillAll = TRUE)

cat("  - Заполнение LE (латентный тепловой поток)...\n")
EProc$sMDSGapFill('LE', FillAll = TRUE)

cat("  - Заполнение H (явный тепловой поток)...\n")
EProc$sMDSGapFill('H', FillAll = TRUE)

cat("\n  ✓ Заполнение метеорологических переменных завершено!\n\n")

# ==============================================================================
# 11. ОЦЕНКА ПОРОГОВ U* И РАЗДЕЛЕНИЕ ПОТОКОВ
# ==============================================================================

cat("Шаг 8: Оценка пороговых значений u* и разделение потоков...\n")

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

# Заполнение NEE
cat("  - Заполнение NEE с фильтрацией по u*...\n")
EProc$sMDSGapFillUStarScens("NEE")

# Разделение потоков методом Lasslop
cat("  - Разделение потоков методом Lasslop (дневная регрессия)...\n")
EProc$sGLFluxPartitionUStarScens()

cat("  ✓ Разделение потоков завершено!\n\n")

# ==============================================================================
# 12. ИЗВЛЕЧЕНИЕ И СОХРАНЕНИЕ РЕЗУЛЬТАТОВ
# ==============================================================================

cat("Шаг 9: Извлечение и сохранение результатов...\n")

# Извлечение данных
filled_data <- EProc$sExportData()
results <- EProc$sExportResults()

# Объединение данных
final_data <- cbind(filled_data, results) %>%
  as_tibble() %>%
  rename(DateTime = sDateTime) %>%
  tidyr::fill(starts_with("FP_"), .direction = "downup")

# Добавление дополнительных переменных
final_data <- final_data %>%
  left_join(
    data_combined %>% select(DateTime, RH, Pa, WS, CO2, SWC,
                            TSoil_1, TSoil_2, TSoil_3, TSoil_4, TSoil_5, TSoil_6,
                            VWC_1, VWC_2, VWC_3, VWC_4, VWC_5, VWC_6),
    by = "DateTime"
  )

# Выбор важных столбцов для сохранения
output_data <- final_data %>%
  select(
    # Временные переменные
    DateTime, Year, DoY, season,

    # NEE (оригинал и заполненный)
    NEE_orig = NEE_uStar_orig,
    NEE_filled = NEE_uStar_f,
    NEE_fqc = NEE_uStar_fqc,
    NEE_fmeth = NEE_uStar_fmeth,

    # NEE для разных сценариев u*
    NEE_U05 = NEE_U05_f,
    NEE_U50 = NEE_U50_f,
    NEE_U95 = NEE_U95_f,

    # GPP и Reco (метод Lasslop)
    GPP = GPP_DT_uStar,
    GPP_SD = GPP_DT_uStar_SD,
    GPP_U05 = GPP_DT_U05,
    GPP_U50 = GPP_DT_U50,
    GPP_U95 = GPP_DT_U95,

    Reco = Reco_DT_uStar,
    Reco_SD = Reco_DT_uStar_SD,
    Reco_U05 = Reco_DT_U05,
    Reco_U50 = Reco_DT_U50,
    Reco_U95 = Reco_DT_U95,

    # Метеорологические переменные (заполненные)
    Tair = Tair_f,
    Tair_qc = Tair_fqc,
    Tsoil = Tsoil_f,
    Tsoil_qc = Tsoil_fqc,

    VPD = VPD_f,
    VPD_qc = VPD_fqc,

    Rg = Rg_f,
    Rg_qc = Rg_fqc,

    PPFD = PPFD_f,
    PPFD_qc = PPFD_fqc,

    Rn = Rn_f,
    Rn_qc = Rn_fqc,

    LE = LE_f,
    LE_qc = LE_fqc,

    H = H_f,
    H_qc = H_fqc,

    # Дополнительные переменные
    RH, Pa, WS, CO2, SWC,

    # Температура почвы по датчикам
    TSoil_1, TSoil_2, TSoil_3, TSoil_4, TSoil_5, TSoil_6,

    # Влажность почвы по датчикам
    VWC_1, VWC_2, VWC_3, VWC_4, VWC_5, VWC_6,

    # Пороги u*
    Ustar_Thresh = Ustar_uStar_Thres,

    # Параметры модели Lasslop
    FP_alpha,      # Начальный наклон световой кривой
    FP_beta,       # Максимальный GPP
    FP_k,          # Параметр влияния VPD
    FP_RRef,       # Базальное дыхание
    FP_E0,         # Энергия активации
    FP_GPP2000,    # GPP при PPFD = 2000
    FP_qc          # QC флаг
  )

# Сохранение
write_csv(output_data, output_file)

cat(paste("  ✓ Результаты сохранены в:", output_file, "\n"))
cat(paste("  ✓ Сохранено", nrow(output_data), "строк и", ncol(output_data), "столбцов\n\n"))

# ==============================================================================
# 13. ИТОГОВАЯ СТАТИСТИКА
# ==============================================================================

cat("========================================================================\n")
cat("ОБРАБОТКА ЗАВЕРШЕНА УСПЕШНО!\n")
cat("========================================================================\n\n")

# Статистика заполнения
final_stats <- output_data %>%
  select(NEE_filled, GPP, Reco, Tair, VPD, Rg, PPFD, Rn, LE, H, Tsoil, SWC) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(
    Total = nrow(output_data),
    Percent_Missing = round(Missing / Total * 100, 1),
    Available = Total - Missing
  ) %>%
  arrange(Percent_Missing)

cat("Итоговая статистика заполнения:\n")
print(final_stats, n = Inf)
cat("\n")

# Кумулятивные потоки
cumulative <- output_data %>%
  summarise(
    Total_GPP = sum(GPP * 30 * 60 / 1e6, na.rm = TRUE),  # mol CO2 m-2
    Total_Reco = sum(Reco * 30 * 60 / 1e6, na.rm = TRUE),
    Total_NEE = sum(NEE_filled * 30 * 60 / 1e6, na.rm = TRUE)
  )

cat("Кумулятивные потоки за период (mol CO₂ m⁻²):\n")
cat(sprintf("  GPP (валовая продукция):      %8.2f mol CO₂ m⁻²\n", cumulative$Total_GPP))
cat(sprintf("  Reco (дыхание):               %8.2f mol CO₂ m⁻²\n", cumulative$Total_Reco))
cat(sprintf("  NEE (чистый обмен):           %8.2f mol CO₂ m⁻²\n", cumulative$Total_NEE))
cat("\n")

cat("Файл результатов:", output_file, "\n")
cat("✓ Использованы данные Biomet с точными значениями радиации и температуры\n")
cat("✓ Все пропуски заполнены методом MDS\n")
cat("✓ Потоки разделены методом Lasslop: NEE → GPP + Reco\n\n")

cat("========================================================================\n")
cat("Для визуализации результатов запустите:\n")
cat("  source('Create_Plots_2013.R')  # (если есть)\n")
cat("========================================================================\n\n")
