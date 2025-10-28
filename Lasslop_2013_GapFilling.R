################################################################################
# РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP С ЗАПОЛНЕНИЕМ ПРОПУСКОВ
# Данные: Eddy Covariance 2013 год (Москва, RU-Mos)
#
# Этот скрипт выполняет:
# 1. Загрузку данных из eddypro_2013.csv
# 2. Заполнение пропусков методом MDS для всех переменных (NEE, Tair, LE, H, Rg, Rn, VPD и др.)
# 3. Разделение потоков методом Lasslop (дневная регрессия): NEE → GPP + Reco
# 4. Оценку пороговых значений u* и фильтрацию данных
# 5. Сохранение полного набора данных с заполненными пропусками
#
# Метод Lasslop (Lasslop et al., 2010):
# - Использует дневную регрессию по световому отклику
# - Учитывает влияние температуры и VPD на фотосинтез
# - Более точен для дневного времени, чем метод Райхштайна
#
# Автор: Claude Code
# Дата: 2025-10-28
################################################################################

# ==============================================================================
# 1. УСТАНОВКА И ЗАГРУЗКА ПАКЕТОВ
# ==============================================================================

# Проверка и установка необходимых пакетов
required_packages <- c("REddyProc", "tidyverse", "lubridate", "stringr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("Все необходимые пакеты загружены успешно!\n\n")

# ==============================================================================
# 2. НАСТРОЙКИ И ПАРАМЕТРЫ
# ==============================================================================

# Пути к файлам
input_file <- "eddypro_2013.csv"
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
season_starts <- c(60, 152, 244)  # День года начала сезонов (март, июнь, сентябрь)
ustar_probs <- c(0.05, 0.5, 0.95)  # Вероятности для сценариев u*

# Создание директории для графиков
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
  cat(paste("Создана директория для графиков:", plots_dir, "\n"))
}

cat("========================================================================\n")
cat("РАЗДЕЛЕНИЕ ПОТОКОВ CO2 МЕТОДОМ LASSLOP - 2013 ГОД\n")
cat("========================================================================\n")
cat(paste("Входной файл:", input_file, "\n"))
cat(paste("Выходной файл:", output_file, "\n"))
cat(paste("Период:", format(start_date, "%Y-%m-%d"), "до", format(end_date, "%Y-%m-%d"), "\n"))
cat(paste("Местоположение:", site_name, sprintf("(%.4f°N, %.4f°E)", latitude, longitude), "\n"))
cat("========================================================================\n\n")

# ==============================================================================
# 3. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ
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
  locale = locale(decimal_mark = ".")
)

cat(paste("  Загружено", nrow(eddy_data), "строк данных\n\n"))

# ==============================================================================
# 4. ПРЕОБРАЗОВАНИЕ И ФИЛЬТРАЦИЯ ДАННЫХ
# ==============================================================================

cat("Шаг 2: Преобразование и очистка данных...\n")

# Преобразование типов данных и создание DateTime
data_processed <- eddy_data %>%
  mutate(
    # Создание временной метки
    date = as.Date(date),
    time = substr(time, 1, 5),  # Берем только HH:MM
    DateTime = ymd_hm(paste(date, time)),

    # Преобразование числовых переменных
    NEE = as.numeric(co2_flux),
    LE = as.numeric(LE),
    H = as.numeric(H),
    Ustar = as.numeric(Ustar),
    Tair = as.numeric(air_temperature),
    VPD = as.numeric(VPD) / 100,  # Преобразование из Pa в hPa
    Rg = as.numeric(PPFD_IN) * 0.5,  # Преобразование PPFD в Rg (примерно)
    PPFD = as.numeric(PPFD_IN),
    Rn = as.numeric(NETRAD),
    RH = as.numeric(RH),
    Pa = as.numeric(air_pressure),
    Tsoil = as.numeric(TS_1),  # Температура почвы

    # QC флаги
    qc_NEE = as.numeric(qc_co2_flux),
    qc_LE = as.numeric(qc_LE),
    qc_H = as.numeric(qc_H)
  ) %>%
  # Фильтрация по QC флагам (2 = плохие данные)
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
  # Выбор нужных столбцов
  select(DateTime, NEE, LE, H, Ustar, Tair, VPD, Rg, PPFD, Rn, RH, Pa, Tsoil) %>%
  # Удаление дубликатов по времени
  distinct(DateTime, .keep_all = TRUE) %>%
  # Сортировка по времени
  arrange(DateTime) %>%
  # Фильтрация по периоду
  filter(DateTime >= start_date & DateTime <= end_date)

# Проверка наличия пропусков
missing_counts <- data_processed %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(Percent = round(Missing / nrow(data_processed) * 100, 1)) %>%
  arrange(desc(Missing))

cat("\nПропуски в данных ДО заполнения:\n")
print(missing_counts, n = Inf)
cat("\n")

# ==============================================================================
# 5. СОЗДАНИЕ ПОЛНОЙ ВРЕМЕННОЙ ПОСЛЕДОВАТЕЛЬНОСТИ
# ==============================================================================

cat("Шаг 3: Создание полной временной последовательности (30-минутные интервалы)...\n")

# Создание полной последовательности 30-минутных интервалов
full_time_seq <- data.frame(
  DateTime = seq(
    from = start_date,
    to = end_date,
    by = "30 min"
  )
)

# Объединение с данными
data_full <- full_time_seq %>%
  left_join(data_processed, by = "DateTime") %>%
  mutate(
    Year = year(DateTime),
    DoY = yday(DateTime) + hour(DateTime)/24 + minute(DateTime)/(24*60)
  )

cat(paste("  Всего временных интервалов:", nrow(data_full), "\n"))
cat(paste("  Интервалов с данными:", sum(!is.na(data_full$NEE)), "\n"))
cat(paste("  Интервалов с пропусками NEE:", sum(is.na(data_full$NEE)), "\n\n"))

# ==============================================================================
# 6. ИНИЦИАЛИЗАЦИЯ REDDYPROC
# ==============================================================================

cat("Шаг 4: Инициализация REddyProc...\n")

# Подготовка данных для REddyProc
data_for_reddyproc <- data_full %>%
  select(DateTime, NEE, LE, H, Rg, Tair, Tsoil, VPD, Ustar, PPFD, Rn) %>%
  as.data.frame()

# Создание объекта sEddyProc
EProc <- sEddyProc$new(
  ID = site_name,
  Data = data_for_reddyproc,
  ColNames = c("NEE", "LE", "H", "Rg", "Tair", "Tsoil", "VPD", "Ustar", "PPFD", "Rn")
)

# Установка информации о местоположении
EProc$sSetLocationInfo(
  LatDeg = latitude,
  LongDeg = longitude,
  TimeZoneHour = timezone
)

cat("  REddyProc объект создан успешно\n\n")

# ==============================================================================
# 7. ЗАПОЛНЕНИЕ ПРОПУСКОВ МЕТОДОМ MDS
# ==============================================================================

cat("Шаг 5: Заполнение пропусков методом MDS (Marginal Distribution Sampling)...\n")
cat("  Это может занять несколько минут...\n\n")

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

cat("\n  Заполнение метеорологических переменных завершено!\n\n")

# ==============================================================================
# 8. ОЦЕНКА ПОРОГОВ U* И СОЗДАНИЕ СЦЕНАРИЕВ
# ==============================================================================

cat("Шаг 6: Оценка пороговых значений u* (скорость трения)...\n")

# Определение сезонных факторов
season_starts_df <- data.frame(
  V1 = season_starts,
  V2 = 2013
)

season_factor <- usCreateSeasonFactorYdayYear(
  data_for_reddyproc$DateTime - 15*60,
  starts = season_starts_df
)

# Установка сезонных факторов
EProc$sSetUStarSeasons(season_factor)

# Оценка u* сценариев
cat("  - Расчет пороговых значений для сценариев (5%, 50%, 95%)...\n")
EProc$sEstimateUstarScenarios(
  seasonFactor = season_factor,
  nSample = 100L,
  probs = ustar_probs
)

# Получение результатов оценки u*
ustar_thresholds <- EProc$sGetUstarScenarios()
cat("\n  Пороговые значения u*:\n")
print(ustar_thresholds)
cat("\n")

# ==============================================================================
# 9. ЗАПОЛНЕНИЕ ПРОПУСКОВ NEE С УЧЕТОМ U*
# ==============================================================================

cat("Шаг 7: Заполнение пропусков NEE с фильтрацией по u*...\n")

# Заполнение NEE с использованием сценариев u*
EProc$sMDSGapFillUStarScens("NEE")

cat("  Заполнение NEE завершено!\n\n")

# ==============================================================================
# 10. РАЗДЕЛЕНИЕ ПОТОКОВ МЕТОДОМ LASSLOP
# ==============================================================================

cat("========================================================================\n")
cat("Шаг 8: РАЗДЕЛЕНИЕ ПОТОКОВ МЕТОДОМ LASSLOP\n")
cat("========================================================================\n")
cat("Метод Lasslop (дневная регрессия по световому отклику):\n")
cat("  - Использует модель прямоугольной гиперболы\n")
cat("  - Учитывает влияние VPD на фотосинтез\n")
cat("  - Оптимален для дневных условий\n")
cat("  - Параметры: α (начальный наклон), β (максимальный GPP),\n")
cat("               k (влияние VPD), RRef (базальное дыхание)\n\n")

# Применение метода Lasslop (дневная регрессия)
cat("  Выполнение разделения потоков...\n")
EProc$sGLFluxPartitionUStarScens()

cat("  Разделение потоков методом Lasslop завершено!\n\n")

# ==============================================================================
# 11. ИЗВЛЕЧЕНИЕ РЕЗУЛЬТАТОВ
# ==============================================================================

cat("Шаг 9: Извлечение и объединение результатов...\n")

# Извлечение данных
filled_data <- EProc$sExportData()
results <- EProc$sExportResults()

# Проверка имен столбцов
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

# Заполнение параметров разделения потоков
final_data <- final_data %>%
  tidyr::fill(starts_with("FP_"), .direction = "downup")

# Добавление дополнительных переменных
final_data <- final_data %>%
  left_join(
    data_full %>% select(DateTime, Year, DoY, RH, Pa),
    by = "DateTime"
  )

# Статистика заполнения
missing_after <- final_data %>%
  select(any_of(c("NEE_uStar_f", "GPP_DT_uStar", "Reco_DT_uStar", "Tair_f", "LE_f", "H_f", "VPD_f", "Rg_f", "Rn_f"))) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  mutate(Percent = round(Missing / nrow(final_data) * 100, 1))

cat("\nПропуски в данных ПОСЛЕ заполнения:\n")
print(missing_after, n = Inf)
cat("\n")

# ==============================================================================
# 12. СОХРАНЕНИЕ РЕЗУЛЬТАТОВ
# ==============================================================================

cat("Шаг 10: Сохранение результатов...\n")

# Выбор и переименование столбцов для сохранения
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

      # Метеорологические переменные (заполненные)
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

      # Пороги u*
      Ustar_Thresh = "Ustar_uStar_Thres"
    )),

    # Дополнительные переменные
    any_of(c("RH", "Pa")),

    # Параметры модели Lasslop
    any_of(c("FP_alpha", "FP_beta", "FP_k", "FP_RRef", "FP_E0", "FP_GPP2000", "FP_qc"))
  )

# Сохранение в CSV
write_csv(output_data, output_file)

cat(paste("  Результаты сохранены в:", output_file, "\n"))
cat(paste("  Сохранено", nrow(output_data), "строк и", ncol(output_data), "столбцов\n\n"))

# ==============================================================================
# 13. СОЗДАНИЕ ДИАГНОСТИЧЕСКИХ ГРАФИКОВ
# ==============================================================================

cat("Шаг 11: Создание диагностических графиков...\n")

# График 1: Временной ряд GPP, Reco и NEE
cat("  - График временных рядов потоков...\n")

p1 <- output_data %>%
  select(DateTime, NEE_filled, GPP, Reco) %>%
  pivot_longer(cols = c(NEE_filled, GPP, Reco),
               names_to = "Flux",
               values_to = "Value") %>%
  mutate(Flux = factor(Flux,
                       levels = c("GPP", "Reco", "NEE_filled"),
                       labels = c("GPP (поглощение)", "Reco (дыхание)", "NEE (чистый поток)"))) %>%
  ggplot(aes(x = DateTime, y = Value, color = Flux)) +
  geom_line(alpha = 0.6, size = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c("GPP (поглощение)" = "#2E7D32",
                                 "Reco (дыхание)" = "#D32F2F",
                                 "NEE (чистый поток)" = "#1976D2")) +
  labs(
    title = "Временные ряды потоков CO₂ (метод Lasslop, 2013)",
    subtitle = "С заполнением пропусков методом MDS",
    x = "Дата",
    y = expression(paste("Поток CO"[2], " (µmol m"^-2, " s"^-1, ")")),
    color = "Тип потока"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(plots_dir, "01_Flux_TimeSeries.png"),
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300
)

# График 2: Суточный цикл
cat("  - График суточного цикла...\n")

p2 <- output_data %>%
  mutate(Hour = hour(DateTime) + minute(DateTime)/60) %>%
  select(Hour, NEE_filled, GPP, Reco) %>%
  pivot_longer(cols = c(NEE_filled, GPP, Reco),
               names_to = "Flux",
               values_to = "Value") %>%
  mutate(Flux = factor(Flux,
                       levels = c("GPP", "Reco", "NEE_filled"),
                       labels = c("GPP", "Reco", "NEE"))) %>%
  group_by(Hour, Flux) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Hour, y = Mean, color = Flux, fill = Flux)) +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("GPP" = "#2E7D32",
                                 "Reco" = "#D32F2F",
                                 "NEE" = "#1976D2")) +
  scale_fill_manual(values = c("GPP" = "#2E7D32",
                                "Reco" = "#D32F2F",
                                "NEE" = "#1976D2")) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(
    title = "Средний суточный цикл потоков CO₂",
    subtitle = "Метод Lasslop, 2013 год",
    x = "Час суток",
    y = expression(paste("Поток CO"[2], " (µmol m"^-2, " s"^-1, ")")),
    color = "Тип потока",
    fill = "Тип потока"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(plots_dir, "02_Diurnal_Cycle.png"),
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

# График 3: Световая кривая (GPP vs PPFD)
cat("  - График световой кривой...\n")

p3 <- output_data %>%
  filter(!is.na(GPP) & !is.na(PPFD) & PPFD > 0) %>%
  ggplot(aes(x = PPFD, y = GPP)) +
  geom_hex(bins = 50) +
  geom_smooth(method = "nls",
              formula = y ~ a * x / (b + x),
              method.args = list(start = list(a = 20, b = 500)),
              se = TRUE,
              color = "red",
              size = 1.2) +
  scale_fill_viridis_c(option = "plasma", trans = "log10") +
  labs(
    title = "Световая кривая: GPP vs PPFD",
    subtitle = "Модель прямоугольной гиперболы (метод Lasslop)",
    x = expression(paste("PPFD (µmol photons m"^-2, " s"^-1, ")")),
    y = expression(paste("GPP (µmol CO"[2], " m"^-2, " s"^-1, ")")),
    fill = "Частота"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(plots_dir, "03_Light_Response_Curve.png"),
  plot = p3,
  width = 10,
  height = 7,
  dpi = 300
)

# График 4: Reco vs Температура
cat("  - График зависимости Reco от температуры...\n")

p4 <- output_data %>%
  filter(!is.na(Reco) & !is.na(Tair)) %>%
  ggplot(aes(x = Tair, y = Reco)) +
  geom_hex(bins = 50) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = TRUE,
              color = "red",
              size = 1.2) +
  scale_fill_viridis_c(option = "inferno", trans = "log10") +
  labs(
    title = "Зависимость экосистемного дыхания от температуры",
    subtitle = "Метод Lasslop, 2013 год",
    x = "Температура воздуха (°C)",
    y = expression(paste("Reco (µmol CO"[2], " m"^-2, " s"^-1, ")")),
    fill = "Частота"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(plots_dir, "04_Reco_Temperature.png"),
  plot = p4,
  width = 10,
  height = 7,
  dpi = 300
)

cat(paste("  Графики сохранены в директории:", plots_dir, "\n\n"))

# ==============================================================================
# 14. СВОДНАЯ СТАТИСТИКА
# ==============================================================================

cat("========================================================================\n")
cat("СВОДНАЯ СТАТИСТИКА\n")
cat("========================================================================\n\n")

# Статистика по потокам
flux_stats <- output_data %>%
  summarise(
    across(
      c(NEE_filled, GPP, Reco, Tair, VPD, Rg, LE, H),
      list(
        Min = ~min(., na.rm = TRUE),
        Mean = ~mean(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE),
        NA_count = ~sum(is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = "Stat", values_to = "Value") %>%
  separate(Stat, into = c("Variable", "Metric"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Metric, values_from = Value)

cat("Статистика по основным переменным:\n")
print(flux_stats, n = Inf)
cat("\n")

# Кумулятивные потоки
cumulative <- output_data %>%
  summarise(
    Total_GPP = sum(GPP * 30 * 60 / 1e6, na.rm = TRUE),  # в mol CO2 m-2
    Total_Reco = sum(Reco * 30 * 60 / 1e6, na.rm = TRUE),
    Total_NEE = sum(NEE_filled * 30 * 60 / 1e6, na.rm = TRUE)
  )

cat("Кумулятивные потоки за период (mol CO₂ m⁻²):\n")
cat(sprintf("  GPP (валовая продукция):      %8.2f mol CO₂ m⁻²\n", cumulative$Total_GPP))
cat(sprintf("  Reco (дыхание):               %8.2f mol CO₂ m⁻²\n", cumulative$Total_Reco))
cat(sprintf("  NEE (чистый обмен):           %8.2f mol CO₂ m⁻²\n", cumulative$Total_NEE))
cat("\n")

# Параметры модели Lasslop
model_params <- output_data %>%
  filter(!is.na(FP_alpha)) %>%
  summarise(
    alpha_mean = mean(FP_alpha, na.rm = TRUE),
    beta_mean = mean(FP_beta, na.rm = TRUE),
    k_mean = mean(FP_k, na.rm = TRUE),
    RRef_mean = mean(FP_RRef, na.rm = TRUE),
    E0_mean = mean(FP_E0, na.rm = TRUE)
  )

cat("Средние параметры модели Lasslop:\n")
cat(sprintf("  α (начальный наклон):         %.4f\n", model_params$alpha_mean))
cat(sprintf("  β (максимальный GPP):         %.4f\n", model_params$beta_mean))
cat(sprintf("  k (влияние VPD):              %.4f\n", model_params$k_mean))
cat(sprintf("  RRef (базальное дыхание):     %.4f\n", model_params$RRef_mean))
cat(sprintf("  E0 (энергия активации):       %.4f\n", model_params$E0_mean))
cat("\n")

cat("========================================================================\n")
cat("ОБРАБОТКА ЗАВЕРШЕНА УСПЕШНО!\n")
cat("========================================================================\n\n")
cat("Файлы результатов:\n")
cat(paste("  1. Данные:", output_file, "\n"))
cat(paste("  2. Графики:", plots_dir, "/\n\n"))

cat("Метод Lasslop успешно применен для разделения потоков CO₂\n")
cat("Все пропуски в данных заполнены методом MDS\n\n")

cat("Для дальнейшего анализа используйте:\n")
cat("  - NEE_filled: заполненный чистый обмен CO₂\n")
cat("  - GPP: валовая первичная продукция (поглощение)\n")
cat("  - Reco: экосистемное дыхание (эмиссия)\n")
cat("  - *_U05, *_U50, *_U95: сценарии с разными порогами u*\n\n")

cat("========================================================================\n")
