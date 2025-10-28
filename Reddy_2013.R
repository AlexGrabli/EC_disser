# --------------------------------------------------------------------------
# Анализ данных Eddy Covariance с использованием пакета REddyProc
# Адаптировано для данных 2013 года на основе скрипта для 2016 года.
# Оставлены методы разделения Райхштайна и Ласслоп.
# Обновлено: учтены имена столбцов DATE_1, TIME_1 и новый диапазон дат.
# --------------------------------------------------------------------------

# 1. Установка и загрузка необходимых пакетов
# --------------------------------------------------------------------------
# Убедитесь, что все пакеты установлены. Если нет, раскомментируйте следующую строку:
install.packages(c("REddyProc", "tidyverse", "lubridate", "bigleaf", "stringr", "ggplot2", "ggpubr"))

library(REddyProc)
library(tidyverse) # Включает ggplot2, dplyr, readr, tidyr и др.
library(lubridate)
library(tidyr) # Уже включен в tidyverse
library(bigleaf)
library(stringr)
library(ggpubr) # Для расширенных возможностей ggplot2

# --------------------------------------------------------------------------
# 2. Определение путей к файлам и основных параметров
# --------------------------------------------------------------------------
eddypro_file_path <- "eddypro_2013.csv"
meteo_file_path <- "biomet2013_full.csv"
output_csv_path <- "Moscow_REddyProc_Processed_Data_2013.csv" # Обновлено имя выходного файла
plots_output_dir <- "REddyProc_Plots_2013" # Обновлена папка для графиков

# Информация о местоположении (Москва)
site_id <- 'RU-Mos'
lat_deg <- 55 + 50/60 + 14/3600  # 55.83722 N
long_deg <- 37 + 33/60 + 56/3600 # 37.56556 E
timezone_h <- 3  # MSK = UTC+3

common_start_datetime_utc <- as.POSIXct("2013-04-02 00:30:00", tz = "UTC") # Конец первого интервала
common_end_datetime_utc <- as.POSIXct("2013-09-02 00:00:00", tz = "UTC")   # Конец последнего интервала
current_year <- year(common_start_datetime_utc) 

season_start_days <- c(60, 152, 244) 
ustar_probabilities <- c(0.05, 0.5, 0.95)
use_Lasslop_partitioning <- TRUE 

if (!dir.exists(plots_output_dir)) {
  dir.create(plots_output_dir)
}

# --------------------------------------------------------------------------
# 3. Загрузка данных
# --------------------------------------------------------------------------
message("Загрузка данных для ", current_year, " года (диапазон: ", 
        format(common_start_datetime_utc - minutes(30), "%Y-%m-%d %H:%M"), " до ", 
        format(common_end_datetime_utc, "%Y-%m-%d %H:%M"), ")...")

# --- Загрузка данных EddyPro ---
message("Считывание заголовков для данных EddyPro...")
header_line_eddy <- tryCatch({
  readLines(eddypro_file_path, n = 1, warn = FALSE)[1]
}, error = function(e) {
  stop(paste("Не удалось прочитать первую строку из файла:", eddypro_file_path, ". Ошибка:", e$message))
})
col_names_eddy <- str_trim(gsub('^"|"$', '', strsplit(header_line_eddy, ",")[[1]]))
col_names_eddy <- make.names(col_names_eddy, unique = TRUE)
message("Извлеченные и очищенные заголовки для EddyPro: ", paste(col_names_eddy, collapse=", "))

eddy_data_raw <- read_delim(eddypro_file_path, 
                          delim = ",", 
                          comment = "#", 
                          skip = 2,      
                          col_names = col_names_eddy,
                          na = c("-9999", "-9999.0", "NA", "", "--", "-"), 
                          col_types = cols(.default = "c"), 
                          locale = locale(decimal_mark = ".")) 

message("--- ДИАГНОСТИКА eddy_data_raw ---")
message("Размеры eddy_data_raw: ", paste(dim(eddy_data_raw), collapse=" x "))
message("Имена столбцов в eddy_data_raw СРАЗУ ПОСЛЕ СЧИТЫВАНИЯ:")
print(names(eddy_data_raw))
message("--- КОНЕЦ ДИАГНОСТИКИ eddy_data_raw ---")


# --- Загрузка метеорологических данных ---
message("Считывание заголовков для метеоданных...")
header_line_meteo <- tryCatch({
  readLines(meteo_file_path, n = 1, warn = FALSE)[1]
}, error = function(e) {
  stop(paste("Не удалось прочитать первую строку из файла:", meteo_file_path, ". Ошибка:", e$message))
})
col_names_meteo <- str_trim(gsub('^"|"$', '', strsplit(header_line_meteo, ",")[[1]])) 
col_names_meteo <- make.names(col_names_meteo, unique = TRUE)
message("Извлеченные и очищенные заголовки для метеоданных: ", paste(col_names_meteo, collapse=", "))

meteo_data_raw <- read_delim(meteo_file_path, 
                           delim = ",", 
                           comment = "#", 
                           skip = 2,    
                           col_names = col_names_meteo,
                           na = c("-9999", "-9999.0", "NA", "NAN", "", "--", "-"), 
                           col_types = cols(.default = "c"),
                           locale = locale(decimal_mark = "."))

message("--- ДИАГНОСТИКА meteo_data_raw ---")
message("Размеры meteo_data_raw: ", paste(dim(meteo_data_raw), collapse=" x "))
message("Имена столбцов в meteo_data_raw СРАЗУ ПОСЛЕ СЧИТЫВАНИЯ:")
print(names(meteo_data_raw))
message("--- КОНЕЦ ДИАГНОСТИКИ meteo_data_raw ---")


# Удаление полностью пустых строк
eddy_data_raw <- eddy_data_raw[rowSums(is.na(eddy_data_raw)) != ncol(eddy_data_raw), ]
meteo_data_raw <- meteo_data_raw[rowSums(is.na(meteo_data_raw)) != ncol(meteo_data_raw), ]

if (!make.names("Ustar") %in% names(eddy_data_raw)) {
  stop("Столбец 'Ustar' (или его make.names версия) не найден в eddy_data_raw. Проверьте вывод 'Имена столбцов в eddy_data_raw'.")
}
if (!make.names("date") %in% names(eddy_data_raw) || !make.names("time") %in% names(eddy_data_raw)) {
    stop("Столбцы 'date' и/или 'time' (или их make.names версии) не найдены в eddy_data_raw.")
}


# --------------------------------------------------------------------------
# 4. Предварительная обработка и слияние данных
# --------------------------------------------------------------------------
message("Предварительная обработка данных...")

# Обработка данных EddyPro
message("Обработка данных EddyPro...")

nee_col_original_name_csv <- NULL 
potential_nee_names_csv <- c("co2_flux", "NEE", "eficiente_co2_flux") 
nee_col_in_df <- NULL
for (name_candidate_csv in potential_nee_names_csv) {
    if (make.names(name_candidate_csv) %in% names(eddy_data_raw)) {
        nee_col_original_name_csv <- name_candidate_csv 
        nee_col_in_df <- make.names(name_candidate_csv) 
        break
    }
}
if (is.null(nee_col_original_name_csv)) {
    stop(paste("Столбец для NEE не найден. Проверенные варианты (после make.names):", paste(make.names(potential_nee_names_csv), collapse=", "), ". Текущие имена в eddy_data_raw:", paste(names(eddy_data_raw)[1:min(10, ncol(eddy_data_raw))], collapse=", "), "..."))
}
message(paste("Используемый столбец для NEE (в датафрейме):", nee_col_in_df, "(оригинал в CSV:", nee_col_original_name_csv, ")"))


qc_nee_col_original_name_csv <- NULL
potential_qc_nee_names_csv <- c(paste0("qc_", nee_col_original_name_csv), "qc_NEE", "qc_co2_flux")
qc_nee_col_in_df <- NULL
for (name_candidate_csv in potential_qc_nee_names_csv) {
    if (make.names(name_candidate_csv) %in% names(eddy_data_raw)) {
        qc_nee_col_original_name_csv <- name_candidate_csv
        qc_nee_col_in_df <- make.names(name_candidate_csv)
        break
    }
}
if (is.null(qc_nee_col_original_name_csv)) {
    warning("Столбец для qc_NEE не найден. Фильтрация по качеству NEE может не работать. Будет создан фиктивный столбец qc_NEE_placeholder с нулями.")
    placeholder_qc_name <- make.names("qc_NEE_placeholder")
    eddy_data_raw[[placeholder_qc_name]] <- 0 
    qc_nee_col_in_df <- placeholder_qc_name
}
message(paste("Используемый столбец для qc_NEE (в датафрейме):", qc_nee_col_in_df))

date_col_in_df <- make.names("date")
time_col_in_df <- make.names("time")

eddy_numeric_cols_to_check_original_csv <- c(nee_col_original_name_csv, "H", "LE", "Ustar", qc_nee_col_original_name_csv, "qc_H", "qc_LE")
eddy_numeric_cols_to_check_made_names <- make.names(eddy_numeric_cols_to_check_original_csv)
eddy_numeric_cols <- intersect(eddy_numeric_cols_to_check_made_names, names(eddy_data_raw))

message("Столбцы, которые будут преобразованы в числовой тип в eddy_data_raw (после make.names):", paste(eddy_numeric_cols, collapse=", "))

eddy_data_processed <- eddy_data_raw %>%
  mutate(
    across(all_of(eddy_numeric_cols), 
           ~suppressWarnings(as.numeric(as.character(.)))),
    DateTime = ymd_hm(paste(!!sym(date_col_in_df), !!sym(time_col_in_df)), tz = "UTC", quiet = FALSE) 
  ) %>%
  filter(!is.na(DateTime), DateTime >= (common_start_datetime_utc - minutes(29)), DateTime <= common_end_datetime_utc) # Фильтр по диапазону

H_col_in_df <- make.names("H")
LE_col_in_df <- make.names("LE")
Ustar_col_in_df <- make.names("Ustar")
qc_H_col_in_df <- make.names("qc_H")
qc_LE_col_in_df <- make.names("qc_LE")

eddy_data_processed <- eddy_data_processed %>%
  select( 
    DateTime,
    NEE = if(nee_col_in_df %in% names(.)) all_of(nee_col_in_df) else NA_real_,
    H = if(H_col_in_df %in% names(.)) all_of(H_col_in_df) else NA_real_, 
    LE = if(LE_col_in_df %in% names(.)) all_of(LE_col_in_df) else NA_real_, 
    Ustar = if(Ustar_col_in_df %in% names(.)) all_of(Ustar_col_in_df) else NA_real_, 
    qc_NEE = if(qc_nee_col_in_df %in% names(.)) all_of(qc_nee_col_in_df) else NA_integer_, 
    qc_H = if(qc_H_col_in_df %in% names(.)) all_of(qc_H_col_in_df) else 0L,     
    qc_LE = if(qc_LE_col_in_df %in% names(.)) all_of(qc_LE_col_in_df) else 0L      
  ) %>%
  mutate( 
    NEE = ifelse(!is.na(qc_NEE) & qc_NEE == 2, NA, NEE),
    H = ifelse(!is.na(qc_H) & qc_H == 2, NA, H),
    LE = ifelse(!is.na(qc_LE) & qc_LE == 2, NA, LE)
  ) %>%
  distinct(DateTime, .keep_all = TRUE) # Убедимся в уникальности DateTime

message(paste("Размер eddy_data_processed после обработки:", nrow(eddy_data_processed), "строк"))


# Обработка метеорологических данных
message("Обработка метеоданных...")

meteo_date_col_in_df <- make.names("DATE_1")
meteo_time_col_in_df <- make.names("TIME_1")

if (all(c(meteo_date_col_in_df, meteo_time_col_in_df) %in% names(meteo_data_raw))) {
  date_str <- as.character(meteo_data_raw[[meteo_date_col_in_df]])
  time_str <- as.character(meteo_data_raw[[meteo_time_col_in_df]])
  datetime_str <- paste(date_str, time_str)
  
  combined_datetime_formats <- c(
      "Ymd HMS", "Ymd HM",   
      "dmY HMS", "dmY HM",   
      "mdY HMS", "mdY HM",   
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
      "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M",
      "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M"
  )
  
  DateTime_interval_start <- parse_date_time(datetime_str, orders = combined_datetime_formats, tz = "UTC", quiet = TRUE)

  meteo_data_processed <- meteo_data_raw %>%
    mutate(
      DateTime_interval_start = DateTime_interval_start,
      DateTime = DateTime_interval_start + minutes(30) 
    ) %>%
    filter(!is.na(DateTime), DateTime >= (common_start_datetime_utc - minutes(29)), DateTime <= common_end_datetime_utc)

    convert_and_check_numeric <- function(df, col_name_original_csv, col_name_target) {
        col_name_in_df <- make.names(col_name_original_csv) 
        if (col_name_in_df %in% names(df)) {
            char_vector <- as.character(df[[col_name_in_df]])
            original_na_mask <- is.na(char_vector) | char_vector %in% c("", "-9999", "-9999.0", "NA", "NAN", "--", "-")
            numeric_vector <- suppressWarnings(as.numeric(char_vector))
            new_na_mask <- is.na(numeric_vector)
            unexpected_na_introduced <- any(new_na_mask & !original_na_mask)

            if (unexpected_na_introduced) {
                problematic_values <- char_vector[new_na_mask & !original_na_mask]
                warning(paste("При преобразовании столбца '", col_name_in_df, "' (оригинал: '", col_name_original_csv ,"') в числовой для '", col_name_target,
                              "' появились NA. Примеры проблемных исходных значений: ", paste(head(unique(problematic_values)), collapse=", ")), call. = FALSE)
            }
            df[[col_name_target]] <- numeric_vector
        } else {
            warning(paste("Исходный столбец '", col_name_in_df, "' (оригинал: '", col_name_original_csv ,"') для '", col_name_target, "' не найден. '", col_name_target, "' будет NA.", sep=""), call. = FALSE)
            df[[col_name_target]] <- NA_real_
        }
        return(df)
    }

    if (nrow(meteo_data_processed) > 0 && "DateTime" %in% names(meteo_data_processed)) {
        meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, "SR01Up_Avg", "Rg") 
        meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, "AirTC_Avg", "Tair") 
        meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, "RH", "rH") 
        meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, "NetRs_Avg", "Rn") 

        tsoil_cols_original_csv <- paste0("TSoil_", 1:6, "_Avg") 
        existing_tsoil_cols_target <- character(0) 
        temp_tsoil_data <- list()

        for (i in seq_along(tsoil_cols_original_csv)) {
            col_orig_csv <- tsoil_cols_original_csv[i]
            col_target_name <- paste0("Tsoil_temp_", i) 
            
            if (make.names(col_orig_csv) %in% names(meteo_data_processed)) {
                meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, col_orig_csv, col_target_name)
                temp_tsoil_data[[col_target_name]] <- meteo_data_processed[[col_target_name]]
                existing_tsoil_cols_target <- c(existing_tsoil_cols_target, col_target_name)
            }
        }

        if (length(existing_tsoil_cols_target) > 0) {
            tsoil_df_for_means <- as.data.frame(temp_tsoil_data)
            meteo_data_processed$Tsoil <- rowMeans(tsoil_df_for_means, na.rm = TRUE)
            meteo_data_processed$Tsoil[is.nan(meteo_data_processed$Tsoil)] <- NA 
            message(paste("Tsoil рассчитана как среднее из:", paste(existing_tsoil_cols_target, collapse=", ")))
            meteo_data_processed <- meteo_data_processed %>% select(-all_of(existing_tsoil_cols_target))
        } else {
            warning(paste0("Столбцы для Tsoil (например, '", tsoil_cols_original_csv[1], "') не найдены. Tsoil будет NA."), call. = FALSE)
            meteo_data_processed$Tsoil <- NA_real_
        }

        svp_col_csv <- "SVP_kPa"
        vp_col_csv <- "VP_kPa"
        vpd_col_csv <- "VPD" 

        if (all(c(make.names(svp_col_csv), make.names(vp_col_csv)) %in% names(meteo_data_processed))) {
            meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, svp_col_csv, "SVP_kPa_numeric")
            meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, vp_col_csv, "VP_kPa_numeric")
            
            if(sum(!is.na(meteo_data_processed$SVP_kPa_numeric)) > 0 && sum(!is.na(meteo_data_processed$VP_kPa_numeric)) > 0) {
                meteo_data_processed$VPD_calc <- (meteo_data_processed$SVP_kPa_numeric - meteo_data_processed$VP_kPa_numeric) * 10
            } else {
                meteo_data_processed$VPD_calc <- NA_real_
            }
            
            vpd_col_in_df_made_names <- make.names(vpd_col_csv)
            if (!(vpd_col_in_df_made_names %in% names(meteo_data_processed)) || all(is.na(meteo_data_processed[[vpd_col_in_df_made_names]]))) { 
                 meteo_data_processed$VPD <- meteo_data_processed$VPD_calc 
            } else {
                meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, vpd_col_csv, "VPD") 
            }
            meteo_data_processed <- meteo_data_processed %>% select(-any_of(c("SVP_kPa_numeric", "VP_kPa_numeric", "VPD_calc")))

        } else if (all(c("Tair", "rH") %in% names(meteo_data_processed)) && 
            sum(!is.na(meteo_data_processed$Tair)) > 0 && sum(!is.na(meteo_data_processed$rH)) > 0) {
          meteo_data_processed <- meteo_data_processed %>%
            mutate(VPD = REddyProc::fCalcVPDfromRHandTair(rH, Tair)) 
        } else {
          meteo_data_processed$VPD <- NA_real_
        }

        ppfd_col_original_csv <- "PPFD_Avg" 
        ppfd_col_in_df_made_names <- make.names(ppfd_col_original_csv)

        if ("Rg" %in% names(meteo_data_processed) && sum(!is.na(meteo_data_processed$Rg)) > 0) { 
           if (!(ppfd_col_in_df_made_names %in% names(meteo_data_processed)) || all(is.na(meteo_data_processed[[ppfd_col_in_df_made_names]]))) {
               meteo_data_processed$PPFD <- bigleaf::Rg.to.PPFD(meteo_data_processed$Rg, J_to_mol = 4.6, frac_PAR = 0.5) 
           } else {
                meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, ppfd_col_original_csv, "PPFD")
           }
        } else {
          if (!(ppfd_col_in_df_made_names %in% names(meteo_data_processed))) {
              meteo_data_processed$PPFD <- NA_real_
          } else {
              meteo_data_processed <- convert_and_check_numeric(meteo_data_processed, ppfd_col_original_csv, "PPFD")
          }
        }

        final_meteo_cols_to_select <- c("DateTime", "Rg", "Tair", "rH", "VPD", "PPFD", "Rn", "Tsoil")
        final_meteo_cols_to_select <- final_meteo_cols_to_select[final_meteo_cols_to_select %in% names(meteo_data_processed)]
        if (length(final_meteo_cols_to_select) > 0 && "DateTime" %in% final_meteo_cols_to_select) {
            meteo_data_processed <- meteo_data_processed %>%
                select(all_of(final_meteo_cols_to_select)) %>%
                distinct(DateTime, .keep_all = TRUE) # Убедимся в уникальности DateTime
        } else {
            warning("Не удалось выбрать финальные метеорологические столбцы. meteo_data_processed может быть некорректным.")
            meteo_data_processed <- data.frame(DateTime = as.POSIXct(character())) # Создаем пустой, если проблема
        }
    } else {
         meteo_data_processed <- data.frame(DateTime = as.POSIXct(character())) # Если нет DateTime или строк
    }
} else {
  warning(paste("Столбцы '", meteo_date_col_in_df, "' и/или '", meteo_time_col_in_df, "' отсутствуют в meteo_data_raw. Метеоданные не будут обработаны.", sep=""))
  meteo_data_processed <- data.frame(DateTime = as.POSIXct(character())) 
}
message(paste("Размер meteo_data_processed после обработки:", nrow(meteo_data_processed), "строк"))


# --- Создание полного временного ряда и слияние ---
message("Создание полного временного ряда для слияния...")
full_time_sequence_df <- data.frame(
  DateTime = seq(from = common_start_datetime_utc, 
                 to = common_end_datetime_utc, 
                 by = "30 min")
)
message(paste("Полный временной ряд создан с", nrow(full_time_sequence_df), "записями, от", 
              format(min(full_time_sequence_df$DateTime), "%Y-%m-%d %H:%M:%S"), "до", 
              format(max(full_time_sequence_df$DateTime), "%Y-%m-%d %H:%M:%S")))

message("Слияние обработанных данных EddyPro с полным временным рядом...")
merged_data <- left_join(full_time_sequence_df, eddy_data_processed, by = "DateTime")

message("Слияние обработанных метеоданных с результатом...")
# Проверяем, есть ли строки в meteo_data_processed перед слиянием
if (nrow(meteo_data_processed) > 0 && "DateTime" %in% names(meteo_data_processed)) {
    merged_data <- left_join(merged_data, meteo_data_processed, by = "DateTime")
} else {
    warning("meteo_data_processed пуст или не содержит DateTime, слияние метеоданных пропущено.")
    # Если метеоданных нет, нужно создать пустые столбцы, которые ожидает REddyProc, если они были в potential_input_vars
    # Это более сложная логика, пока просто предупреждаем.
}


message(paste("Размер merged_data после всех слияний:", nrow(merged_data), "строк"))
message("Имена столбцов в merged_data после всех слияний:")
print(names(merged_data))

# Фильтрация по значениям (после слияния, чтобы не нарушить последовательность NA)
if (nrow(merged_data) > 0) {
    # Используем стандартные имена столбцов, так как они должны были быть переименованы ранее
    # или созданы как NA в full_time_sequence_df, если отсутствовали
    if ("VPD" %in% names(merged_data)) {
        merged_data <- merged_data %>% mutate(VPD = ifelse(VPD < 0 | VPD > 60, NA, VPD)) 
    }
    if ("Ustar" %in% names(merged_data)) { 
        merged_data <- merged_data %>% mutate(Ustar = ifelse(Ustar < 0 | Ustar > 5, NA, Ustar)) 
    }
    if ("NEE" %in% names(merged_data)) { 
        merged_data <- merged_data %>% mutate(NEE = ifelse(NEE < -50 | NEE > 50, NA, NEE)) 
    }
    message(paste("Размер merged_data после фильтрации по значениям:", nrow(merged_data)))
}


message(paste("Размер merged_data перед передачей в REddyProc:", nrow(merged_data), "строк"))
if (nrow(merged_data) > 0 && "DateTime" %in% names(merged_data) && sum(!is.na(merged_data$DateTime)) > 0) {
  message(paste("Первая временная метка в merged_data:", as.character(first(na.omit(merged_data$DateTime)))))
  message(paste("Последняя временная метка в merged_data:", as.character(last(na.omit(merged_data$DateTime)))))
  # Проверка на равноотстояние перед передачей в REddyProc
  time_diffs <- diff(as.numeric(merged_data$DateTime))
  if (any(time_diffs != 1800)) { # 1800 секунд = 30 минут
      warning("ВНИМАНИЕ: Временной ряд в merged_data НЕ равноотстоящий ПЕРЕД передачей в REddyProc!")
      problem_indices <- which(time_diffs != 1800)
      message("Проблемные разницы (в секундах) в индексах (относительно diff):")
      print(head(data.frame(index = problem_indices, diff_value = time_diffs[problem_indices])))
  } else {
      message("Временной ряд в merged_data равноотстоящий (30 мин) перед передачей в REddyProc.")
  }

} else if (nrow(merged_data) == 0) {
  message("merged_data пуст перед передачей в REddyProc.")
}

# Переименования здесь уже не нужны, если они были сделаны правильно выше
# и если REddyProc будет использовать стандартные имена из merged_data
# Убедимся, что final_input_vars соответствует именам в merged_data
potential_input_vars <- c("DateTime", "NEE","Tair","rH","LE","H","Ustar","VPD","Rg","PPFD","Rn","Tsoil")
if (nrow(merged_data) > 0) {
    final_input_vars <- potential_input_vars[potential_input_vars %in% names(merged_data)]
    # Проверка и преобразование типов
    for (col_name in final_input_vars) {
        if (col_name != "DateTime" && !is.numeric(merged_data[[col_name]])) {
            # Если столбец весь NA, as.numeric сделает его numeric NA, что нормально
            if(all(is.na(merged_data[[col_name]]))) {
                 merged_data[[col_name]] <- as.numeric(merged_data[[col_name]])
            } else {
                warning(paste("Столбец", col_name, "в merged_data не является числовым перед инициализацией EProc. Попытка преобразования..."))
                merged_data[[col_name]] <- suppressWarnings(as.numeric(as.character(merged_data[[col_name]])))
            }
        }
    }
} else {
    final_input_vars <- character(0)
}
message(paste("Финальный набор переменных для REddyProc (final_input_vars):", paste(final_input_vars, collapse=", ")))


if (nrow(merged_data) == 0 || length(final_input_vars) == 0 || !("DateTime" %in% final_input_vars) ) {
  stop("После предобработки и слияния не осталось данных для указанного диапазона дат, ключевых переменных или столбца DateTime для REddyProc.")
}
if (!"NEE" %in% final_input_vars) {
    stop("Столбец NEE отсутствует в final_input_vars. Анализ невозможен.")
}
if (!"Ustar" %in% final_input_vars) { 
    stop("Столбец Ustar отсутствует в final_input_vars после обработки. Анализ невозможен.")
}


# --------------------------------------------------------------------------
# 5. Инициализация класса sEddyProc и начальное заполнение пропусков метеоданных
# --------------------------------------------------------------------------
message("Инициализация класса sEddyProc...")
EProc <- NULL # Обнуляем EProc перед попыткой создания
tryCatch({
    EProc <- sEddyProc$new(
      ID = site_id,
      Data = merged_data[, final_input_vars], 
      ColNames = final_input_vars,
      ColPOSIXTime = 'DateTime',
      DTS = 48, 
      LatDeg = lat_deg,
      LongDeg = long_deg,
      TimeZoneHour = timezone_h
    )
}, error = function(e) {
    message("ОШИБКА при инициализации sEddyProc$new():")
    message(e$message)
    stop("Не удалось инициализировать объект sEddyProc. Проверьте сообщения об ошибках выше, особенно касающиеся временного ряда.")
})

message("Проверка данных в объекте EProc после инициализации:")
if (!is.null(EProc) && !is.null(EProc$.self)) {
    eproc_data_check <- tryCatch(EProc$sGetData(), error = function(e) {
        warning(paste("Ошибка при вызове EProc$sGetData():", e$message), call. = FALSE)
        return(NULL)
    })

    if (!is.null(eproc_data_check) && is.data.frame(eproc_data_check)) {
        message(paste("Размерность EProc$sGetData():", paste(dim(eproc_data_check), collapse=" x ")))
        message("Имена столбцов в EProc$sGetData():")
        print(names(eproc_data_check))

        time_col_in_eproc <- if ("sDateTime" %in% names(eproc_data_check)) "sDateTime" else "DateTime"
        if (time_col_in_eproc %in% names(eproc_data_check)) {
            message(paste0("Первые несколько значений ", time_col_in_eproc, " в EProc$sGetData():"))
            print(head(eproc_data_check[[time_col_in_eproc]]))
            message(paste0("Класс ", time_col_in_eproc, " в EProc$sGetData():", class(eproc_data_check[[time_col_in_eproc]])[1]))
        } else {
            warning("Столбцы 'DateTime' и 'sDateTime' ОТСУТСТВУЮТ в EProc$sGetData() после инициализации!", call. = FALSE)
        }
    } else {
        warning("EProc$sGetData() не вернул корректный data.frame или объект EProc не инициализирован.", call. = FALSE)
    }
} else {
    warning("Объект EProc не был корректно инициализирован.", call. = FALSE)
}


message("Начальное заполнение пропусков в метеорологических переменных...")
fill_if_present <- function(eproc_obj, var_name, fill_all_flag = FALSE) {
    if (is.null(eproc_obj) || is.null(eproc_obj$.self)) {
        warning(paste("Объект EProc не инициализирован, пропуск sMDSGapFill для", var_name), call. = FALSE)
        return()
    }
    current_data_names <- names(eproc_obj$sGetData()) 
    if (var_name %in% current_data_names) {
        message(paste("Заполнение пропусков для:", var_name))
        eproc_obj$sMDSGapFill(var_name, FillAll = fill_all_flag)
    } else {
        warning(paste("Столбец", var_name, "отсутствует в EProc (проверено через sGetData), пропуск sMDSGapFill."), call. = FALSE)
    }
}

if ("Rg" %in% final_input_vars) fill_if_present(EProc, "Rg")
if (!is.null(EProc) && !is.null(EProc$.self)) EProc$sCalcPotRadiation(useSolartime = TRUE) 
if ("Rn" %in% final_input_vars) fill_if_present(EProc, "Rn")
if ("PPFD" %in% final_input_vars) fill_if_present(EProc, "PPFD")
if ("Tsoil" %in% final_input_vars) fill_if_present(EProc, "Tsoil")
if ("Tair" %in% final_input_vars) fill_if_present(EProc, "Tair")
if ("VPD" %in% final_input_vars) fill_if_present(EProc, "VPD")
if ("LE" %in% final_input_vars) fill_if_present(EProc, "LE")
if ("H" %in% final_input_vars) fill_if_present(EProc, "H")


# --------------------------------------------------------------------------
# 6. Оценка порога uStar (фрикционной скорости)
# --------------------------------------------------------------------------
if (!is.null(EProc) && !is.null(EProc$.self)) {
    message("Определение сезонного фактора для оценки uStar...")
    season_definition_df <- data.frame(V1 = season_start_days, V2 = current_year)

    eproc_get_data_result <- NULL
    time_col_name_in_eproc <- NULL

    eproc_get_data_result <- EProc$sGetData()
    if (is.data.frame(eproc_get_data_result) && nrow(eproc_get_data_result) > 0) {
        if ("sDateTime" %in% names(eproc_get_data_result)) { 
            time_col_name_in_eproc <- "sDateTime"
        } else if ("DateTime" %in% names(eproc_get_data_result)) {
            time_col_name_in_eproc <- "DateTime"
        }
    }

    if (!is.null(time_col_name_in_eproc)) {
        if (!inherits(eproc_get_data_result[[time_col_name_in_eproc]], "POSIXct")) {
            stop(paste("Временной столбец", time_col_name_in_eproc, "в EProc не является POSIXct."))
        }
        seasonFactor <- usCreateSeasonFactorYdayYear(
        eproc_get_data_result[[time_col_name_in_eproc]] - minutes(15), 
        starts = season_definition_df
        )
        EProc$sSetUStarSeasons(seasonFactor = seasonFactor)
        message("Сезонный фактор успешно создан и установлен в объекте EProc.")
    } else {
        warning("Ошибка: Нет данных или подходящего временного столбца (sDateTime/DateTime) в EProc$sGetData() для создания seasonFactor. Оценка uStar может быть неточной.", call. = FALSE)
        seasonFactor <- NULL 
    }

    message("Оценка порога uStar (sEstUstarThold)...")
    rg_col_for_ustar <- if ("Rg_f" %in% names(EProc$sExportData())) "Rg_f" else if ("Rg" %in% names(EProc$sExportData())) "Rg" else NULL

    if (is.null(rg_col_for_ustar)) {
        warning("Столбец для Rg ('Rg' или 'Rg_f') не найден для оценки uStar. Пропускается sEstUstarThold.", call. = FALSE)
    } else {
        # ИСПРАВЛЕНО: Удален аргумент SeasonFactor из вызова sEstUstarThold
        uStarThold_results <- EProc$sEstUstarThold(RgColName = rg_col_for_ustar) 
        message("sEstUstarThold выполнен.")
        print(uStarThold_results)
    }


    message("Оценка сценариев uStar (sEstimateUstarScenarios)...")
    tair_col_for_ustar <- if ("Tair_f" %in% names(EProc$sExportData())) "Tair_f" else if ("Tair" %in% names(EProc$sExportData())) "Tair" else NULL

    required_cols_for_scenarios <- c("Ustar", "NEE") 
    if (!is.null(tair_col_for_ustar)) required_cols_for_scenarios <- c(required_cols_for_scenarios, tair_col_for_ustar)
    if (!is.null(rg_col_for_ustar)) required_cols_for_scenarios <- c(required_cols_for_scenarios, rg_col_for_ustar)

    cols_exist_in_eproc_for_scenarios <- sapply(required_cols_for_scenarios, function(col) {
        col %in% names(EProc$sExportData()) 
    })


    if (!all(cols_exist_in_eproc_for_scenarios)) {
         missing_cols_str <- paste(required_cols_for_scenarios[!cols_exist_in_eproc_for_scenarios], collapse=", ")
         warning(paste("Необходимые столбцы (", missing_cols_str, ") не найдены для sEstimateUstarScenarios. Пропускается."), call. = FALSE)
         median_suffix_plot <- NULL 
    } else {
        EProc$sEstimateUstarScenarios(
          nSample = 100L, 
          probs = ustar_probabilities,
          seasonFactor = seasonFactor, # seasonFactor здесь используется правильно
          UstarColName = "Ustar", 
          NEEColName ="NEE",
          TempColName = tair_col_for_ustar, 
          RgColName = rg_col_for_ustar      
        )
        ustar_scenarios_df <- EProc$sGetEstimatedUstarThresholdDistribution()
        message("Сценарии uStar:")
        print(ustar_scenarios_df)

        median_suffix_plot <- "U50" 
        available_uStar_suffixes <- character(0)
        if (!is.null(EProc) && !is.null(EProc$.self)) {
            available_uStar_suffixes <- EProc$sGetUstarSuffixes()
        }

        if (length(available_uStar_suffixes) > 0 && !median_suffix_plot %in% available_uStar_suffixes) {
            guessed_median_suffix_candidates <- available_uStar_suffixes[grep("50", available_uStar_suffixes)]
            if (length(guessed_median_suffix_candidates) > 0 && nchar(guessed_median_suffix_candidates[1]) > 0) {
                median_suffix_plot <- guessed_median_suffix_candidates[1]
            } else if (length(available_uStar_suffixes) > 0) {
                median_suffix_plot <- available_uStar_suffixes[ceiling(length(available_uStar_suffixes)/2)]
            } else {
                median_suffix_plot <- NULL
            }
             warning(paste("Суффикс 'U50' не найден. Используется автоматически определенный:", median_suffix_plot), call. = FALSE)
        } else if (length(available_uStar_suffixes) == 0 && median_suffix_plot == "U50") {
            warning("Сценарии uStar не были оценены (available_uStar_suffixes пуст). median_suffix_plot установлен в NULL.", call. = FALSE)
            median_suffix_plot <- NULL
        } else if (length(available_uStar_suffixes) > 0 && median_suffix_plot %in% available_uStar_suffixes) {
            message(paste("Используется медианный суффикс uStar:", median_suffix_plot))
        } else {
            warning("Не удалось определить median_suffix_plot. Проверьте результаты sEstimateUstarScenarios.", call. = FALSE)
            median_suffix_plot <- NULL
        }
    }
    message(paste("Выбранный median_suffix_plot для дальнейших шагов:", ifelse(is.null(median_suffix_plot), "NULL", median_suffix_plot)))
} else { # EProc is NULL
    warning("Объект EProc не инициализирован. Шаги оценки uStar будут пропущены.", call. = FALSE)
    median_suffix_plot <- NULL
}

# --------------------------------------------------------------------------
# 7. Заполнение пропусков NEE с учетом сценариев uStar
# --------------------------------------------------------------------------
if (!is.null(EProc) && !is.null(EProc$.self) && "NEE" %in% names(EProc$sDATA) && !is.null(median_suffix_plot) && length(EProc$sGetUstarSuffixes()) > 0) {
    message("Заполнение пропусков NEE с учетом сценариев uStar...")
    EProc$sMDSGapFillUStarScens("NEE", FillAll = TRUE)
    message("Заполнение пропусков NEE завершено.")
} else {
    warning("NEE, median_suffix_plot или сценарии uStar отсутствуют/некорректны, или EProc не инициализирован. Пропуск sMDSGapFillUStarScens для NEE.", call. = FALSE)
    if (!is.null(EProc) && !is.null(EProc$.self) && "NEE" %in% names(EProc$sDATA)) {
        message("Попытка выполнить обычное заполнение NEE (sMDSGapFill), если sMDSGapFillUStarScens пропущено.")
        EProc$sMDSGapFill("NEE", FillAll = TRUE) 
    }
}

# --------------------------------------------------------------------------
# 8. Разделение потоков (Flux Partitioning)
# --------------------------------------------------------------------------
if (!is.null(EProc) && !is.null(EProc$.self)) {
    is_nee_filled <- FALSE
    nee_filled_col_to_check <- NULL

    if (!is.null(median_suffix_plot)) {
        nee_filled_col_to_check <- paste0("NEE_", median_suffix_plot, "_f") 
    } else {
        nee_filled_col_to_check <- "NEE_f" 
    }

    if (exists("EProc") && !is.null(EProc$.self) && !is.null(nee_filled_col_to_check) && nee_filled_col_to_check %in% names(EProc$sExportResults())) {
        is_nee_filled <- TRUE
        message(paste("Обнаружен заполненный столбец NEE:", nee_filled_col_to_check))
    }


    if (is_nee_filled) {
        message("Разделение потоков...")
        temp_var_for_partitioning <- if ("Tair_f" %in% names(EProc$sExportData())) "Tair_f" else if ("Tair" %in% names(EProc$sExportData())) "Tair" else NULL
        vpd_var_for_partitioning <- if ("VPD_f" %in% names(EProc$sExportData())) "VPD_f" else if ("VPD" %in% names(EProc$sExportData())) "VPD" else NULL
        rg_var_for_partitioning <- if ("Rg_f" %in% names(EProc$sExportData())) "Rg_f" else if ("Rg" %in% names(EProc$sExportData())) "Rg" else NULL
        ppfd_var_for_partitioning <- if ("PPFD_f" %in% names(EProc$sExportData())) "PPFD_f" else if ("PPFD" %in% names(EProc$sExportData())) "PPFD" else NULL

        rad_var_for_lasslop <- if (!is.null(ppfd_var_for_partitioning)) ppfd_var_for_partitioning else rg_var_for_partitioning
        actual_data_cols_in_eproc <- names(EProc$sExportData()) 

        required_vars_for_part_MR <- c(temp_var_for_partitioning, rg_var_for_partitioning) 
        required_vars_for_part_MR <- required_vars_for_part_MR[!sapply(required_vars_for_part_MR, is.null)] 

        if(all(required_vars_for_part_MR %in% actual_data_cols_in_eproc)){
            message("Разделение потоков ночным методом (Reichstein)...")
            EProc$sMRFluxPartitionUStarScens(TempVar = temp_var_for_partitioning, RadVar = rg_var_for_partitioning)
        } else {
            missing_part_vars <- required_vars_for_part_MR[!required_vars_for_part_MR %in% actual_data_cols_in_eproc]
            warning(paste("Отсутствуют переменные для sMRFluxPartitionUStarScens (",paste(missing_part_vars, collapse=", "),"). Пропуск."), call. = FALSE)
        }

        if (use_Lasslop_partitioning) {
            required_vars_for_part_GL <- c(temp_var_for_partitioning, vpd_var_for_partitioning, rad_var_for_lasslop)
            required_vars_for_part_GL <- required_vars_for_part_GL[!sapply(required_vars_for_part_GL, is.null)]

            if(length(required_vars_for_part_GL) == 3 && all(required_vars_for_part_GL %in% actual_data_cols_in_eproc)){
                message("Разделение потоков дневным методом (Lasslop)...")
                EProc$sGLFluxPartitionUStarScens(
                    NEEVar = "NEE", 
                    TempVar = temp_var_for_partitioning,
                    VPDVar = vpd_var_for_partitioning,
                    RadVar = rad_var_for_lasslop 
                )
            } else {
                missing_part_vars <- required_vars_for_part_GL[!required_vars_for_part_GL %in% actual_data_cols_in_eproc]
                expected_vars_str <- paste(c("Tair_f/Tair", "VPD_f/VPD", "PPFD_f/PPFD или Rg_f/Rg"), collapse=", ")
                warning(paste("Отсутствуют или некорректны переменные для sGLFluxPartitionUStarScens. Ожидались:", expected_vars_str,
                              ". Найдены для использования:", paste(required_vars_for_part_GL, collapse=", "),
                              ". Отсутствуют из ожидаемых:", paste(missing_part_vars, collapse=", "), ". Пропуск sGLFluxPartitionUStarScens."), call. = FALSE)
            }
        }
    } else {
        warning(paste0("Заполненный NEE (ожидался столбец '", nee_filled_col_to_check, "') отсутствует. Пропуск разделения потоков."), call. = FALSE)
    }
} else { # EProc is NULL
     warning("Объект EProc не инициализирован. Шаги разделения потоков будут пропущены.", call. = FALSE)
}

# --------------------------------------------------------------------------
# 9. Экспорт результатов
# --------------------------------------------------------------------------
message("Экспорт результатов...")
FilledEddyData <- data.frame()

if (!is.null(EProc) && !is.null(EProc$.self)) {
    exported_data_part <- tryCatch(EProc$sExportData(), error = function(e) { 
        warning(paste("Ошибка при вызове EProc$sExportData():", e$message), call. = FALSE)
        return(data.frame())
    })
    exported_results_part <- tryCatch(EProc$sExportResults(), error = function(e) { 
        warning(paste("Ошибка при вызове EProc$sExportResults():", e$message), call. = FALSE)
        return(data.frame())
    })

    if (!is.data.frame(exported_data_part)) exported_data_part <- data.frame()
    if (!is.data.frame(exported_results_part)) exported_results_part <- data.frame()

    time_col_data <- NULL
    if ("sDateTime" %in% names(exported_data_part)) {
        time_col_data <- "sDateTime"
    } else if ("DateTime" %in% names(exported_data_part)) { 
        time_col_data <- "DateTime"
    }


    if (nrow(exported_data_part) > 0 && !is.null(time_col_data)) {
        exported_results_part_to_bind <- exported_results_part
        time_col_results <- NULL
        if ("sDateTime" %in% names(exported_results_part_to_bind)) {
            time_col_results <- "sDateTime"
        } else if ("DateTime" %in% names(exported_results_part_to_bind)) {
            time_col_results <- "DateTime"
        }

        if (!is.null(time_col_results)) {
             exported_results_part_to_bind <- exported_results_part_to_bind %>% dplyr::select(-all_of(time_col_results))
        }
        
        common_cols_to_remove_from_results <- intersect(names(exported_data_part), names(exported_results_part_to_bind))
        common_cols_to_remove_from_results <- common_cols_to_remove_from_results[common_cols_to_remove_from_results != time_col_data] 

        if (length(common_cols_to_remove_from_results) > 0) {
            exported_results_part_to_bind <- exported_results_part_to_bind %>% dplyr::select(-any_of(common_cols_to_remove_from_results))
        }
        
        if (ncol(exported_results_part_to_bind) > 0 || nrow(exported_results_part_to_bind) == 0 ) { 
            if (nrow(exported_data_part) == nrow(exported_results_part) || nrow(exported_results_part_to_bind) == 0) { 
                 FilledEddyData <- cbind(exported_data_part, exported_results_part_to_bind)
            } else {
                warning(paste("Несовпадение количества строк между sExportData (", nrow(exported_data_part),
                              ") и sExportResults (", nrow(exported_results_part),
                              "). Попытка слияния по времени, если возможно, иначе только sExportData."), call. = FALSE)
                FilledEddyData <- exported_data_part
            }
        } else { 
            FilledEddyData <- exported_data_part
        }
        
        if (time_col_data %in% names(FilledEddyData)) {
          FilledEddyData <- FilledEddyData %>% rename(timestamp = all_of(time_col_data))
        }

        fp_cols <- names(FilledEddyData)[grep("^FP_", names(FilledEddyData))]
        if (length(fp_cols) > 0) {
          FilledEddyData <- FilledEddyData %>%
            fill(all_of(fp_cols), .direction = "downup") 
        }

        if (nrow(FilledEddyData) > 0) {
            write.csv(FilledEddyData, output_csv_path, row.names = FALSE, na = "") 
            message(paste("Обработанные данные сохранены в:", output_csv_path))
        } else {
            message("Нет данных для экспорта в CSV (FilledEddyData пуст после обработки).")
        }

    } else {
        warning("EProc$sExportData() не вернул данные или подходящий временной столбец (DateTime/sDateTime). Экспорт невозможен.", call. = FALSE)
        message("Нет данных для экспорта в CSV.")
    }
} else {
    message("Объект EProc не был корректно инициализирован или не существует. Экспорт невозможен.")
}


# --------------------------------------------------------------------------
# 10. Построение графиков
# --------------------------------------------------------------------------
message("Построение графиков...")

if (!is.null(EProc) && !is.null(EProc$.self) && !is.null(plots_output_dir) && !is.null(median_suffix_plot) && exists("FilledEddyData") && nrow(FilledEddyData) > 0 && "timestamp" %in% names(FilledEddyData)) {

  message("Построение стандартных графиков REddyProc...")
  vars_for_plotting <- list()
  vars_for_plotting$NEE_filled <- paste0("NEE_", median_suffix_plot, "_f")
  vars_for_plotting$LE_filled <- paste0("LE_", median_suffix_plot, "_f") 
  vars_for_plotting$H_filled <- paste0("H_", median_suffix_plot, "_f")   

  if (!vars_for_plotting$LE_filled %in% names(FilledEddyData) && "LE_f" %in% names(FilledEddyData)) vars_for_plotting$LE_filled <- "LE_f"
  if (!vars_for_plotting$H_filled %in% names(FilledEddyData) && "H_f" %in% names(FilledEddyData)) vars_for_plotting$H_filled <- "H_f"

  vars_for_plotting$Reco_MR <- paste0("Reco_", median_suffix_plot) 
  vars_for_plotting$GPP_MR <- paste0("GPP_", median_suffix_plot, "_f") 

  if (use_Lasslop_partitioning) {
    vars_for_plotting$Reco_GL <- paste0("Reco_DT_", median_suffix_plot) 
    vars_for_plotting$GPP_GL <- paste0("GPP_DT_", median_suffix_plot) 
  }

  meteo_vars_f <- c("Tair_f", "Rg_f", "VPD_f", "PPFD_f", "Rn_f", "Tsoil_f")
  for(mv in meteo_vars_f) {
      if (mv %in% names(FilledEddyData)) vars_for_plotting[[mv]] <- mv
  }

  vars_to_plot_final <- unlist(vars_for_plotting)
  vars_to_plot_final <- vars_to_plot_final[vars_to_plot_final %in% names(FilledEddyData)]

  for (var_to_plot in unique(vars_to_plot_final)) {
      message(paste("Построение графика HHFluxes для:", var_to_plot))
      tryCatch({
        EProc$sPlotHHFluxes(Var.s = var_to_plot, Dir.s = plots_output_dir, Format.s = "png")
      }, error = function(e) {
        warning(paste("Не удалось построить график HHFluxes для", var_to_plot, ":", e$message), call. = FALSE)
      })
  }

  tair_fp_var <- if ("Tair_f" %in% names(FilledEddyData)) "Tair_f" else if ("Tair" %in% names(FilledEddyData)) "Tair" else NULL
  if (!is.null(tair_fp_var)) {
      message(paste("Построение графика Fingerprint для:", tair_fp_var))
      tryCatch({
        EProc$sPlotFingerprint(Var.s = tair_fp_var, Dir.s = plots_output_dir, Format.s = "png")
      }, error = function(e) {
        warning(paste("Не удалось построить график Fingerprint для", tair_fp_var, ":", e$message), call. = FALSE)
      })
  } else {
      message("Tair_f или Tair отсутствуют, график Fingerprint не будет построен.")
  }

  dc_vars_to_plot <- c(vars_for_plotting$GPP_GL, vars_for_plotting$Reco_GL, vars_for_plotting$GPP_MR, vars_for_plotting$Reco_MR)
  dc_vars_to_plot <- dc_vars_to_plot[!sapply(dc_vars_to_plot, is.null)] 
  dc_vars_to_plot <- dc_vars_to_plot[dc_vars_to_plot %in% names(FilledEddyData)] 

  for (var_to_plot in unique(dc_vars_to_plot)) {
      message(paste("Построение графика DiurnalCycle (стандартный REddyProc) для:", var_to_plot))
      tryCatch({
        EProc$sPlotDiurnalCycle(Var.s = var_to_plot, Dir.s = plots_output_dir, Format.s = "png")
      }, error = function(e) {
        warning(paste("Не удалось построить график DiurnalCycle для", var_to_plot, ":", e$message), call. = FALSE)
      })
  }

  message("Построение сравнительных графиков GPP и Reco...")
  plot_data_long_list <- list()

  gpp_mr_col <- vars_for_plotting$GPP_MR
  reco_mr_col <- vars_for_plotting$Reco_MR
  if (!is.null(gpp_mr_col) && !is.null(reco_mr_col) &&
      gpp_mr_col %in% names(FilledEddyData) && reco_mr_col %in% names(FilledEddyData)) {
      plot_data_long_list$MR <- FilledEddyData %>%
        select(timestamp, GPP = all_of(gpp_mr_col), Reco = all_of(reco_mr_col)) %>%
        mutate(Method = "Reichstein (Ночной)")
  }

  if (use_Lasslop_partitioning) {
    gpp_gl_col <- vars_for_plotting$GPP_GL
    reco_gl_col <- vars_for_plotting$Reco_GL
    if (!is.null(gpp_gl_col) && !is.null(reco_gl_col) &&
        gpp_gl_col %in% names(FilledEddyData) && reco_gl_col %in% names(FilledEddyData)) {
        plot_data_long_list$GL <- FilledEddyData %>%
          select(timestamp, GPP = all_of(gpp_gl_col), Reco = all_of(reco_gl_col)) %>%
          mutate(Method = "Lasslop (Дневной)")
    }
  }

  if (length(plot_data_long_list) > 0) {
      comparison_data_long <- bind_rows(plot_data_long_list) %>%
        mutate(
            Method = factor(Method, levels = c("Reichstein (Ночной)", "Lasslop (Дневной)")), 
            Year = year(timestamp),
            Month = month(timestamp, label = TRUE, abbr = FALSE, locale="ru_RU.UTF-8"), 
            DOY = yday(timestamp),
            Hour = hour(timestamp)
        ) %>%
        filter(!is.na(Method)) 


      if (nrow(comparison_data_long) > 0 && length(unique(comparison_data_long$Method)) > 0) {
          plot_gpp_ts_combined <- ggplot(comparison_data_long, aes(x = timestamp, y = GPP, color = Method)) +
            geom_line(alpha = 0.7, na.rm = TRUE) +
            labs(title = paste("Сравнение GPP по методам:", site_id, current_year), x = "Время", y = expression("GPP ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
            theme_bw() +
            scale_color_brewer(palette = "Set1", name = "Метод") +
            theme(legend.position = "top")
          ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_GPP_TimeSeries_Combined.png")), plot_gpp_ts_combined, width = 12, height = 6, dpi=300)
          message("Сохранен график Comparison_GPP_TimeSeries_Combined.png")

          plot_reco_ts_combined <- ggplot(comparison_data_long, aes(x = timestamp, y = Reco, color = Method)) +
            geom_line(alpha = 0.7, na.rm = TRUE) +
            labs(title = paste("Сравнение Reco по методам:", site_id, current_year), x = "Время", y = expression("Reco ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
            theme_bw() +
            scale_color_brewer(palette = "Set1", name = "Метод") +
            theme(legend.position = "top")
          ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_Reco_TimeSeries_Combined.png")), plot_reco_ts_combined, width = 12, height = 6, dpi=300)
          message("Сохранен график Comparison_Reco_TimeSeries_Combined.png")

          diurnal_summary_monthly <- comparison_data_long %>%
            filter(!is.na(GPP), !is.na(Reco)) %>%
            group_by(Month, Hour, Method) %>%
            summarise(
              Mean_GPP = mean(GPP, na.rm = TRUE),
              SE_GPP = sd(GPP, na.rm = TRUE) / sqrt(n()),
              Mean_Reco = mean(Reco, na.rm = TRUE),
              SE_Reco = sd(Reco, na.rm = TRUE) / sqrt(n()),
              .groups = "drop"
            ) %>%
            mutate(
              GPP_lower = Mean_GPP - 1.96 * SE_GPP, 
              GPP_upper = Mean_GPP + 1.96 * SE_GPP,
              Reco_lower = Mean_Reco - 1.96 * SE_Reco,
              Reco_upper = Mean_Reco + 1.96 * SE_Reco
            )

          if(nrow(diurnal_summary_monthly) > 0) {
              plot_gpp_dc_monthly <- ggplot(diurnal_summary_monthly, aes(x = Hour, y = Mean_GPP, color = Method, group = Method)) +
                geom_line(na.rm = TRUE) +
                geom_ribbon(aes(ymin = GPP_lower, ymax = GPP_upper, fill = Method), alpha = 0.2, linetype = 0, na.rm = TRUE) +
                # ИСПРАВЛЕНО: Удален scales = "free_y" для одинаковой оси Y
                facet_wrap(~Month, ncol = 3) + 
                labs(title = paste("Среднесуточный ход GPP по месяцам:", site_id, current_year),
                     x = "Час дня", y = expression("Средний GPP ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
                theme_bw() +
                scale_color_brewer(palette = "Set1", name = "Метод") +
                scale_fill_brewer(palette = "Set1", name = "Метод") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top",
                      strip.text = element_text(size = 8)) 
              ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_GPP_DiurnalCycle_Monthly.png")), plot_gpp_dc_monthly, width = 12, height = 8, dpi=300) 
              message("Сохранен график Comparison_GPP_DiurnalCycle_Monthly.png")

              plot_reco_dc_monthly <- ggplot(diurnal_summary_monthly, aes(x = Hour, y = Mean_Reco, color = Method, group = Method)) +
                geom_line(na.rm = TRUE) +
                geom_ribbon(aes(ymin = Reco_lower, ymax = Reco_upper, fill = Method), alpha = 0.2, linetype = 0, na.rm = TRUE) +
                # ИСПРАВЛЕНО: Удален scales = "free_y" для одинаковой оси Y
                facet_wrap(~Month, ncol = 3) + 
                labs(title = paste("Среднесуточный ход Reco по месяцам:", site_id, current_year),
                     x = "Час дня", y = expression("Среднее Reco ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
                theme_bw() +
                scale_color_brewer(palette = "Set1", name = "Метод") +
                scale_fill_brewer(palette = "Set1", name = "Метод") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top",
                      strip.text = element_text(size = 8))
              ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_Reco_DiurnalCycle_Monthly.png")), plot_reco_dc_monthly, width = 12, height = 8, dpi=300) 
              message("Сохранен график Comparison_Reco_DiurnalCycle_Monthly.png")
          } else {
              message("Нет данных для построения месячных суточных циклов GPP/Reco.")
          }
              
            diurnal_summary_overall <- comparison_data_long %>%
            filter(!is.na(GPP), !is.na(Reco)) %>%
            group_by(Hour, Method) %>%
            summarise(
              Mean_GPP = mean(GPP, na.rm = TRUE),
              SE_GPP = sd(GPP, na.rm = TRUE) / sqrt(n()),
              Mean_Reco = mean(Reco, na.rm = TRUE),
              SE_Reco = sd(Reco, na.rm = TRUE) / sqrt(n()),
              .groups = "drop"
            ) %>% 
            mutate(
              GPP_lower = Mean_GPP - 1.96 * SE_GPP,
              GPP_upper = Mean_GPP + 1.96 * SE_GPP,
              Reco_lower = Mean_Reco - 1.96 * SE_Reco,
              Reco_upper = Mean_Reco + 1.96 * SE_Reco
            )
          if(nrow(diurnal_summary_overall) > 0){
              plot_gpp_dc_overall <- ggplot(diurnal_summary_overall, aes(x = Hour, y = Mean_GPP, color = Method, group = Method)) +
                geom_line(na.rm = TRUE) +
                geom_ribbon(aes(ymin = GPP_lower, ymax = GPP_upper, fill = Method), alpha = 0.2, linetype = 0, na.rm = TRUE) +
                labs(title = paste("Среднесуточный ход GPP (весь период):", site_id, current_year), 
                     x = "Час дня", y = expression("Средний GPP ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
                theme_bw() +
                scale_color_brewer(palette = "Set1", name = "Метод") +
                scale_fill_brewer(palette = "Set1", name = "Метод") +
                theme(legend.position = "top")
              ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_GPP_DiurnalCycle_Overall.png")), plot_gpp_dc_overall, width = 8, height = 6, dpi=300)
              message("Сохранен график Comparison_GPP_DiurnalCycle_Overall.png")

              plot_reco_dc_overall <- ggplot(diurnal_summary_overall, aes(x = Hour, y = Mean_Reco, color = Method, group = Method)) +
                geom_line(na.rm = TRUE) +
                geom_ribbon(aes(ymin = Reco_lower, ymax = Reco_upper, fill = Method), alpha = 0.2, linetype = 0, na.rm = TRUE) +
                labs(title = paste("Среднесуточный ход Reco (весь период):", site_id, current_year), 
                     x = "Час дня", y = expression("Среднее Reco ("*mu*"mol CO"[2]*" м"^-2*" с"^-1*")")) +
                theme_bw() +
                scale_color_brewer(palette = "Set1", name = "Метод") +
                scale_fill_brewer(palette = "Set1", name = "Метод") +
                theme(legend.position = "top")
              ggsave(file.path(plots_output_dir, paste0(site_id,"_",current_year,"_Comparison_Reco_DiurnalCycle_Overall.png")), plot_reco_dc_overall, width = 8, height = 6, dpi=300)
              message("Сохранен график Comparison_Reco_DiurnalCycle_Overall.png")
          } else {
              message("Нет данных для построения общих суточных циклов GPP/Reco.")
          }
      } else {
          message("Недостаточно данных или методов для построения сравнительных графиков GPP/Reco (после фильтрации).")
      }
  } else {
      message("Недостаточно данных для построения сравнительных графиков GPP/Reco (не удалось собрать данные из FilledEddyData).")
  }
  
} else {
  message("Графики не будут построены: папка для графиков не указана, median_suffix_plot не определен, FilledEddyData пуст или не содержит столбец 'timestamp'.")
}

message(paste("Анализ для", current_year, "года (диапазон апрель-сентябрь) завершен."))
# --------------------------------------------------------------------------
# Конец скрипта
# ---------------------