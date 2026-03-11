library(REddyProc)
library(dplyr)
library(zoo)

# ==============================================================================
# 1. ЗАГРУЗКА И ВРЕМЕННАЯ СЕТКА
# ==============================================================================
df <- read.csv("Kursk_GapFilled_Bigleaf_2.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
df <- df[!duplicated(df$DateTime), ]

# Идеальная сетка (24.04 - 31.08)
start_date <- as.POSIXct("2013-04-24 00:00:00", tz = "GMT")
end_date   <- as.POSIXct("2013-08-31 23:30:00", tz = "GMT")
full_grid <- data.frame(DateTime = seq(from = start_date, to = end_date, by = "30 min"))

df_complete <- merge(full_grid, df, by = "DateTime", all.x = TRUE)
df_complete <- df_complete[order(df_complete$DateTime), ]

# ==============================================================================
# 2. PRE-FILLING МЕТЕО (Чтобы REddyProc не ругался)
# ==============================================================================
safe_interp <- function(x) {
  x_filled <- na.approx(x, na.rm = FALSE, rule = 2)
  if(sum(is.na(x_filled)) > 0) x_filled[is.na(x_filled)] <- 0
  return(x_filled)
}
df_complete$Rg   <- safe_interp(df_complete$Rg)
df_complete$Tair <- safe_interp(df_complete$Tair)
df_complete$rH   <- safe_interp(df_complete$rH)
df_complete$VPD  <- safe_interp(df_complete$VPD)

# ==============================================================================
# 3. РУЧНАЯ ЧИСТКА (Убираем ложный GPP весной)
# ==============================================================================
# Удаляем экстремальные выбросы
df_complete$NEE[df_complete$NEE > 25]  <- NA
df_complete$NEE[df_complete$NEE < -25] <- NA

# Удаляем ночное поглощение (Rg < 10 и NEE < 0)
bad_night <- which(df_complete$Rg < 10 & df_complete$NEE < 0)
if(length(bad_night) > 0) df_complete$NEE[bad_night] <- NA

# Удаляем поглощение в холод (Tair < 5 и NEE < -1)
cold_uptake <- which(df_complete$Tair < 5 & df_complete$NEE < -1)
if(length(cold_uptake) > 0) df_complete$NEE[cold_uptake] <- NA


# ==============================================================================
# 4. ЗАПУСК REddyProc (ИСПРАВЛЕННАЯ ВЕРСИЯ)
# ==============================================================================
EProc <- sEddyProc$new(
  'Kursk_Site', 
  df_complete, 
  c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar', 'LE', 'H', 'rH')
)

EProc$sSetLocationInfo(LatDeg = 51.7, LongDeg = 36.2, TimeZoneHour = 4)

# --- ШАГ 1: Заполняем МЕТЕОДАННЫЕ (Обязательно для Partitioning!) ---
# Программа создаст колонки Tair_f, Rg_f, VPD_f
message("Заполнение метеоданных...")
EProc$sMDSGapFill('Tair', FillAll = TRUE)
EProc$sMDSGapFill('VPD',  FillAll = TRUE)
EProc$sMDSGapFill('Rg',   FillAll = TRUE)

# --- ШАГ 2: Заполняем NEE с учетом Ustar ---
message("Заполнение NEE (Ustar = 0.15)...")
EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.15, FillAll = TRUE)

# --- ШАГ 3: Заполняем остальные потоки ---
EProc$sMDSGapFill('LE', FillAll = TRUE)
EProc$sMDSGapFill('H',  FillAll = TRUE)

# --- ШАГ 4: Разделение потоков (Lasslop) ---
message("Запуск разделения потоков (Lasslop)...")
# ВАЖНО: Добавляем suffix = "uStar", чтобы программа взяла NEE_uStar_f
EProc$sGLFluxPartition(suffix = "uStar")

# ==============================================================================
# 5. ЭКСПОРТ
# ==============================================================================
results <- EProc$sExportResults()
output <- cbind(df_complete[, "DateTime", drop=FALSE], results)

write.csv(output, "Kursk_REddyProc_Results_Manual_Final.csv", row.names = FALSE)
message("Готово! Файл 'Kursk_REddyProc_Results_Final.csv' создан.")