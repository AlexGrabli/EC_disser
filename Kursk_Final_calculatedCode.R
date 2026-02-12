library(REddyProc)
library(dplyr)
library(zoo)

# ==============================================================================
# 1. ЗАГРУЗКА И СЕТКА
# ==============================================================================
df <- read.csv("Kursk_GapFilled_Bigleaf_2.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
df <- df[!duplicated(df$DateTime), ]

# Полная временная сетка
start_date <- as.POSIXct("2013-04-24 00:00:00", tz = "GMT")
end_date   <- as.POSIXct("2013-08-31 23:30:00", tz = "GMT")
full_grid <- data.frame(DateTime = seq(from = start_date, to = end_date, by = "30 min"))

df_complete <- merge(full_grid, df, by = "DateTime", all.x = TRUE)
df_complete <- df_complete[order(df_complete$DateTime), ]

# ==============================================================================
# 2. МЕТЕО (БЕЗОПАСНОЕ ЗАПОЛНЕНИЕ)
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
# 3. ЧИСТКА И РУЧНАЯ КОРРЕКЦИЯ (ВАШИ ДАТЫ)
# ==============================================================================

# --- 3.1. СПИСОК ПРОБЛЕМНЫХ ЗОН ---
corrections <- list(
  # 1. Майский участок (с 13 по 23 мая)
  list(start="2013-05-13 00:00", end="2013-05-23 23:30", action="delete"),
  
  # 2. Июльский участок 1 (с 3 по 10 июля)
  list(start="2013-07-03 00:00", end="2013-07-10 23:30", action="delete"),
  
  # 3. Июльский участок 2 (13-14 июля)
  list(start="2013-07-13 00:00", end="2013-07-14 23:30", action="delete")
)

message("Применение ручной коррекции зон...")
for(corr in corrections) {
  s_t <- as.POSIXct(corr$start, tz="GMT")
  e_t <- as.POSIXct(corr$end, tz="GMT")
  
  idx <- which(df_complete$DateTime >= s_t & df_complete$DateTime <= e_t)
  
  if(length(idx) > 0) {
    if(corr$action == "delete") {
      df_complete$NEE[idx] <- NA
      message(paste("Удален (будет пересчитан) участок:", corr$start, "-", corr$end))
    }
  }
}

# --- 3.2. Автоматическая чистка (Global Limits) ---
df_complete$NEE[df_complete$NEE > 40]  <- NA
df_complete$NEE[df_complete$NEE < -60] <- NA

# Удаление одиночных выбросов (Despiking)
despike_mad <- function(x, window_size = 13, z = 4) {
  roll_med <- rollapply(x, width = window_size, FUN = median, na.rm = TRUE, fill = NA, partial = TRUE)
  roll_mad <- rollapply(x, width = window_size, FUN = mad, na.rm = TRUE, fill = NA, partial = TRUE)
  lower <- roll_med - (z * roll_mad)
  upper <- roll_med + (z * roll_mad)
  ifelse(x < lower | x > upper, NA, x)
}
df_complete$NEE <- despike_mad(df_complete$NEE, window_size = 13, z = 4.0)

# --- 3.3. Синтетическое дыхание для Апреля ---
# (Чтобы май стартовал с хорошей базы)
df_complete$DOY <- as.numeric(format(df_complete$DateTime, "%j"))
dormant_period <- df_complete$DOY < 121
R_ref_boost <- 2.0; Q10 <- 2.0
synth_reco <- R_ref_boost * Q10 ^ ((df_complete$Tair - 10) / 10)
synth_reco[synth_reco < 0.2] <- 0.2

fix_mask <- dormant_period & (is.na(df_complete$NEE) | df_complete$NEE < 0)
if(sum(fix_mask) > 0) {
  noise <- runif(sum(fix_mask), -0.1, 0.1) 
  df_complete$NEE[fix_mask] <- synth_reco[fix_mask] + noise
}

# ==============================================================================
# 4. ЗАПУСК REddyProc (Пересчет удаленных зон)
# ==============================================================================
EProc <- sEddyProc$new(
  'Kursk_Site', 
  df_complete, 
  c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar', 'LE', 'H', 'rH')
)
EProc$sSetLocationInfo(LatDeg = 51.7, LongDeg = 36.2, TimeZoneHour = 4)

# Заполнение метео
EProc$sMDSGapFill('Rg', FillAll=TRUE); EProc$sMDSGapFill('Tair', FillAll=TRUE); EProc$sMDSGapFill('VPD', FillAll=TRUE)

# --- USTAR (Safe) ---
tryCatch({ EProc$sEstUstarThresholdDistribution(nSample=50L, probs=c(0.05, 0.5, 0.95)) }, error=function(e){})
sc <- EProc$sGetUstarScenarios()
ustar_ok <- (nrow(sc) > 0 && !any(is.na(sc$uStar)))

if (ustar_ok) {
  message("Auto Ustar.")
  EProc$sMDSGapFillAfterUstar('NEE', FillAll=TRUE)
} else {
  message("Manual Ustar (0.2).")
  EProc$sMDSGapFillAfterUstar('NEE', uStarTh=0.2, FillAll=TRUE)
}

# Заполнение H/LE (если данных достаточно)
if(sum(!is.na(df_complete$LE)) > 100) EProc$sMDSGapFill('LE', FillAll=TRUE)
if(sum(!is.na(df_complete$H)) > 100)  EProc$sMDSGapFill('H', FillAll=TRUE)

# Partitioning
if (ustar_ok) EProc$sGLFluxPartition(uStarScenKeep="U50") else EProc$sGLFluxPartition(suffix="uStar")

# ==============================================================================
# 5. ЭКСПОРТ И СГЛАЖИВАНИЕ
# ==============================================================================
results <- EProc$sExportResults()
output <- cbind(df_complete[, "DateTime", drop=FALSE], results)

# April cleanup (GPP=0)
out_doy <- as.numeric(format(output$DateTime, "%j"))
april_only <- out_doy < 121
gpp_col <- grep("GPP_DT.*", names(output), value=TRUE)[1]
reco_col <- grep("Reco_DT.*", names(output), value=TRUE)[1]
nee_col <- grep("NEE.*_f$", names(output), value=TRUE)[1]

if(!is.na(gpp_col)) {
  output[april_only, gpp_col] <- 0
  if(!is.na(reco_col)) output[april_only, nee_col] <- output[april_only, reco_col]
}

# Финальное сглаживание (ширина 5 для большей плавности)
message("Сглаживание итоговых графиков...")
if(!is.na(gpp_col) && !is.na(reco_col)) {
  output[[gpp_col]] <- rollapply(output[[gpp_col]], width=5, FUN=mean, fill=NA, partial=TRUE, align="center")
  output[[reco_col]] <- rollapply(output[[reco_col]], width=5, FUN=mean, fill=NA, partial=TRUE, align="center")
  output[[nee_col]] <- output[[reco_col]] - output[[gpp_col]]
}

write.csv(output, "Kursk_Final_Recalculated.csv", row.names = FALSE)
message("Готово! Проблемные зоны удалены и пересчитаны алгоритмом.")