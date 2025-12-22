# =============================================================================
# Kursk EC Data Postprocessing with REddyProc (Lasslop Flux Partitioning)
# =============================================================================
# This script performs:
# 1. Data loading and preparation for REddyProc
# 2. Gap-filling (MDS method)
# 3. Flux partitioning using Lasslop (GL2010) method
# 4. Visualization: diurnal cycles, WUE, light curves, cumulative fluxes
# =============================================================================

# Load required libraries
library(REddyProc)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(readxl)
library(bigleaf)
library(ETpartitioning)

# Define phenophase names
PHASE_RU <- c("Всходы", "Кущение", "Выход в трубку", "Колошение", "Цветение", "Созревание")
PHASE_EN <- c("Emergence", "Tillering", "Stem elongation", "Heading", "Flowering", "Ripening")

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load raw data from EddyPro output
kursk_data <- fread("Kursk_data_half_our.csv")

# Load phenophase data from xlsx
# Try to read Phase column from xlsx file
phenophase_data <- tryCatch({
  xlsx_data <- read_excel("Kursk_data.xlsx", sheet = 1)
  if ("Phase" %in% names(xlsx_data)) {
    xlsx_data %>% select(DoY, Phase) %>% distinct() %>% arrange(DoY)
  } else {
    NULL
  }
}, error = function(e) {
  cat("Warning: Could not load phenophase data from Kursk_data.xlsx\n")
  NULL
})

# Define phenophase boundaries by DoY (Kursk 2013)
# Sowing: DoY 115, Harvesting: DoY 226
phase_bounds <- data.frame(
  Phase_ru = factor(PHASE_RU, levels = PHASE_RU),
  Phase_en = factor(PHASE_EN, levels = PHASE_EN),
  DoY_start = c(118, 136, 157, 165, 180, 196)  # Kursk phenophase dates
)

# Function to assign phenophase based on DoY
assign_phase <- function(doy, bounds = phase_bounds) {
  phase <- NA_character_
  for (i in nrow(bounds):1) {
    if (!is.na(doy) && doy >= bounds$DoY_start[i]) {
      phase <- as.character(bounds$Phase_ru[i])
      break
    }
  }
  return(phase)
}

# Site coordinates (Kursk region - Obojan)
Lat_deg <- 51.14567
Long_deg <- 36.50624
TimeZone_h <- 3  # Moscow time zone (UTC+3)

# Parse DateTime
kursk_data$DateTime <- as.POSIXct(kursk_data$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Shift time by 3 hours (UTC to Moscow time)
kursk_data$DateTime <- kursk_data$DateTime + 3 * 3600

# Create REddyProc-compatible columns
# REddyProc expects specific column names including DateTime
EddyData <- kursk_data %>%
  mutate(
    DateTime = DateTime,  # Keep DateTime for REddyProc
    Year = year(DateTime),
    DoY = yday(DateTime),
    Hour = hour(DateTime) + minute(DateTime)/60,
    # NEE in umol m-2 s-1 (already in this format from EddyPro)
    NEE = as.numeric(NEE),
    # Latent heat flux in W m-2
    LE = as.numeric(LE),
    # Sensible heat flux in W m-2
    H = as.numeric(H),
    # Friction velocity in m s-1
    Ustar = as.numeric(Ustar),
    # Air temperature in deg C
    Tair = as.numeric(Tair),
    # Global radiation in W m-2
    Rg = as.numeric(Rg),
    # Vapor Pressure Deficit in hPa
    VPD = as.numeric(VPD) * 10,  # Convert from kPa to hPa if needed
    # Relative humidity in %
    rH = as.numeric(rH),
    # Soil temperature in deg C
    Tsoil = as.numeric(Tsoil),
    # Soil water content in m3 m-3
    SWC = as.numeric(SWC_1)
  ) %>%
  select(DateTime, Year, DoY, Hour, NEE, LE, H, Ustar, Tair, Rg, VPD, rH, Tsoil, SWC) %>%
  as.data.frame()

# Replace -9999 and other missing value indicators with NA
EddyData[EddyData == -9999] <- NA
EddyData[EddyData == -999] <- NA

# Check for valid data ranges
EddyData$NEE[abs(EddyData$NEE) > 100] <- NA
EddyData$Ustar[EddyData$Ustar < 0 | EddyData$Ustar > 5] <- NA
EddyData$Rg[EddyData$Rg < 0] <- 0
EddyData$VPD[EddyData$VPD < 0] <- 0  # Fix negative VPD values

# Pad data to complete days for REddyProc
# Find first and last timestamps
first_dt <- min(EddyData$DateTime, na.rm = TRUE)
last_dt <- max(EddyData$DateTime, na.rm = TRUE)

# Calculate expected first timestamp (00:30 of first day)
first_day <- as.Date(first_dt)
expected_first <- as.POSIXct(paste(first_day, "00:30:00"))

# Calculate expected last timestamp (00:00 of next day after last)
last_day <- as.Date(last_dt)
expected_last <- as.POSIXct(paste(last_day + 1, "00:00:00"))

# Create complete time sequence
complete_times <- seq(expected_first, expected_last, by = "30 min")

# Merge with existing data
complete_df <- data.frame(DateTime = complete_times) %>%
  left_join(EddyData, by = "DateTime") %>%
  mutate(
    Year = year(DateTime),
    DoY = yday(DateTime),
    Hour = hour(DateTime) + minute(DateTime)/60
  )

EddyData <- complete_df

cat("Data loaded:", nrow(EddyData), "half-hourly records\n")
cat("Date range:", min(kursk_data$DateTime, na.rm = TRUE), "to",
    max(kursk_data$DateTime, na.rm = TRUE), "\n")

# =============================================================================
# 2. REDDYPROC INITIALIZATION AND GAP-FILLING
# =============================================================================

# Initialize REddyProc object
EProc <- sEddyProc$new(
  "Kursk",           # Site ID
  EddyData,          # Data frame
  c("NEE", "LE", "H", "Rg", "Tair", "VPD", "Ustar", "rH", "Tsoil", "SWC")  # Variables
)

# Set location info for solar time calculation
EProc$sSetLocationInfo(
  LatDeg = Lat_deg,
  LongDeg = Long_deg,
  TimeZoneHour = TimeZone_h
)

# Estimate Ustar threshold (for filtering low-turbulence periods)
# Use a fixed threshold if automatic estimation fails
ustar_est <- EProc$sEstUstarThold()

if (length(ustar_est$uStarTh) == 0 || is.na(ustar_est$uStarTh[1])) {
  # Use typical value for agricultural sites if estimation fails
  ustar_val <- 0.1
  cat("\nUstar threshold could not be estimated, using default:", ustar_val, "m/s\n")
} else {
  ustar_val <- ustar_est$uStarTh[1]
  cat("\nEstimated Ustar threshold:", ustar_val, "m/s\n")
}

# Gap-filling using Marginal Distribution Sampling (MDS)
# Using standard gap-filling without Ustar scenarios
EProc$sMDSGapFill("NEE", FillAll = TRUE, minNWarnRunLength = 5)
EProc$sMDSGapFill("LE", FillAll = TRUE)
EProc$sMDSGapFill("H", FillAll = TRUE)
EProc$sMDSGapFill("Tair", FillAll = FALSE)
EProc$sMDSGapFill("VPD", FillAll = FALSE)
EProc$sMDSGapFill("Rg", FillAll = FALSE)
EProc$sMDSGapFill("rH", FillAll = FALSE)
EProc$sMDSGapFill("Tsoil", FillAll = FALSE)
EProc$sMDSGapFill("SWC", FillAll = FALSE)

cat("\nGap-filling completed\n")

# =============================================================================
# 3. FLUX PARTITIONING - LASSLOP (GL2010) METHOD
# =============================================================================

# Lasslop (GL2010) method - daytime-based partitioning
# Uses light response curve with temperature dependency
EProc$sGLFluxPartition()  # Uses NEE_f by default

cat("Flux partitioning (Lasslop GL2010) completed\n")

# Extract results
Results <- EProc$sExportResults()

# Add DateTime back
Results$DateTime <- kursk_data$DateTime[1:nrow(Results)]

# Add Tsoil, Hour, and Precipitation from original data (not included in REddyProc output)
Results$Tsoil <- EddyData$Tsoil[1:nrow(Results)]
Results$Hour <- EddyData$Hour[1:nrow(Results)]
Results$Precip <- as.numeric(kursk_data$P_Tot[1:nrow(Results)])

# Add DoY and assign phenophases
Results$DoY <- yday(Results$DateTime)
Results$Phase_ru <- sapply(Results$DoY, assign_phase)
Results$Phase_ru <- factor(Results$Phase_ru, levels = PHASE_RU)
Results$Phase_en <- factor(Results$Phase_ru, levels = PHASE_RU, labels = PHASE_EN)

# Convert Rg to PPFD using bigleaf (for light curves with PPFD)
Results$PPFD <- bigleaf::Rg.to.PPFD(Results$Rg_f)

# Calculate E (evapotranspiration in mmol m-2 s-1) from LE
# LE in W m-2, convert using latent heat of vaporization
# E = LE / lambda, where lambda ~ 2.45 MJ/kg at 20C
# Then convert kg to mol: / 0.018015 kg/mol, then to mmol: * 1000
Results$E_mmol <- Results$LE_f / (2.45e6) / 0.018015 * 1000  # mmol H2O m-2 s-1

# Calculate instantaneous WUE and IWUE
# WUE = GPP / E (umol CO2 / mmol H2O)
Results$WUE_inst <- ifelse(Results$E_mmol > 0.01 & !is.na(Results$GPP_DT) & Results$GPP_DT > 0,
                           Results$GPP_DT / Results$E_mmol, NA)
# IWUE = GPP * VPD / E (umol CO2 * hPa / mmol H2O)
Results$IWUE <- ifelse(Results$E_mmol > 0.01 & !is.na(Results$GPP_DT) & Results$GPP_DT > 0 & Results$VPD_f > 0,
                       Results$GPP_DT * Results$VPD_f / Results$E_mmol, NA)

# Convert GPP and Reco to umol m-2 s-1 (they come in this unit)
# GPP_DT and Reco_DT are the partitioned fluxes from Lasslop method

# =============================================================================
# 3.1 SAVE PROCESSED DATA TO CSV
# =============================================================================

# Create output directory
dir.create("output_Kursk", showWarnings = FALSE)

# Save processed half-hourly data
write.csv(Results, "output_Kursk/Kursk_REddyProc_halfhourly.csv", row.names = FALSE)
cat("\nProcessed data saved to: output_Kursk/Kursk_REddyProc_halfhourly.csv\n")
cat("Total records:", nrow(Results), "\n")

# =============================================================================
# 4. CALCULATE DERIVED VARIABLES
# =============================================================================

# Daily aggregation
Daily <- Results %>%
  mutate(
    Date = as.Date(DateTime),
    DoY = yday(DateTime),
    Year = year(DateTime)
  ) %>%
  group_by(Date) %>%
  summarise(
    DoY = first(DoY),
    Year = first(Year),
    # Convert umol m-2 s-1 to g C m-2 d-1 (multiply by 12 * 1800 / 10^6 * 48)
    NEE_sum = sum(NEE_f, na.rm = TRUE) * 12 * 1800 / 10^6,
    GPP_sum = sum(GPP_DT, na.rm = TRUE) * 12 * 1800 / 10^6,
    Reco_sum = sum(Reco_DT, na.rm = TRUE) * 12 * 1800 / 10^6,
    # Evapotranspiration (from LE, W m-2 to mm d-1)
    ET = sum(LE_f, na.rm = TRUE) * 1800 / (2.45 * 10^6),
    # Precipitation sum (mm d-1)
    Precip_sum = sum(Precip, na.rm = TRUE),
    # Environmental means
    Tair_mean = mean(Tair_f, na.rm = TRUE),
    VPD_mean = mean(VPD_f, na.rm = TRUE),
    Rg_mean = mean(Rg_f, na.rm = TRUE),
    Tsoil_mean = mean(Tsoil_f, na.rm = TRUE),
    rH_mean = mean(rH_f, na.rm = TRUE),
    SWC_mean = mean(SWC_f, na.rm = TRUE),
    PPFD_mean = mean(PPFD, na.rm = TRUE),
    # WUE metrics
    WUE_mean = mean(WUE_inst, na.rm = TRUE),
    IWUE_mean = mean(IWUE, na.rm = TRUE),
    # Count of valid observations
    n_obs = n()
  ) %>%
  ungroup() %>%
  mutate(
    # Water Use Efficiency (WUE) - g C per mm H2O
    WUE = ifelse(ET > 0, GPP_sum / ET, NA),
    # Cumulative sums
    NEE_cum = cumsum(NEE_sum),
    GPP_cum = cumsum(GPP_sum),
    Reco_cum = cumsum(Reco_sum)
  )

# Hourly aggregation for diurnal patterns BY PHENOPHASE
Hourly <- Results %>%
  filter(!is.na(Phase_ru)) %>%
  mutate(Hour = floor(Hour)) %>%
  group_by(Phase_ru, Hour) %>%
  summarise(
    NEE_mean = mean(NEE_f, na.rm = TRUE),
    NEE_sd = sd(NEE_f, na.rm = TRUE),
    NEE_se = NEE_sd / sqrt(n()),
    GPP_mean = mean(GPP_DT, na.rm = TRUE),
    GPP_sd = sd(GPP_DT, na.rm = TRUE),
    Reco_mean = mean(Reco_DT, na.rm = TRUE),
    Reco_sd = sd(Reco_DT, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 5. VISUALIZATION
# =============================================================================

# Set theme (consistent with all_seasons_finalversion_1.R)
theme_flux <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

# -----------------------------------------------------------------------------
# 5.1 Diurnal Cycles by Phenophase
# -----------------------------------------------------------------------------

plot_diurnal_NEE <- ggplot(Hourly, aes(x = Hour, y = NEE_mean)) +
  geom_ribbon(aes(ymin = NEE_mean - NEE_se, ymax = NEE_mean + NEE_se),
              alpha = 0.2, fill = "blue") +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 1.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~Phase_ru, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
  labs(
    x = "Hour of day",
    y = expression("NEE ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal NEE by Phenophase"
  ) +
  theme_flux

plot_diurnal_GPP <- ggplot(Hourly, aes(x = Hour, y = GPP_mean)) +
  geom_ribbon(aes(ymin = GPP_mean - GPP_sd/sqrt(10), ymax = GPP_mean + GPP_sd/sqrt(10)),
              alpha = 0.2, fill = "darkgreen") +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 1.5, color = "darkgreen") +
  facet_wrap(~Phase_ru, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
  labs(
    x = "Hour of day",
    y = expression("GPP ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal GPP by Phenophase"
  ) +
  theme_flux

plot_diurnal_Reco <- ggplot(Hourly, aes(x = Hour, y = Reco_mean)) +
  geom_ribbon(aes(ymin = Reco_mean - Reco_sd/sqrt(10), ymax = Reco_mean + Reco_sd/sqrt(10)),
              alpha = 0.2, fill = "brown") +
  geom_line(linewidth = 1, color = "brown") +
  geom_point(size = 1.5, color = "brown") +
  facet_wrap(~Phase_ru, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
  labs(
    x = "Hour of day",
    y = expression("Reco ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal Reco by Phenophase"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.2 Daily Time Series with Flux Separation
# -----------------------------------------------------------------------------

# Prepare data for plotting
Daily_long <- Daily %>%
  select(Date, DoY, NEE_sum, GPP_sum, Reco_sum) %>%
  pivot_longer(cols = c(NEE_sum, GPP_sum, Reco_sum),
               names_to = "Flux", values_to = "Value") %>%
  mutate(Flux = factor(Flux,
                       levels = c("NEE_sum", "GPP_sum", "Reco_sum"),
                       labels = c("NEE", "GPP", "Reco")))

plot_daily_fluxes <- ggplot(Daily_long, aes(x = DoY, y = Value, color = Flux)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("NEE" = "blue", "GPP" = "darkgreen", "Reco" = "brown")) +
  labs(
    x = "Day of Year",
    y = expression(bold("Flux")~"(g C"~m^{-2}~d^{-1}*")"),
    title = "Daily CO2 Fluxes (Lasslop GL2010 Partitioning)",
    color = "Flux type"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.3 Cumulative Fluxes
# -----------------------------------------------------------------------------

plot_cumulative <- ggplot(Daily, aes(x = DoY)) +
  geom_line(aes(y = NEE_cum, color = "NEE"), size = 1) +
  geom_line(aes(y = GPP_cum, color = "GPP"), size = 1) +
  geom_line(aes(y = Reco_cum, color = "Reco"), size = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("NEE" = "blue", "GPP" = "darkgreen", "Reco" = "brown")) +
  labs(
    x = "Day of Year",
    y = expression(bold("Cumulative Flux")~"(g C"~m^{-2}*")"),
    title = "Cumulative Carbon Fluxes",
    color = "Flux type"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.4 Water Use Efficiency (WUE)
# -----------------------------------------------------------------------------

plot_WUE <- ggplot(Daily %>% filter(!is.na(WUE) & is.finite(WUE)),
                   aes(x = DoY, y = WUE)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", span = 0.3, color = "red", se = TRUE) +
  labs(
    x = "Day of Year",
    y = expression(bold("WUE")~"(g C / mm"~H[2]*O*")"),
    title = "Water Use Efficiency"
  ) +
  theme_flux

# WUE vs VPD
plot_WUE_VPD <- ggplot(Daily %>% filter(!is.na(WUE) & is.finite(WUE) & VPD_mean > 0),
                       aes(x = VPD_mean, y = WUE)) +
  geom_point(aes(color = Tair_mean), alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  scale_color_viridis_c(name = "Tair (C)") +
  labs(
    x = "VPD (hPa)",
    y = expression(bold("WUE")~"(g C / mm"~H[2]*O*")"),
    title = "WUE vs Vapor Pressure Deficit"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.4a WUE and IWUE by Phenophase (Barplot and Boxplot)
# -----------------------------------------------------------------------------

# Prepare WUE data by phenophase
WUE_by_phase <- Results %>%
  filter(!is.na(Phase_ru), is.finite(WUE_inst), WUE_inst > 0, WUE_inst < 50) %>%
  group_by(Phase_ru) %>%
  summarise(
    WUE_mean = mean(WUE_inst, na.rm = TRUE),
    WUE_se = sd(WUE_inst, na.rm = TRUE) / sqrt(n()),
    IWUE_mean = mean(IWUE, na.rm = TRUE),
    IWUE_se = sd(IWUE, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Seasonal means
WUE_season <- Results %>%
  filter(is.finite(WUE_inst), WUE_inst > 0, WUE_inst < 50) %>%
  summarise(
    WUE_mean = mean(WUE_inst, na.rm = TRUE),
    WUE_se = sd(WUE_inst, na.rm = TRUE) / sqrt(n()),
    IWUE_mean = mean(IWUE, na.rm = TRUE),
    IWUE_se = sd(IWUE, na.rm = TRUE) / sqrt(n())
  )

# WUE Barplot by Phenophase
plot_WUE_bar_phase <- ggplot(WUE_by_phase, aes(x = Phase_ru, y = WUE_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = WUE_mean - 1.96*WUE_se, ymax = WUE_mean + 1.96*WUE_se),
                width = 0.3) +
  labs(
    x = "Phenophase",
    y = expression("WUE ("*mu*"mol CO"[2]*" / mmol H"[2]*"O)"),
    title = "Water Use Efficiency by Phenophase"
  ) +
  theme_flux +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# IWUE Barplot by Phenophase
plot_IWUE_bar_phase <- ggplot(WUE_by_phase, aes(x = Phase_ru, y = IWUE_mean)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  geom_errorbar(aes(ymin = IWUE_mean - 1.96*IWUE_se, ymax = IWUE_mean + 1.96*IWUE_se),
                width = 0.3) +
  labs(
    x = "Phenophase",
    y = expression("IWUE ("*mu*"mol CO"[2]*" hPa / mmol H"[2]*"O)"),
    title = "Intrinsic WUE by Phenophase"
  ) +
  theme_flux +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# WUE Boxplot by Phenophase (with quantile-based outlier filtering)
wue_q99 <- quantile(Results$WUE_inst[Results$WUE_inst > 0], 0.99, na.rm = TRUE)
plot_WUE_box_phase <- ggplot(Results %>%
                               filter(!is.na(Phase_ru), is.finite(WUE_inst),
                                      WUE_inst > 0, WUE_inst < wue_q99),
                             aes(x = Phase_ru, y = WUE_inst)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5, outlier.size = 0.5) +
  labs(
    x = "Phenophase",
    y = expression("WUE ("*mu*"mol CO"[2]*" / mmol H"[2]*"O)"),
    title = "WUE Distribution by Phenophase"
  ) +
  theme_flux +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# IWUE Boxplot by Phenophase (with quantile-based outlier filtering)
iwue_q99 <- quantile(Results$IWUE[Results$IWUE > 0], 0.99, na.rm = TRUE)
plot_IWUE_box_phase <- ggplot(Results %>%
                                filter(!is.na(Phase_ru), is.finite(IWUE),
                                       IWUE > 0, IWUE < iwue_q99),
                              aes(x = Phase_ru, y = IWUE)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.5, outlier.size = 0.5) +
  labs(
    x = "Phenophase",
    y = expression("IWUE ("*mu*"mol CO"[2]*" hPa / mmol H"[2]*"O)"),
    title = "IWUE Distribution by Phenophase"
  ) +
  theme_flux +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Seasonal WUE Barplot
plot_WUE_bar_season <- ggplot(WUE_season, aes(x = "Season", y = WUE_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.5) +
  geom_errorbar(aes(ymin = WUE_mean - 1.96*WUE_se, ymax = WUE_mean + 1.96*WUE_se),
                width = 0.2) +
  labs(
    x = "",
    y = expression("WUE ("*mu*"mol CO"[2]*" / mmol H"[2]*"O)"),
    title = "Seasonal Mean WUE"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.4b GPP vs PPFD Scatter by Phenophase
# -----------------------------------------------------------------------------

plot_GPP_PPFD <- ggplot(Results %>%
                          filter(!is.na(Phase_ru), PPFD > 10,
                                 is.finite(GPP_DT), GPP_DT >= 0),
                        aes(x = PPFD, y = GPP_DT)) +
  geom_point(alpha = 0.1, size = 0.5, color = "darkgreen") +
  geom_smooth(method = "loess", span = 0.5, color = "red", se = FALSE) +
  facet_wrap(~Phase_ru, ncol = 3) +
  labs(
    x = expression("PPFD ("*mu*"mol"~m^{-2}~s^{-1}*")"),
    y = expression("GPP ("*mu*"mol CO"[2]~m^{-2}~s^{-1}*")"),
    title = "GPP vs PPFD by Phenophase"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.4c Separate Cumulative Plots with Phenophase Lines
# -----------------------------------------------------------------------------

# Define vegetation period (from sowing to harvest)
veg_start <- 115  # Sowing date for Kursk
veg_end <- 226    # Harvesting date for Kursk

# Create complete sequence of days from sowing to harvest
complete_days <- data.frame(DoY = seq(veg_start, veg_end))

# Filter daily data to vegetation period and fill missing days with zeros
Daily_veg <- complete_days %>%
  left_join(Daily, by = "DoY") %>%
  mutate(
    NEE_sum = ifelse(is.na(NEE_sum), 0, NEE_sum),
    GPP_sum = ifelse(is.na(GPP_sum), 0, GPP_sum),
    Reco_sum = ifelse(is.na(Reco_sum), 0, Reco_sum),
    NEE_cum = cumsum(NEE_sum),
    GPP_cum = cumsum(GPP_sum),
    Reco_cum = cumsum(Reco_sum)
  )

# Create phenophase boundaries for vertical lines
phase_lines <- phase_bounds %>%
  filter(DoY_start >= veg_start & DoY_start <= veg_end)

# Cumulative NEE
plot_cum_NEE <- ggplot(Daily_veg, aes(x = DoY, y = NEE_cum)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(data = phase_lines, aes(xintercept = DoY_start),
             linetype = "dotted", color = "gray40", linewidth = 0.7) +
  geom_text(data = phase_lines,
            aes(x = DoY_start + 2, y = max(Daily_veg$NEE_cum, na.rm = TRUE) * 0.9,
                label = Phase_ru),
            angle = 90, hjust = 1, vjust = 0, size = 3, color = "gray30") +
  labs(
    x = "Day of Year",
    y = expression("Cumulative NEE (g C"~m^{-2}*")"),
    title = "Cumulative NEE"
  ) +
  theme_flux

# Cumulative GPP
plot_cum_GPP <- ggplot(Daily_veg, aes(x = DoY, y = GPP_cum)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_vline(data = phase_lines, aes(xintercept = DoY_start),
             linetype = "dotted", color = "gray40", linewidth = 0.7) +
  geom_text(data = phase_lines,
            aes(x = DoY_start + 2, y = max(Daily_veg$GPP_cum, na.rm = TRUE) * 0.9,
                label = Phase_ru),
            angle = 90, hjust = 1, vjust = 0, size = 3, color = "gray30") +
  labs(
    x = "Day of Year",
    y = expression("Cumulative GPP (g C"~m^{-2}*")"),
    title = "Cumulative GPP"
  ) +
  theme_flux

# Cumulative Reco
plot_cum_Reco <- ggplot(Daily_veg, aes(x = DoY, y = Reco_cum)) +
  geom_line(linewidth = 1, color = "brown") +
  geom_vline(data = phase_lines, aes(xintercept = DoY_start),
             linetype = "dotted", color = "gray40", linewidth = 0.7) +
  geom_text(data = phase_lines,
            aes(x = DoY_start + 2, y = max(Daily_veg$Reco_cum, na.rm = TRUE) * 0.9,
                label = Phase_ru),
            angle = 90, hjust = 1, vjust = 0, size = 3, color = "gray30") +
  labs(
    x = "Day of Year",
    y = expression("Cumulative Reco (g C"~m^{-2}*")"),
    title = "Cumulative Ecosystem Respiration"
  ) +
  theme_flux

# -----------------------------------------------------------------------------
# 5.5 Light Response Curves with α and β coefficients
# -----------------------------------------------------------------------------

# Filter daytime data for light curves
light_data <- Results %>%
  filter(!is.na(Phase_ru),
         is.finite(PPFD), PPFD >= 10, PPFD <= 2200,
         is.finite(GPP_DT), GPP_DT >= 0, GPP_DT <= 40)

# Binning by PPFD for stable fitting
bin_w <- 100
binned <- light_data %>%
  mutate(PPFD_bin = pmax(0, floor(PPFD/bin_w)*bin_w)) %>%
  group_by(Phase_ru, PPFD_bin) %>%
  summarise(PPFD = mean(PPFD), GPP = mean(GPP_DT), n = n(), .groups = "drop") %>%
  arrange(Phase_ru, PPFD)

# Fit rectangular hyperbola: GPP = (α * β * PPFD) / (α * PPFD + β)
fit_lrc_group <- function(dat) {
  dat <- arrange(dat, PPFD)
  if (nrow(dat) < 5 || diff(range(dat$PPFD)) < 200 || var(dat$GPP) < 0.1)
    return(tibble(alpha = NA_real_, beta = NA_real_))

  # Starting values
  low <- dat %>% filter(PPFD <= quantile(PPFD, 0.2, na.rm = TRUE))
  a0 <- suppressWarnings(coef(lm(GPP ~ 0 + PPFD, data = low)))[1]
  if (!is.finite(a0)) a0 <- 0.03
  a0 <- min(max(a0, 0.005), 0.12)
  b0 <- quantile(dat$GPP, 0.95, na.rm = TRUE)
  if (!is.finite(b0) || b0 <= 0) b0 <- max(dat$GPP, na.rm = TRUE)
  b0 <- min(max(b0, 5), 40)

  fit <- try(
    nls(GPP ~ (alpha * beta * PPFD) / (alpha * PPFD + beta),
        data = dat,
        start = list(alpha = a0, beta = b0),
        algorithm = "port",
        lower = c(alpha = 1e-4, beta = 1),
        upper = c(alpha = 0.2, beta = 60),
        control = nls.control(maxiter = 500, warnOnly = TRUE)),
    silent = TRUE
  )

  if (!inherits(fit, "try-error")) {
    co <- coef(fit)
    return(tibble(alpha = unname(co["alpha"]), beta = unname(co["beta"])))
  }

  # Fallback: grid search
  grid_a <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.12)
  grid_b <- c(5, 8, 10, 15, 20, 30, 40)
  best <- list(a = NA_real_, b = NA_real_, rss = Inf)
  for (aa in grid_a) {
    for (bb in grid_b) {
      pred <- (aa * bb * dat$PPFD) / (aa * dat$PPFD + bb)
      rss <- sum((dat$GPP - pred)^2)
      if (is.finite(rss) && rss < best$rss) best <- list(a = aa, b = bb, rss = rss)
    }
  }
  tibble(alpha = best$a, beta = best$b)
}

# Fit coefficients for each phenophase
coef_tbl <- binned %>%
  group_by(Phase_ru) %>%
  group_modify(~fit_lrc_group(.x)) %>%
  ungroup()

# Generate fitted curves
range_tbl <- light_data %>%
  group_by(Phase_ru) %>%
  summarise(xmax = min(max(PPFD, na.rm = TRUE), 2000), .groups = "drop")

curve_tbl <- coef_tbl %>%
  left_join(range_tbl, by = "Phase_ru") %>%
  filter(is.finite(alpha), is.finite(beta), is.finite(xmax), xmax > 0) %>%
  rowwise() %>%
  mutate(data = list(tibble(
    PPFD = seq(0, xmax, length.out = 200),
    GPP_hat = (alpha * beta * PPFD) / (alpha * PPFD + beta)
  ))) %>%
  ungroup() %>%
  unnest(data) %>%
  select(Phase_ru, PPFD, GPP_hat)

# Create annotations for α and β
y_max_fixed <- suppressWarnings(max(c(light_data$GPP_DT, curve_tbl$GPP_hat), na.rm = TRUE))
if (!is.finite(y_max_fixed) || y_max_fixed <= 0) y_max_fixed <- 10

fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 3), "N/A")

anno <- coef_tbl %>%
  mutate(
    x = 50,
    y = y_max_fixed * 0.9,
    label = paste0("α = ", fmt_num(alpha), "\nβ = ", fmt_num(beta))
  )

# Theme for plots (consistent with all_seasons_finalversion_1.R)
theme_base <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

# Light curves by phenophase with fitted curves and coefficients
plot_light_curve_phase <- ggplot() +
  geom_point(data = light_data, aes(x = PPFD, y = GPP_DT),
             alpha = 0.1, size = 0.5, color = "grey50") +
  geom_line(data = curve_tbl, aes(x = PPFD, y = GPP_hat),
            color = "red", linewidth = 1) +
  geom_text(data = anno, aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, color = "black") +
  facet_wrap(~Phase_ru, ncol = 3) +
  labs(
    x = expression("PPFD ("*mu*"mol"~m^{-2}~s^{-1}*")"),
    y = expression("GPP ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Light Response Curves by Phenophase"
  ) +
  theme_base

# Print coefficients table
cat("\n========================================\n")
cat("LIGHT CURVE COEFFICIENTS (α, β) by Phenophase\n")
cat("Formula: GPP = (α * β * Rg) / (α * Rg + β)\n")
cat("========================================\n")
print(coef_tbl)

# Save coefficients
write.csv(coef_tbl, "output_Kursk/light_curve_coefficients.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 5.6 Temperature Response of Reco
# -----------------------------------------------------------------------------

plot_Reco_temp <- ggplot(Results %>% filter(!is.na(Reco_DT) & Reco_DT > 0),
                         aes(x = Tair_f, y = Reco_DT)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "gam", color = "brown", se = TRUE) +
  labs(
    x = expression(bold("Air Temperature")~"("*degree*C*")"),
    y = expression(bold("Reco")~"("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Temperature Response of Ecosystem Respiration"
  ) +
  theme_flux

# =============================================================================
# 6. SAVE OUTPUTS
# =============================================================================

# Save daily aggregated data
write.csv(Daily, "output_Kursk/Kursk_REddyProc_daily.csv", row.names = FALSE)
write.csv(coef_tbl, "output_Kursk/Kursk_light_curve_coefficients.csv", row.names = FALSE)

# Create kurskfilled.csv with gap-filled meteorological data
# This file contains daily aggregations with filled RH, VPD, Tsoil, SWC, and Precipitation
kurskfilled <- Daily %>%
  select(
    Doy = DoY,
    Date,
    NEE_f_sums = NEE_sum,
    Reco = Reco_sum,
    GPP = GPP_sum,
    SWC_f = SWC_mean,
    Tsoil_f = Tsoil_mean,
    PAR_f = PPFD_mean,
    Tair_f = Tair_mean,
    VPD_f = VPD_mean,
    rH_f = rH_mean,
    Rain_mm_Tot_sums = Precip_sum
  )

# Save kurskfilled.csv
write.csv(kurskfilled, "kurskfilled.csv", row.names = FALSE)
cat("\nGap-filled meteorological data saved to: kurskfilled.csv\n")
cat("Columns included: Doy, Date, NEE_f_sums, Reco, GPP, SWC_f, Tsoil_f, PAR_f, Tair_f, VPD_f, rH_f, Rain_mm_Tot_sums\n")

# Display all plots
print(plot_diurnal_NEE)
print(plot_diurnal_GPP)
print(plot_diurnal_Reco)
print(plot_daily_fluxes)
print(plot_cumulative)
print(plot_WUE)
print(plot_WUE_VPD)
print(plot_light_curve_phase)
print(plot_Reco_temp)
print(plot_WUE_bar_phase)
print(plot_IWUE_bar_phase)
print(plot_WUE_box_phase)
print(plot_IWUE_box_phase)
print(plot_WUE_bar_season)
print(plot_GPP_PPFD)
print(plot_cum_NEE)
print(plot_cum_GPP)
print(plot_cum_Reco)

# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

cat("\n========================================\n")
cat("SUMMARY STATISTICS (Lasslop GL2010)\n")
cat("========================================\n")

# Seasonal totals (excluding incomplete periods)
valid_days <- Daily %>% filter(!is.na(NEE_sum))

cat("\nTotal days analyzed:", nrow(valid_days), "\n")
cat("\nSeasonal carbon budget (g C m-2):\n")
cat("  NEE total:", round(sum(valid_days$NEE_sum, na.rm = TRUE), 1), "\n")
cat("  GPP total:", round(sum(valid_days$GPP_sum, na.rm = TRUE), 1), "\n")
cat("  Reco total:", round(sum(valid_days$Reco_sum, na.rm = TRUE), 1), "\n")

cat("\nMean daily fluxes (g C m-2 d-1):\n")
cat("  NEE:", round(mean(valid_days$NEE_sum, na.rm = TRUE), 2),
    "+/-", round(sd(valid_days$NEE_sum, na.rm = TRUE), 2), "\n")
cat("  GPP:", round(mean(valid_days$GPP_sum, na.rm = TRUE), 2),
    "+/-", round(sd(valid_days$GPP_sum, na.rm = TRUE), 2), "\n")
cat("  Reco:", round(mean(valid_days$Reco_sum, na.rm = TRUE), 2),
    "+/-", round(sd(valid_days$Reco_sum, na.rm = TRUE), 2), "\n")

cat("\nWater Use Efficiency (g C / mm H2O):\n")
cat("  Mean:", round(mean(valid_days$WUE, na.rm = TRUE), 2),
    "+/-", round(sd(valid_days$WUE, na.rm = TRUE), 2), "\n")

cat("\nTotal Evapotranspiration:", round(sum(valid_days$ET, na.rm = TRUE), 1), "mm\n")

cat("\n========================================\n")
cat("Results saved to 'output_Kursk/' directory\n")
cat("========================================\n")

# =============================================================================
# 8. COMPARATIVE ANALYSIS: MOSCOW (2013) vs KURSK
# =============================================================================

cat("\n========================================\n")
cat("COMPARATIVE ANALYSIS: Moscow 2013 vs Kursk\n")
cat("========================================\n")

# Load Moscow 2013 data
moscow_file <- "Lasslop_2013_Complete_GapFilled.csv"
if (file.exists(moscow_file)) {

  # Read Moscow data
  moscow_raw <- fread(moscow_file)
  names(moscow_raw) <- tolower(names(moscow_raw))

  # Parse time - try multiple formats
  if ("timestamp_start" %in% names(moscow_raw)) {
    # Format: YYYYMMDDhhmm
    moscow_raw$datetime <- as.POSIXct(moscow_raw$timestamp_start, format = "%Y%m%d%H%M", tz = "UTC")
  } else if ("datetime" %in% names(moscow_raw)) {
    # Try ISO format first
    moscow_raw$datetime <- as.POSIXct(moscow_raw$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (all(is.na(moscow_raw$datetime))) {
      # Try standard format
      moscow_raw$datetime <- as.POSIXct(moscow_raw$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }
  }

  # NOTE: No time shift for Moscow data - timestamps already in correct local time
  # (matching all_seasons_finalversion_1.R approach)

  # Moscow phenophase boundaries (2013)
  # Sowing: 2013-05-14 (DoY 134)
  B_Moscow <- list(
    Sowing = as.Date("2013-05-14"),
    Emergence = as.Date("2013-05-17"), Tillering = as.Date("2013-06-03"),
    StemElong = as.Date("2013-06-27"), Heading = as.Date("2013-07-17"),
    Flowering = as.Date("2013-07-28"), Ripening = as.Date("2013-08-03"),
    HarvestStart = as.Date("2013-08-14"),  # DoY 226, DAS 93
    HarvestEnd = as.Date("2013-09-09")     # DoY 252, DAS 119
  )

  # Kursk harvest dates (2013)
  # Sowing: DoY 115
  # Harvest start: DoY 226, DAS 112
  # Harvest end: DoY 244, DAS 130
  Harvest_Kursk <- list(
    Sowing_DoY = 115,
    HarvestStart_DoY = 226,   # DAS 112
    HarvestEnd_DoY = 244       # DAS 130
  )

  # Assign phases for Moscow
  assign_phase_moscow <- function(date_vec) {
    res <- rep(NA_character_, length(date_vec))
    d <- as.Date(date_vec)
    res[d >= B_Moscow$Emergence & d < B_Moscow$Tillering] <- "Всходы"        # May 17 - Jun 2
    res[d >= B_Moscow$Tillering & d < B_Moscow$StemElong] <- "Кущение"       # Jun 3 - Jun 26
    res[d >= B_Moscow$StemElong & d < B_Moscow$Heading] <- "Выход в трубку"  # Jun 27 - Jul 16
    res[d >= B_Moscow$Heading & d < B_Moscow$Flowering] <- "Колошение"       # Jul 17 - Jul 27
    res[d >= B_Moscow$Flowering & d < B_Moscow$Ripening] <- "Цветение"       # Jul 28 - Aug 2
    res[d >= B_Moscow$Ripening & d <= B_Moscow$HarvestEnd] <- "Созревание"   # Aug 3 - Sep 9
    factor(res, levels = PHASE_RU)
  }

  # Helper function for column selection
  pick_col <- function(nm, pats) {
    for (p in pats) {
      hit <- grep(paste0("^", p, "$"), nm, ignore.case = TRUE, value = TRUE)
      if (length(hit)) return(hit[1])
    }
    NA_character_
  }

  # Prepare Moscow data
  nm <- names(moscow_raw)
  col_gpp <- pick_col(nm, c("gpp_dt_u50", "gpp_dt_ustar", "gpp_dt", "gpp_f", "gpp"))
  col_reco <- pick_col(nm, c("reco_dt_u50", "reco_dt_ustar", "reco_dt", "reco_f", "reco"))
  col_nee <- pick_col(nm, c("nee_u50_f", "nee_ustar_f", "nee_filled", "nee_f", "nee"))
  col_le <- pick_col(nm, c("le_f", "le_orig", "le"))
  col_rg <- pick_col(nm, c("rg_f", "rg_orig", "rg", "sw_in"))
  col_tair <- pick_col(nm, c("tair_f", "tair_orig", "tair", "ta_f", "ta"))

  Moscow <- data.frame(
    Site = "Moscow",
    DateTime = moscow_raw$datetime,
    DoY = yday(moscow_raw$datetime),
    Hour = hour(moscow_raw$datetime) + minute(moscow_raw$datetime)/60
  )

  Moscow$NEE <- if (!is.na(col_nee)) as.numeric(moscow_raw[[col_nee]]) else NA
  Moscow$GPP <- if (!is.na(col_gpp)) as.numeric(moscow_raw[[col_gpp]]) else NA
  Moscow$Reco <- if (!is.na(col_reco)) as.numeric(moscow_raw[[col_reco]]) else NA
  Moscow$LE <- if (!is.na(col_le)) as.numeric(moscow_raw[[col_le]]) else NA
  Moscow$Rg <- if (!is.na(col_rg)) as.numeric(moscow_raw[[col_rg]]) else NA
  Moscow$Tair <- if (!is.na(col_tair)) as.numeric(moscow_raw[[col_tair]]) else NA
  Moscow$PPFD <- bigleaf::Rg.to.PPFD(Moscow$Rg)
  Moscow$Phase_ru <- assign_phase_moscow(Moscow$DateTime)

  # Calculate WUE for Moscow
  Moscow$E_mmol <- Moscow$LE / (2.45e6) / 0.018015 * 1000
  Moscow$WUE <- ifelse(Moscow$E_mmol > 0.01 & Moscow$GPP > 0, Moscow$GPP / Moscow$E_mmol, NA)

  # Prepare Kursk data for comparison
  Kursk <- data.frame(
    Site = "Kursk",
    DateTime = Results$DateTime,
    DoY = Results$DoY,
    Hour = Results$Hour,
    NEE = Results$NEE_f,
    GPP = Results$GPP_DT,
    Reco = Results$Reco_DT,
    LE = Results$LE_f,
    Rg = Results$Rg_f,
    Tair = Results$Tair_f,
    PPFD = Results$PPFD,
    Phase_ru = Results$Phase_ru,
    E_mmol = Results$E_mmol,
    WUE = Results$WUE_inst
  )

  # Combine datasets
  Compare <- bind_rows(Moscow, Kursk) %>%
    filter(!is.na(Phase_ru)) %>%
    # Calculate NEE as Reco - GPP (for verification)
    # NEE = Reco - GPP (where negative NEE means net CO2 uptake)
    mutate(NEE_calc = Reco - GPP)

  # Color palette
  pal_site <- c("Moscow" = "#1b9e77", "Kursk" = "#d95f02")

  # -----------------------------------------------------------------------------
  # 8.1 Comparative Diurnal Cycles
  # -----------------------------------------------------------------------------

  Hourly_compare <- Compare %>%
    mutate(HourInt = floor(Hour)) %>%
    group_by(Site, Phase_ru, HourInt) %>%
    summarise(
      NEE_mean = mean(NEE, na.rm = TRUE),
      NEE_se = sd(NEE, na.rm = TRUE) / sqrt(n()),
      GPP_mean = mean(GPP, na.rm = TRUE),
      GPP_se = sd(GPP, na.rm = TRUE) / sqrt(n()),
      Reco_mean = mean(Reco, na.rm = TRUE),
      Reco_se = sd(Reco, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(across(ends_with("_se"), ~replace_na(.x, 0)))

  # Diurnal NEE comparison
  plot_compare_diurnal_NEE <- ggplot(Hourly_compare,
                                     aes(x = HourInt, y = NEE_mean, color = Site, fill = Site)) +
    geom_ribbon(aes(ymin = NEE_mean - 1.96*NEE_se, ymax = NEE_mean + 1.96*NEE_se),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_wrap(~Phase_ru, ncol = 3) +
    scale_color_manual(values = pal_site) +
    scale_fill_manual(values = pal_site) +
    scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
    labs(
      title = "Diurnal NEE: Moscow vs Kursk",
      x = "Hour of day",
      y = expression("NEE ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")")
    ) +
    theme_flux

  # Diurnal GPP comparison (Kursk Всходы as dashed line due to limited data)
  hourly_gpp_main <- Hourly_compare %>% filter(!(Site == "Kursk" & Phase_ru == "Всходы"))
  hourly_gpp_dashed <- Hourly_compare %>% filter(Site == "Kursk" & Phase_ru == "Всходы")

  plot_compare_diurnal_GPP <- ggplot(Hourly_compare,
                                     aes(x = HourInt, y = GPP_mean, color = Site, fill = Site)) +
    geom_ribbon(aes(ymin = GPP_mean - 1.96*GPP_se, ymax = GPP_mean + 1.96*GPP_se),
                alpha = 0.15, color = NA) +
    geom_line(data = hourly_gpp_main, linewidth = 1) +
    geom_point(data = hourly_gpp_main, size = 1.2) +
    geom_line(data = hourly_gpp_dashed, linewidth = 0.7, linetype = "dashed") +
    geom_point(data = hourly_gpp_dashed, size = 1.2) +
    facet_wrap(~Phase_ru, ncol = 3) +
    scale_color_manual(values = pal_site) +
    scale_fill_manual(values = pal_site) +
    scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
    labs(
      title = "Diurnal GPP: Moscow vs Kursk",
      x = "Hour of day",
      y = expression("GPP ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")")
    ) +
    theme_flux

  # Diurnal Reco comparison
  plot_compare_diurnal_Reco <- ggplot(Hourly_compare,
                                      aes(x = HourInt, y = Reco_mean, color = Site, fill = Site)) +
    geom_ribbon(aes(ymin = Reco_mean - 1.96*Reco_se, ymax = Reco_mean + 1.96*Reco_se),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    facet_wrap(~Phase_ru, ncol = 3) +
    scale_color_manual(values = pal_site) +
    scale_fill_manual(values = pal_site) +
    scale_x_continuous(breaks = seq(0, 23, 6), limits = c(0, 23)) +
    labs(
      title = "Diurnal Reco: Moscow vs Kursk",
      x = "Hour of day",
      y = expression("Reco ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")")
    ) +
    theme_flux

  # -----------------------------------------------------------------------------
  # 8.2 Comparative Light Curves
  # -----------------------------------------------------------------------------

  light_compare <- Compare %>%
    filter(!is.na(Phase_ru),
           is.finite(PPFD), PPFD >= 10, PPFD <= 2200,
           is.finite(GPP), GPP >= 0, GPP <= 40)

  # Binning for fitting
  bin_w <- 100
  binned_compare <- light_compare %>%
    mutate(PPFD_bin = pmax(0, floor(PPFD/bin_w)*bin_w)) %>%
    group_by(Site, Phase_ru, PPFD_bin) %>%
    summarise(PPFD = mean(PPFD), GPP = mean(GPP), n = n(), .groups = "drop")

  # Fit function with improved robustness (matching fit_lrc_group from main script)
  fit_lrc <- function(dat) {
    dat <- arrange(dat, PPFD)
    if (nrow(dat) < 5 || diff(range(dat$PPFD)) < 200 || var(dat$GPP, na.rm = TRUE) < 0.1)
      return(tibble(alpha = NA_real_, beta = NA_real_))

    # Starting values - estimate alpha from low PPFD data
    low <- dat %>% filter(PPFD <= quantile(PPFD, 0.2, na.rm = TRUE))
    if (nrow(low) >= 3 && sum(!is.na(low$GPP) & !is.na(low$PPFD)) >= 3) {
      a0 <- suppressWarnings(coef(lm(GPP ~ 0 + PPFD, data = low)))[1]
      if (!is.finite(a0)) a0 <- 0.03
    } else {
      a0 <- 0.03
    }
    a0 <- min(max(a0, 0.005), 0.12)
    b0 <- quantile(dat$GPP, 0.95, na.rm = TRUE)
    if (!is.finite(b0) || b0 <= 0) b0 <- max(dat$GPP, na.rm = TRUE)
    b0 <- min(max(b0, 5), 40)

    fit <- try(
      nls(GPP ~ (alpha * beta * PPFD) / (alpha * PPFD + beta),
          data = dat,
          start = list(alpha = a0, beta = b0),
          algorithm = "port",
          lower = c(alpha = 1e-4, beta = 1),
          upper = c(alpha = 0.2, beta = 60),
          control = nls.control(maxiter = 500, warnOnly = TRUE)),
      silent = TRUE
    )

    if (!inherits(fit, "try-error")) {
      co <- coef(fit)
      return(tibble(alpha = unname(co["alpha"]), beta = unname(co["beta"])))
    }

    # Fallback: grid search if nls fails
    grid_a <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.12)
    grid_b <- c(5, 8, 10, 15, 20, 30, 40)
    best <- list(a = NA_real_, b = NA_real_, rss = Inf)
    for (aa in grid_a) {
      for (bb in grid_b) {
        pred <- (aa * bb * dat$PPFD) / (aa * dat$PPFD + bb)
        rss <- sum((dat$GPP - pred)^2, na.rm = TRUE)
        if (is.finite(rss) && rss < best$rss) best <- list(a = aa, b = bb, rss = rss)
      }
    }
    tibble(alpha = best$a, beta = best$b)
  }

  coef_compare <- binned_compare %>%
    group_by(Site, Phase_ru) %>%
    group_modify(~fit_lrc(.x)) %>%
    ungroup()

  # Generate curves
  curve_compare <- coef_compare %>%
    filter(is.finite(alpha), is.finite(beta)) %>%
    rowwise() %>%
    mutate(data = list(tibble(
      PPFD = seq(0, 2000, length.out = 200),
      GPP_hat = (alpha * beta * PPFD) / (alpha * PPFD + beta)
    ))) %>%
    ungroup() %>%
    unnest(data)

  # Create annotations for α and β
  y_max_compare <- max(c(light_compare$GPP, curve_compare$GPP_hat), na.rm = TRUE)
  fmt_num <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 3), "N/A")

  anno_compare <- coef_compare %>%
    group_by(Phase_ru) %>%
    arrange(Site) %>%
    mutate(
      x = 50,
      y = y_max_compare * (0.95 - 0.12 * (row_number() - 1)),
      label = paste0(Site, ": α=", fmt_num(alpha), " β=", fmt_num(beta))
    ) %>%
    ungroup()

  # Plot light curves comparison (Kursk Всходы as dashed due to limited data)
  curve_main <- curve_compare %>% filter(!(Site == "Kursk" & Phase_ru == "Всходы"))
  curve_dashed <- curve_compare %>% filter(Site == "Kursk" & Phase_ru == "Всходы")

  plot_compare_light <- ggplot() +
    geom_point(data = light_compare, aes(x = PPFD, y = GPP, color = Site),
               alpha = 0.05, size = 0.3) +
    geom_line(data = curve_main, aes(x = PPFD, y = GPP_hat, color = Site),
              linewidth = 1) +
    geom_line(data = curve_dashed, aes(x = PPFD, y = GPP_hat, color = Site),
              linewidth = 0.7, linetype = "dashed") +
    geom_text(data = anno_compare, aes(x = x, y = y, label = label, color = Site),
              hjust = 0, vjust = 1, size = 2.5, show.legend = FALSE) +
    facet_wrap(~Phase_ru, ncol = 3) +
    scale_color_manual(values = pal_site) +
    labs(
      title = "Light Response Curves: Moscow vs Kursk",
      x = expression("PPFD ("*mu*"mol"~m^{-2}~s^{-1}*")"),
      y = expression("GPP ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")")
    ) +
    theme_flux

  # -----------------------------------------------------------------------------
  # 8.3 Comparative WUE by Phenophase
  # -----------------------------------------------------------------------------

  WUE_compare <- Compare %>%
    filter(!is.na(Phase_ru), is.finite(WUE), WUE > 0, WUE < 15) %>%
    group_by(Site, Phase_ru) %>%
    summarise(
      WUE_mean = mean(WUE, na.rm = TRUE),
      WUE_se = sd(WUE, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  plot_compare_WUE <- ggplot(WUE_compare, aes(x = Phase_ru, y = WUE_mean, fill = Site)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.7) +
    geom_errorbar(aes(ymin = WUE_mean - 1.96*WUE_se, ymax = WUE_mean + 1.96*WUE_se),
                  position = position_dodge(0.8), width = 0.25) +
    scale_fill_manual(values = pal_site) +
    labs(
      title = "WUE by Phenophase: Moscow vs Kursk",
      x = "Phenophase",
      y = expression("WUE ("*mu*"mol CO"[2]*" / mmol H"[2]*"O)")
    ) +
    theme_flux +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # -----------------------------------------------------------------------------
  # 8.4 Comparative Cumulative Fluxes
  # -----------------------------------------------------------------------------

  # Sowing dates for each site
  sowing_Moscow <- yday(as.Date("2013-05-14"))  # DoY 134
  sowing_Kursk <- 115  # Sowing date for Kursk

  # Daily aggregation for both sites
  Daily_compare_raw <- Compare %>%
    mutate(Date = as.Date(DateTime)) %>%
    group_by(Site, Date) %>%
    summarise(
      DoY = first(DoY),
      NEE_sum = sum(NEE, na.rm = TRUE) * 12 * 1800 / 10^6,  # NEE_f from gap-filling
      NEE_calc_sum = sum(NEE_calc, na.rm = TRUE) * 12 * 1800 / 10^6,  # NEE calculated as Reco - GPP
      GPP_sum = sum(GPP, na.rm = TRUE) * 12 * 1800 / 10^6,
      Reco_sum = sum(Reco, na.rm = TRUE) * 12 * 1800 / 10^6,
      # Active temperature sum (GDD - Growing Degree Days)
      # Sum of (Tair - 10) for half-hourly periods where Tair > 10°C
      # Multiplied by 0.5/24 to convert half-hours to degree-days
      GDD_sum = sum(ifelse(!is.na(Rg) & Rg >= 0,
                           pmax(0, Tair - 10),
                           0), na.rm = TRUE) * 0.5 / 24,  # degree-days
      .groups = "drop"
    )

  # Get actual data ranges for each site
  actual_range_Moscow <- Daily_compare_raw %>%
    filter(Site == "Moscow") %>%
    summarise(min_DoY = min(DoY, na.rm = TRUE), max_DoY = max(DoY, na.rm = TRUE))

  actual_range_Kursk <- Daily_compare_raw %>%
    filter(Site == "Kursk") %>%
    summarise(min_DoY = min(DoY, na.rm = TRUE), max_DoY = max(DoY, na.rm = TRUE))

  # Create complete day sequences based on ACTUAL data range (not fixed phenophase dates)
  # This prevents filling leading days with zeros when data starts later
  complete_Moscow <- data.frame(
    Site = "Moscow",
    DoY = seq(actual_range_Moscow$min_DoY, actual_range_Moscow$max_DoY)
  )
  complete_Kursk <- data.frame(
    Site = "Kursk",
    DoY = seq(actual_range_Kursk$min_DoY, actual_range_Kursk$max_DoY)
  )
  complete_both <- bind_rows(complete_Moscow, complete_Kursk)

  # Calculate actual sowing DoY based on first data point and known DAS to emergence
  # For Moscow: Emergence is DoY 137 (May 17), which is 3 days after sowing (May 14 = DoY 134)
  # For Kursk: Emergence is DoY 118, which is 3 days after sowing (DoY 115)
  # Use actual first data DoY as reference
  actual_sowing_Moscow <- sowing_Moscow  # Keep original DoY 134
  actual_sowing_Kursk <- actual_range_Kursk$min_DoY - (118 - 115)  # Adjust based on data start

  # Merge with actual data, fill missing days with zeros
  Daily_compare <- complete_both %>%
    left_join(Daily_compare_raw, by = c("Site", "DoY")) %>%
    mutate(
      NEE_sum = ifelse(is.na(NEE_sum), 0, NEE_sum),
      NEE_calc_sum = ifelse(is.na(NEE_calc_sum), 0, NEE_calc_sum),
      GPP_sum = ifelse(is.na(GPP_sum), 0, GPP_sum),
      Reco_sum = ifelse(is.na(Reco_sum), 0, Reco_sum),
      GDD_sum = ifelse(is.na(GDD_sum), 0, GDD_sum),
      DAS = ifelse(Site == "Moscow", DoY - actual_sowing_Moscow, DoY - actual_sowing_Kursk)
    ) %>%
    group_by(Site) %>%
    arrange(DAS) %>%
    mutate(
      NEE_cum = cumsum(NEE_sum),  # Cumulative NEE from NEE_f
      NEE_calc_cum = cumsum(NEE_calc_sum),  # Cumulative NEE from Reco - GPP
      GPP_cum = cumsum(GPP_sum),
      Reco_cum = cumsum(Reco_sum),
      GDD_cum = cumsum(GDD_sum)
    ) %>%
    ungroup()

  # Filter to vegetation period (days from sowing to end of harvest)
  # Extended to 135 DAS to cover both sites' harvest end
  Daily_compare_veg <- Daily_compare %>%
    filter(DAS >= 0 & DAS <= 135) %>%
    # Recalculate cumulative sums after filtering to ensure they start from 0 at DAS = 0
    group_by(Site) %>%
    arrange(DAS) %>%
    mutate(
      NEE_cum = cumsum(NEE_sum),  # Cumulative NEE from NEE_f
      NEE_calc_cum = cumsum(NEE_calc_sum),  # Cumulative NEE from Reco - GPP
      GPP_cum = cumsum(GPP_sum),
      Reco_cum = cumsum(Reco_sum),
      GDD_cum = cumsum(GDD_sum)
    ) %>%
    ungroup()

  # Phenophase boundaries in days after sowing
  phase_DAS_Moscow <- data.frame(
    Phase_ru = PHASE_RU,
    Phase_en = PHASE_EN,
    DAS = c(yday(as.Date("2013-05-17")) - sowing_Moscow,  # Emergence
            yday(as.Date("2013-06-03")) - sowing_Moscow,  # Tillering
            yday(as.Date("2013-06-27")) - sowing_Moscow,  # StemElong
            yday(as.Date("2013-07-17")) - sowing_Moscow,  # Heading
            yday(as.Date("2013-07-28")) - sowing_Moscow,  # Flowering
            yday(as.Date("2013-08-03")) - sowing_Moscow)  # Ripening
  )

  phase_DAS_Kursk <- data.frame(
    Phase_ru = PHASE_RU,
    Phase_en = PHASE_EN,
    DAS = c(118, 136, 157, 165, 180, 196) - sowing_Kursk
  )

  # Harvest period boundaries (in days after sowing)
   # Only harvest start lines (harvest end removed for cleaner visualization)

  harvest_Moscow <- data.frame(
    Event = c("Начало уборки"),
    Event_en = c("Harvest start"),
    DAS = c(yday(B_Moscow$HarvestStart) - sowing_Moscow),    # DAS 93
    Site = "Moscow"
  )
  harvest_Kursk <- data.frame(
    Event = c("Начало уборки"),
    Event_en = c("Harvest start"),
    DAS = c(Harvest_Kursk$HarvestStart_DoY - sowing_Kursk),  # DAS 112
    Site = "Kursk"
  )
  harvest_both <- bind_rows(harvest_Moscow, harvest_Kursk)

  # Prepare phenophase lines for both sites (with Site identifier for coloring)
  phase_DAS_Moscow_plot <- phase_DAS_Moscow %>%
    mutate(Site = "Moscow")
  phase_DAS_Kursk_plot <- phase_DAS_Kursk %>%
    mutate(Site = "Kursk")
  phase_DAS_both <- bind_rows(phase_DAS_Moscow_plot, phase_DAS_Kursk_plot)

  # Color palette for phenophase lines matching site colors
  pal_phase_line <- c("Moscow" = "#1b9e77", "Kursk" = "#d95f02")
  pal_phase_alpha <- c("Moscow" = 0.4, "Kursk" = 0.4)
  # Harvest line styles (thicker and more prominent)
  pal_harvest_line <- c("Moscow" = "#1b9e77", "Kursk" = "#d95f02")

  # Russian version of cumulative plots
  plot_compare_cum_NEE_ru <- ggplot(Daily_compare_veg, aes(x = DAS, y = NEE_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = 2, color = "gray50") +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site, labels = c("Курск", "Москва")) +
    labs(
      title = "Кумулятивный NEE: Москва vs Курск",
      subtitle = "Тонкие линии - фенофазы, жирные линии - период уборки. Сплошные (Москва), пунктир (Курск)",
      x = "Дни от посева",
      y = expression("Кумулятивный NEE (g C"~m^{-2}*")"),
      color = "Участок"
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  plot_compare_cum_GPP_ru <- ggplot(Daily_compare_veg, aes(x = DAS, y = GPP_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site, labels = c("Курск", "Москва")) +
    labs(
      title = "Кумулятивный GPP: Москва vs Курск",
      subtitle = "Тонкие линии - фенофазы, жирные линии - период уборки. Сплошные (Москва), пунктир (Курск)",
      x = "Дни от посева",
      y = expression("Кумулятивный GPP (g C"~m^{-2}*")"),
      color = "Участок"
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  plot_compare_cum_Reco_ru <- ggplot(Daily_compare_veg, aes(x = DAS, y = Reco_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site, labels = c("Курск", "Москва")) +
    labs(
      title = "Кумулятивный Reco: Москва vs Курск",
      subtitle = "Тонкие линии - фенофазы, жирные линии - период уборки. Сплошные (Москва), пунктир (Курск)",
      x = "Дни от посева",
      y = expression("Кумулятивный Reco (g C"~m^{-2}*")"),
      color = "Участок"
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  # English version of cumulative plots
  plot_compare_cum_NEE_en <- ggplot(Daily_compare_veg, aes(x = DAS, y = NEE_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = 2, color = "gray50") +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$NEE_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site) +
    labs(
      title = "Cumulative NEE: Moscow vs Kursk",
      subtitle = "Thin lines - phenophases, thick lines - harvest period. Solid (Moscow), dashed (Kursk)",
      x = "Days after sowing",
      y = expression("Cumulative NEE (g C"~m^{-2}*")")
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  plot_compare_cum_GPP_en <- ggplot(Daily_compare_veg, aes(x = DAS, y = GPP_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
     #Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GPP_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site) +
    labs(
      title = "Cumulative GPP: Moscow vs Kursk",
      subtitle = "Thin lines - phenophases, thick lines - harvest period. Solid (Moscow), dashed (Kursk)",
      x = "Days after sowing",
      y = expression("Cumulative GPP (g C"~m^{-2}*")")
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  plot_compare_cum_Reco_en <- ggplot(Daily_compare_veg, aes(x = DAS, y = Reco_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$Reco_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site) +
    labs(
      title = "Cumulative Reco: Moscow vs Kursk",
      subtitle = "Thin lines - phenophases, thick lines - harvest period. Solid (Moscow), dashed (Kursk)",
      x = "Days after sowing",
      y = expression("Cumulative Reco (g C"~m^{-2}*")")
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))
  # -----------------------------------------------------------------------------
  # 8.5 Cumulative Active Temperature Sum (GDD - Growing Degree Days)
  # -----------------------------------------------------------------------------
  # Russian version - Cumulative GDD
  plot_compare_cum_GDD_ru <- ggplot(Daily_compare_veg, aes(x = DAS, y = GDD_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.95,
                  label = Phase_ru),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.75,
                  label = Event),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site, labels = c("Курск", "Москва")) +
    labs(
      title = "Кумулятивная сумма активных температур (САТ): Москва vs Курск",
      subtitle = "САТ = сумма периодов, когда T > 10°C. Тонкие линии - фенофазы, жирные - период уборки",
      x = "Дни от посева",
      y = expression("САТ (°C·день)"),
      color = "Участок"
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

  # English version - Cumulative GDD
  plot_compare_cum_GDD_en <- ggplot(Daily_compare_veg, aes(x = DAS, y = GDD_cum, color = Site)) +
    geom_line(linewidth = 1.2) +
    # Phenophase lines for Moscow (solid, thin)
    geom_vline(data = phase_DAS_Moscow_plot, aes(xintercept = DAS),
               linetype = "solid", color = pal_phase_line["Moscow"],
               linewidth = 0.5, alpha = 0.4) +
    # Phenophase lines for Kursk (dashed, thin)
    geom_vline(data = phase_DAS_Kursk_plot, aes(xintercept = DAS),
               linetype = "dashed", color = pal_phase_line["Kursk"],
               linewidth = 0.5, alpha = 0.4) +
    # Harvest lines for Moscow (solid, thick)
    geom_vline(data = harvest_Moscow, aes(xintercept = DAS),
               linetype = "solid", color = pal_harvest_line["Moscow"],
               linewidth = 1.1, alpha = 0.7) +
    # Harvest lines for Kursk (dashed, thick)
    geom_vline(data = harvest_Kursk, aes(xintercept = DAS),
               linetype = "dashed", color = pal_harvest_line["Kursk"],
               linewidth = 1.1, alpha = 0.7) +
    # Phenophase labels for Moscow (top)
    geom_text(data = phase_DAS_Moscow_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.2,
              color = pal_phase_line["Moscow"], fontface = "plain") +
    # Phenophase labels for Kursk (bottom)
    geom_text(data = phase_DAS_Kursk_plot,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.95,
                  label = Phase_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.2,
              color = pal_phase_line["Kursk"], fontface = "plain") +
    # Harvest labels for Moscow (middle-top)
    geom_text(data = harvest_Moscow,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = -0.2, size = 2.5,
              color = pal_harvest_line["Moscow"], fontface = "bold") +
    # Harvest labels for Kursk (middle-bottom)
    geom_text(data = harvest_Kursk,
              aes(x = DAS + 0.5, y = max(Daily_compare_veg$GDD_cum, na.rm = TRUE) * 0.75,
                  label = Event_en),
              angle = 90, hjust = 1, vjust = 1.3, size = 2.5,
              color = pal_harvest_line["Kursk"], fontface = "bold") +
    scale_color_manual(values = pal_site) +
    labs(
      title = "Cumulative Growing Degree Days (GDD): Moscow vs Kursk",
      subtitle = "GDD = sum when T > 10°C. Thin lines - phenophases, thick - harvest period",
      x = "Days after sowing",
      y = expression("GDD (°C·day)")
    ) +
    theme_flux +
    theme(plot.subtitle = element_text(size = 8.5, color = "gray40"))

   # Print all comparison plots
  print(plot_compare_diurnal_NEE)
  print(plot_compare_diurnal_GPP)
  print(plot_compare_diurnal_Reco)
  print(plot_compare_light)
  print(plot_compare_WUE)
  # Russian cumulative plots
  print(plot_compare_cum_NEE_ru)
  print(plot_compare_cum_GPP_ru)
  print(plot_compare_cum_Reco_ru)
  print(plot_compare_cum_GDD_ru)
  # English cumulative plots
  print(plot_compare_cum_NEE_en)
  print(plot_compare_cum_GPP_en)
  print(plot_compare_cum_Reco_en)
  print(plot_compare_cum_GDD_en)


  # Print coefficients comparison
  cat("\n========================================\n")
  cat("LIGHT CURVE COEFFICIENTS: Moscow vs Kursk\n")
  cat("========================================\n")
  print(coef_compare %>% arrange(Phase_ru, Site))

  # Save comparison data
  write.csv(coef_compare, "output_Kursk/compare_light_curve_coefficients.csv", row.names = FALSE)
  write.csv(Daily_compare_veg, "output_Kursk/compare_daily_cumulative.csv", row.names = FALSE)

  # Save combined half-hourly data (Moscow + Kursk)
  Compare_export <- Compare %>%
    select(Site, DateTime, DoY, Hour, Phase_ru, NEE, GPP, Reco, LE, Rg, PPFD, WUE) %>%
    arrange(Site, DateTime)
  write.csv(Compare_export, "output_Kursk/Moscow_Kursk_combined_halfhourly.csv", row.names = FALSE)
  cat("\nCombined data saved to: output_Kursk/Moscow_Kursk_combined_halfhourly.csv\n")
  cat("Moscow records:", sum(Compare_export$Site == "Moscow"), "\n")
  cat("Kursk records:", sum(Compare_export$Site == "Kursk"), "\n")

  # -----------------------------------------------------------------------------
  # 8.6 Final Cumulative Summary Table
  # -----------------------------------------------------------------------------
  
  cat("\n========================================\n")
  cat("ИТОГОВЫЕ КУМУЛЯТИВНЫЕ СУММЫ ЗА ВЕГЕТАЦИОННЫЙ ПЕРИОД\n")
  cat("========================================\n")
  
  # Calculate final cumulative values for each site
  cumulative_summary <- Daily_compare_veg %>%
    group_by(Site) %>%
    summarise(
      Start_DAS = min(DAS, na.rm = TRUE),
      End_DAS = max(DAS, na.rm = TRUE),
      Days_veg = as.numeric(End_DAS - Start_DAS) + 1,
      GPP_total_gC = last(GPP_cum),
      Reco_total_gC = last(Reco_cum),
      NEE_total_gC = last(NEE_cum),  # From NEE_f (gap-filled)
      NEE_calc_total_gC = last(NEE_calc_cum),  # From Reco - GPP
      GDD_total = last(GDD_cum),
      .groups = "drop"
    ) %>%
    mutate(
      # Compare cumulative NEE_f with cumulative (Reco - GPP)
      # NEE = Reco - GPP (where negative NEE means net CO2 uptake)
      # Calculate absolute difference
      NEE_diff = abs(NEE_total_gC - NEE_calc_total_gC),
      # Calculate percentage difference using the larger absolute value as denominator
      NEE_diff_pct = ifelse(pmax(abs(NEE_total_gC), abs(NEE_calc_total_gC)) > 0.1,
                            (NEE_diff / pmax(abs(NEE_total_gC), abs(NEE_calc_total_gC))) * 100,
                            NA_real_),
      # Check if difference is within 10%
      NEE_check = ifelse(is.na(NEE_diff_pct), "N/A",
                         ifelse(NEE_diff_pct <= 10, "OK", "WARNING"))
    )
  
  # Format summary table for display
  cumulative_summary_display <- cumulative_summary %>%
    mutate(
      GPP_total_gC = round(GPP_total_gC, 2),
      Reco_total_gC = round(Reco_total_gC, 2),
      NEE_total_gC = round(NEE_total_gC, 2),  # From NEE_f
      NEE_calc_total_gC = round(NEE_calc_total_gC, 2),  # From Reco - GPP
      GDD_total = round(GDD_total, 1),
      NEE_diff = round(NEE_diff, 2),
      NEE_diff_pct = round(NEE_diff_pct, 2)
    )
  
  # Print summary table
  cat("\nСравнение кумулятивных сумм:\n")
  cat("NEE_total_gC - из NEE_f (gap-filled)\n")
  cat("NEE_calc_total_gC - из Reco - GPP (рассчитанный)\n\n")
  print(cumulative_summary_display)
  
  # Check for warnings
  warnings_found <- cumulative_summary %>%
    filter(NEE_check == "WARNING")
  
  if (nrow(warnings_found) > 0) {
    cat("\n⚠ ВНИМАНИЕ: Обнаружены расхождения между NEE_f и (Reco - GPP) > 10%:\n")
    print(warnings_found %>% 
          select(Site, NEE_total_gC, NEE_calc_total_gC, NEE_diff, NEE_diff_pct) %>%
          mutate(
            NEE_total_gC = round(NEE_total_gC, 2),
            NEE_calc_total_gC = round(NEE_calc_total_gC, 2),
            NEE_diff = round(NEE_diff, 2),
            NEE_diff_pct = round(NEE_diff_pct, 2)
          ))
  } else {
    cat("\n✓ Проверка пройдена: все расхождения между NEE_f и (Reco - GPP) в пределах 10%\n")
  }
  
  # Save summary table
  write.csv(cumulative_summary, "output_Kursk/cumulative_summary_final.csv", row.names = FALSE)
  cat("\n✓ Итоговая таблица сохранена: output_Kursk/cumulative_summary_final.csv\n")

  cat("\nComparative analysis completed.\n")

} else {
  cat("\nWarning: Moscow data file not found:", moscow_file, "\n")
  cat("Skipping comparative analysis.\n")
}

# =============================================================================
# 9. METEO PLOTS FROM kurskfilled.csv
# =============================================================================

meteo_file_kursk <- "kurskfilled.csv"
if (file.exists(meteo_file_kursk)) {
  meteo_raw <- data.table::fread(meteo_file_kursk, encoding = "UTF-8")
  names(meteo_raw) <- trimws(names(meteo_raw))
  nm <- names(meteo_raw)

  pick_first_present <- function(nm, candidates) {
    nm_lower <- tolower(nm)
    candidates_lower <- tolower(candidates)
    for (cand in candidates_lower) {
      idx <- which(nm_lower == cand)
      if (length(idx) > 0) return(nm[idx[1]])
    }
    NA_character_
  }

  to_num <- function(x) suppressWarnings(as.numeric(x))
  has_values <- function(x) sum(is.finite(x)) > 2
  gap_fill_by_doy <- function(doy, x) {
    ok <- is.finite(doy) & is.finite(x)
    if (sum(ok) < 2) return(x)
    x_filled <- x
    x_filled[!ok] <- approx(doy[ok], x[ok], doy[!ok], rule = 2)$y
    x_filled
  }
  parse_date_any <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    if (sum(is.finite(x_num)) > 0) {
      return(as.Date(x_num, origin = "1899-12-30"))
    }
    d1 <- suppressWarnings(as.Date(x))
    if (all(is.na(d1))) {
      d1 <- suppressWarnings(lubridate::ymd(x))
    }
    d1
  }
  convert_vpd_to_pa <- function(x) {
    if (!has_values(x)) return(x)
    v_q90 <- suppressWarnings(quantile(x, 0.9, na.rm = TRUE))
    if (!is.finite(v_q90)) return(x)
    if (v_q90 <= 20) return(x * 1000)  # kPa -> Pa
    if (v_q90 <= 200) return(x * 100)  # hPa -> Pa
    x
  }

  col_doy <- pick_first_present(nm, c("Doy", "DOY", "doy", "DayOfYear", "dayofyear"))
  col_date <- pick_first_present(nm, c("date", "Date"))
  col_par <- pick_first_present(nm, c("PAR_f", "PPFD_f", "PPFD_f_mean", "PPFD_f_avg"))
  col_tair <- pick_first_present(nm, c("Tair_f", "TA_f", "Ta_f", "tair_f"))
  col_rh <- pick_first_present(nm, c("RH_f", "rH_f", "rh_f"))
  col_tsoil <- pick_first_present(nm, c("Tsoil_f", "TS_f", "tsoil_f"))
  col_swc <- pick_first_present(nm, c("SWC_f", "swc_f"))
  col_vpd <- pick_first_present(nm, c("VPD_f", "vpd_f"))
  col_precip <- pick_first_present(nm, c("Rain_mm_Tot_sums"))

  if (is.na(col_doy)) {
    stop("В kurskfilled.csv не найдена колонка дня года (Doy/DOY).")
  }

  missing_cols <- c(
    PAR_f = is.na(col_par),
    Tair_f = is.na(col_tair),
    RH_f = is.na(col_rh),
    Tsoil_f = is.na(col_tsoil),
    SWC_f = is.na(col_swc),
    VPD_f = is.na(col_vpd),
    Precip = is.na(col_precip)
  )

  if (any(missing_cols)) {
    cat("\n[meteo] Предупреждение: не найдены колонки:\n")
    cat(paste0("  - ", names(missing_cols)[missing_cols], collapse = "\n"), "\n")
  }

  meteo_kursk <- data.frame(
    Doy = to_num(meteo_raw[[col_doy]]),
    Date = if (!is.na(col_date)) parse_date_any(meteo_raw[[col_date]]) else as.Date(NA),
    PAR = if (!is.na(col_par)) to_num(meteo_raw[[col_par]]) else NA_real_,
    Tair = if (!is.na(col_tair)) to_num(meteo_raw[[col_tair]]) else NA_real_,
    RH = if (!is.na(col_rh)) to_num(meteo_raw[[col_rh]]) else NA_real_,
    Tsoil = if (!is.na(col_tsoil)) to_num(meteo_raw[[col_tsoil]]) else NA_real_,
    SWC = if (!is.na(col_swc)) to_num(meteo_raw[[col_swc]]) else NA_real_,
    VPD = if (!is.na(col_vpd)) to_num(meteo_raw[[col_vpd]]) else NA_real_,
    Precip = if (!is.na(col_precip)) to_num(meteo_raw[[col_precip]]) else NA_real_
  )

  meteo_kursk <- meteo_kursk %>%
    filter(is.finite(Doy)) %>%
    arrange(Doy)

  need_rh <- !has_values(meteo_kursk$RH)
  need_vpd <- !has_values(meteo_kursk$VPD)
  fill_rh <- any(!is.finite(meteo_kursk$RH))
  fill_vpd <- any(!is.finite(meteo_kursk$VPD))
  need_swc <- !has_values(meteo_kursk$SWC)
  need_tsoil <- !has_values(meteo_kursk$Tsoil)
  prefer_swc_20 <- TRUE
  prefer_tsoil_20 <- TRUE

  if (need_rh || need_vpd || fill_rh || fill_vpd || need_swc || need_tsoil ||
      prefer_swc_20 || prefer_tsoil_20) {
    meteo_half_file <- "Kursk_data_half_our.csv"
    if (file.exists(meteo_half_file)) {
      meteo_half_raw <- data.table::fread(meteo_half_file, encoding = "UTF-8")
      names(meteo_half_raw) <- trimws(names(meteo_half_raw))
      nm_half <- names(meteo_half_raw)

      col_dt <- pick_first_present(nm_half, c("DateTime", "datetime", "date_time", "timestamp"))
      col_rh_hh <- pick_first_present(nm_half, c("rH_f", "RH_f", "rh_f"))
      col_vpd_hh <- pick_first_present(nm_half, c("VPD_f", "vpd_f"))
      col_swc_20 <- pick_first_present(nm_half, c("SWC_avg_20cm_Avg", "swc_avg_20cm_avg"))
      col_swc_10 <- pick_first_present(nm_half, c("SWC_avg_10cm_Avg", "swc_avg_10cm_avg"))
      col_swc_50 <- pick_first_present(nm_half, c("SWC_avg_50cm_Avg", "swc_avg_50cm_avg"))
      col_ts_20 <- pick_first_present(nm_half, c("Ts_avg_20cm_Avg", "ts_avg_20cm_avg"))
      col_ts_50 <- pick_first_present(nm_half, c("Ts_avg_50cm_Avg", "ts_avg_50cm_avg"))

      col_swc_use <- if (!is.na(col_swc_20)) col_swc_20 else if (!is.na(col_swc_10)) col_swc_10 else col_swc_50
      col_ts_use <- if (!is.na(col_ts_20)) col_ts_20 else col_ts_50

      if (is.na(col_swc_20) && !is.na(col_swc_use)) {
        cat(sprintf("\n[meteo] SWC_avg_20cm_Avg not found, using %s.\n", col_swc_use))
      }
      if (is.na(col_ts_20) && !is.na(col_ts_use)) {
        cat(sprintf("\n[meteo] Ts_avg_20cm_Avg not found, using %s.\n", col_ts_use))
      }

      if (!is.na(col_dt) && (!is.na(col_rh_hh) || !is.na(col_vpd_hh) ||
                             !is.na(col_swc_use) || !is.na(col_ts_use))) {
        dt_raw <- meteo_half_raw[[col_dt]]
        dt_parsed <- suppressWarnings(lubridate::ymd_hms(dt_raw, tz = "UTC"))
        if (all(is.na(dt_parsed))) {
          dt_parsed <- suppressWarnings(lubridate::ymd_hm(dt_raw, tz = "UTC"))
        }

        meteo_half <- data.frame(
          DateTime = dt_parsed,
          RH = if (!is.na(col_rh_hh)) to_num(meteo_half_raw[[col_rh_hh]]) else NA_real_,
          VPD = if (!is.na(col_vpd_hh)) to_num(meteo_half_raw[[col_vpd_hh]]) else NA_real_,
          SWC = if (!is.na(col_swc_use)) to_num(meteo_half_raw[[col_swc_use]]) else NA_real_,
          Tsoil = if (!is.na(col_ts_use)) to_num(meteo_half_raw[[col_ts_use]]) else NA_real_
        )

        mean_or_na <- function(x) if (all(!is.finite(x))) NA_real_ else mean(x, na.rm = TRUE)

        meteo_half_daily <- meteo_half %>%
          filter(!is.na(DateTime)) %>%
          mutate(Doy = yday(DateTime)) %>%
          group_by(Doy) %>%
          summarise(
            Date_hh = min(as.Date(DateTime)),
            RH_hh = mean_or_na(RH),
            VPD_hh = mean_or_na(VPD),
            SWC_hh = mean_or_na(SWC),
            Tsoil_hh = mean_or_na(Tsoil),
            .groups = "drop"
          )

        meteo_kursk <- meteo_kursk %>%
          left_join(meteo_half_daily, by = "Doy") %>%
          mutate(
            Date = dplyr::coalesce(Date_hh, Date),
            RH = ifelse(is.finite(RH), RH, RH_hh),
            VPD = ifelse(is.finite(VPD), VPD, VPD_hh),
            SWC = ifelse(is.finite(SWC_hh), SWC_hh, SWC),
            Tsoil = ifelse(is.finite(Tsoil_hh), Tsoil_hh, Tsoil)
          ) %>%
          select(-Date_hh, -RH_hh, -VPD_hh, -SWC_hh, -Tsoil_hh)
      } else {
        cat("\n[meteo] Kursk_data_half_our.csv: нет DateTime или RH/VPD колонок для заполнения.\n")
      }
    } else {
      cat("\n[meteo] Файл Kursk_data_half_our.csv не найден для заполнения RH/VPD.\n")
    }
  }

  if (!has_values(meteo_kursk$PAR)) {
    reddy_file <- "Kursk_REddyProc_results_halfhourly.csv"
    if (file.exists(reddy_file)) {
      meteo_rp <- data.table::fread(reddy_file, encoding = "UTF-8")
      names(meteo_rp) <- trimws(names(meteo_rp))
      nm_rp <- names(meteo_rp)

      col_rg_f <- pick_first_present(nm_rp, c("Rg_f", "rg_f"))
      col_doy_rp <- pick_first_present(nm_rp, c("DoY", "DOY", "doy"))
      col_dt_rp <- pick_first_present(nm_rp, c("DateTime", "datetime", "date_time", "timestamp"))

      if (!is.na(col_rg_f) && (!is.na(col_doy_rp) || !is.na(col_dt_rp))) {
        rg_f <- to_num(meteo_rp[[col_rg_f]])
        if (!is.na(col_doy_rp)) {
          doy_vals <- to_num(meteo_rp[[col_doy_rp]])
        } else {
          dt_raw <- meteo_rp[[col_dt_rp]]
          dt_parsed <- suppressWarnings(lubridate::ymd_hms(dt_raw, tz = "UTC"))
          if (all(is.na(dt_parsed))) {
            dt_parsed <- suppressWarnings(lubridate::ymd_hm(dt_raw, tz = "UTC"))
          }
          doy_vals <- yday(dt_parsed)
        }

        ppfd_f <- bigleaf::Rg.to.PPFD(rg_f)
        meteo_ppfd_daily <- data.frame(Doy = doy_vals, PAR_f = ppfd_f) %>%
          filter(is.finite(Doy)) %>%
          group_by(Doy) %>%
          summarise(PAR_f = mean(PAR_f, na.rm = TRUE), .groups = "drop")

        meteo_kursk <- meteo_kursk %>%
          left_join(meteo_ppfd_daily, by = "Doy") %>%
          mutate(PAR = ifelse(is.finite(PAR), PAR, PAR_f)) %>%
          select(-PAR_f)
      } else {
        cat("\n[meteo] Kursk_REddyProc_results_halfhourly.csv: missing Rg_f or DoY/DateTime for PPFD_f.\n")
      }
    } else {
      cat("\n[meteo] Kursk_REddyProc_results_halfhourly.csv not found for PPFD_f.\n")
    }
  }

  meteo_kursk <- meteo_kursk %>%
    mutate(
      Tsoil = gap_fill_by_doy(Doy, Tsoil),
      SWC = gap_fill_by_doy(Doy, SWC)
    )

  if (has_values(meteo_kursk$SWC)) {
    swc_max <- suppressWarnings(max(meteo_kursk$SWC, na.rm = TRUE))
    if (is.finite(swc_max) && swc_max <= 1.5) {
      meteo_kursk$SWC <- meteo_kursk$SWC * 100
    }
  }

  if (has_values(meteo_kursk$VPD)) {
    meteo_kursk$VPD <- convert_vpd_to_pa(meteo_kursk$VPD)
  }

  par_min_cut <- 400
  roll_window_days <- 7
  roll_mean <- function(x) {
    n <- length(x)
    out <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      start <- max(1, i - roll_window_days + 1)
      out[i] <- mean(x[start:i], na.rm = TRUE)
    }
    out
  }

  meteo_kursk <- meteo_kursk %>%
    mutate(
      PAR_plot = ifelse(is.finite(PAR) & PAR >= par_min_cut, PAR, NA_real_),
      PAR_roll = roll_mean(PAR_plot),
      Tair_roll = roll_mean(Tair),
      RH_roll = roll_mean(RH),
      Tsoil_roll = roll_mean(Tsoil),
      SWC_roll = roll_mean(SWC),
      VPD_roll = roll_mean(VPD)
    )

  use_date_axis <- any(!is.na(meteo_kursk$Date))
  x_var <- if (use_date_axis) "Date" else "Doy"
  x_lab <- if (use_date_axis) "Дата" else "День года"
  x_scale <- if (use_date_axis) {
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d.%m")
  } else {
    doy_min <- floor(min(meteo_kursk$Doy, na.rm = TRUE))
    doy_max <- ceiling(max(meteo_kursk$Doy, na.rm = TRUE))
    scale_x_continuous(
      limits = c(doy_min, doy_max),
      breaks = scales::pretty_breaks(n = 8)
    )
  }

  if (has_values(meteo_kursk$PAR_plot)) {
    p_par <- ggplot(filter(meteo_kursk, is.finite(PAR_plot)), aes(x = .data[[x_var]])) +
      geom_point(aes(y = PAR_plot), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = PAR_roll), color = "darkgreen", linewidth = 0.7, na.rm = TRUE) +
      labs(
        y = "ФАР, мкмоль м^-2 с^-1",
        x = x_lab
      ) +
      x_scale +
      theme_bw()
    ggsave("Kursk_meteo_PAR.png", plot = p_par, width = 12, height = 4, dpi = 300)
  }

  if (has_values(meteo_kursk$Tair)) {
    p_tair <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_point(aes(y = Tair, color = "Температура воздуха"), size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tair_roll, color = "Температура воздуха"), linewidth = 0.7, na.rm = TRUE) +
      scale_color_manual(name = "", values = c("Температура воздуха" = "#D55E00")) +
      labs(y = "Температура воздуха, °C", x = x_lab) +
      x_scale +
      theme_bw() +
      theme(legend.position = "top", legend.background = element_blank())
    ggsave("Kursk_meteo_Tair.png", plot = p_tair, width = 12, height = 4, dpi = 300)
  }

  if (has_values(meteo_kursk$Tsoil)) {
    p_tsoil <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_point(aes(y = Tsoil, color = "Температура почвы"), size = 0.35, alpha = 0.35) +
      geom_line(aes(y = Tsoil_roll, color = "Температура почвы"), linewidth = 0.7, na.rm = TRUE) +
      scale_color_manual(name = "", values = c("Температура почвы" = "#000000")) +
      labs(y = "Температура почвы, °C", x = x_lab) +
      x_scale +
      theme_bw() +
      theme(legend.position = "top", legend.background = element_blank())
    ggsave("Kursk_meteo_Tsoil.png", plot = p_tsoil, width = 12, height = 4, dpi = 300)
  }

  if (has_values(meteo_kursk$SWC)) {
    p_swc <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_point(aes(y = SWC), color = "grey50", size = 0.35, alpha = 0.35, na.rm = TRUE) +
      geom_line(aes(y = SWC_roll), color = "#009E73", linewidth = 0.7, na.rm = TRUE) +
      labs(y = "Влажность почвы, %", x = x_lab) +
      x_scale +
      theme_bw()
    ggsave("Kursk_meteo_SWC.png", plot = p_swc, width = 12, height = 4, dpi = 300)
  }

  if (has_values(meteo_kursk$VPD)) {
    p_vpd <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_point(aes(y = VPD), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = VPD_roll), color = "#0072B2", linewidth = 0.7, na.rm = TRUE) +
      labs(y = "Дефицит давления пара (VPD), Па", x = x_lab) +
      x_scale +
      theme_bw()
    ggsave("Kursk_meteo_VPD.png", plot = p_vpd, width = 12, height = 4, dpi = 300)
  }

  if (has_values(meteo_kursk$Precip) && has_values(meteo_kursk$RH)) {
    rh_max <- suppressWarnings(max(meteo_kursk$RH, na.rm = TRUE))
    precip_max <- suppressWarnings(max(meteo_kursk$Precip, na.rm = TRUE))
    rh_sf <- if (is.finite(rh_max) && rh_max > 0 && is.finite(precip_max) && precip_max > 0) {
      precip_max / rh_max
    } else {
      1
    }

    p_rh_precip <- ggplot() +
      geom_col(
        data = meteo_kursk,
        aes(x = .data[[x_var]], y = Precip, fill = "Осадки"),
        width = 0.9,
        alpha = 0.85
      ) +
      geom_point(
        data = meteo_kursk,
        aes(x = .data[[x_var]], y = RH * rh_sf, color = "Относительная влажность"),
        size = 0.35,
        alpha = 0.35
      ) +
      geom_line(
        data = meteo_kursk,
        aes(x = .data[[x_var]], y = RH_roll * rh_sf, color = "Относительная влажность"),
        linewidth = 0.7,
        na.rm = TRUE
      ) +
      scale_y_continuous(
        name = "Осадки, мм/день",
        sec.axis = sec_axis(~ . / rh_sf, name = "Относительная влажность, %")
      ) +
      scale_fill_manual(NULL, values = c("Осадки" = "#92C5DE")) +
      scale_color_manual(NULL, values = c("Относительная влажность" = "grey30")) +
      labs(x = x_lab) +
      x_scale +
      theme_bw() +
      theme(legend.position = "bottom", legend.background = element_blank())

    ggsave("Kursk_meteo_RH_Precip.png", plot = p_rh_precip, width = 12, height = 4, dpi = 300)
  } else if (has_values(meteo_kursk$Precip)) {
    p_precip <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_col(aes(y = Precip), fill = "#92C5DE", width = 0.9, alpha = 0.85) +
      labs(y = "Осадки, мм/день", x = x_lab) +
      x_scale +
      theme_bw()
    ggsave("Kursk_meteo_Precip.png", plot = p_precip, width = 12, height = 4, dpi = 300)
  } else if (has_values(meteo_kursk$RH)) {
    p_rh <- ggplot(meteo_kursk, aes(x = .data[[x_var]])) +
      geom_point(aes(y = RH), color = "grey50", size = 0.35, alpha = 0.35) +
      geom_line(aes(y = RH_roll), color = "grey30", linewidth = 0.7, na.rm = TRUE) +
      labs(y = "Относительная влажность, %", x = x_lab) +
      x_scale +
      theme_bw()
    ggsave("Kursk_meteo_RH.png", plot = p_rh, width = 12, height = 4, dpi = 300)
  }
} else {
  cat("\n[meteo] Файл kurskfilled.csv не найден. Построение метеографиков пропущено.\n")
}
