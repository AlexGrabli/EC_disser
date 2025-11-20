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

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load raw data from EddyPro output
kursk_data <- fread("Kursk_data_half_our.csv")

# Site coordinates (Kursk region - Obojan)
Lat_deg <- 51.14567
Long_deg <- 36.50624
TimeZone_h <- 0  # Data already in local time, no shift needed

# Parse DateTime
kursk_data$DateTime <- as.POSIXct(kursk_data$DateTime, format = "%Y-%m-%d %H:%M:%S")

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
    Tsoil = as.numeric(Tsoil)
  ) %>%
  select(DateTime, Year, DoY, Hour, NEE, LE, H, Ustar, Tair, Rg, VPD, rH, Tsoil) %>%
  as.data.frame()

# Replace -9999 and other missing value indicators with NA
EddyData[EddyData == -9999] <- NA
EddyData[EddyData == -999] <- NA

# Check for valid data ranges
EddyData$NEE[abs(EddyData$NEE) > 100] <- NA
EddyData$Ustar[EddyData$Ustar < 0 | EddyData$Ustar > 5] <- NA
EddyData$Rg[EddyData$Rg < 0] <- 0

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
  c("NEE", "LE", "H", "Rg", "Tair", "VPD", "Ustar", "rH", "Tsoil")  # Variables
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

# Add Tsoil and Hour from original data (not included in REddyProc output)
Results$Tsoil <- EddyData$Tsoil[1:nrow(Results)]
Results$Hour <- EddyData$Hour[1:nrow(Results)]

# Convert GPP and Reco to umol m-2 s-1 (they come in this unit)
# GPP_DT and Reco_DT are the partitioned fluxes from Lasslop method

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
    # Environmental means
    Tair_mean = mean(Tair_f, na.rm = TRUE),
    VPD_mean = mean(VPD_f, na.rm = TRUE),
    Rg_mean = mean(Rg_f, na.rm = TRUE),
    Tsoil_mean = mean(Tsoil, na.rm = TRUE),
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

# Hourly aggregation for diurnal patterns
Hourly <- Results %>%
  mutate(
    Month = month(DateTime),
    Hour = floor(Hour)
  ) %>%
  group_by(Month, Hour) %>%
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

# Set theme
theme_flux <- theme_few(base_size = 14, base_family = "serif") +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# -----------------------------------------------------------------------------
# 5.1 Diurnal Cycles by Month
# -----------------------------------------------------------------------------

plot_diurnal_NEE <- ggplot(Hourly, aes(x = Hour, y = NEE_mean)) +
  geom_ribbon(aes(ymin = NEE_mean - NEE_se, ymax = NEE_mean + NEE_se),
              alpha = 0.3, fill = "blue") +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~Month, ncol = 3) +
  labs(
    x = "Hour of day",
    y = expression(bold("NEE")~"("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal NEE Cycle by Month (Lasslop Partitioning)"
  ) +
  theme_flux

plot_diurnal_GPP <- ggplot(Hourly, aes(x = Hour, y = GPP_mean)) +
  geom_ribbon(aes(ymin = GPP_mean - GPP_sd/sqrt(10), ymax = GPP_mean + GPP_sd/sqrt(10)),
              alpha = 0.3, fill = "darkgreen") +
  geom_line(size = 1, color = "darkgreen") +
  geom_point(size = 2, shape = 21, fill = "white") +
  facet_wrap(~Month, ncol = 3) +
  labs(
    x = "Hour of day",
    y = expression(bold("GPP")~"("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal GPP Cycle by Month"
  ) +
  theme_flux

plot_diurnal_Reco <- ggplot(Hourly, aes(x = Hour, y = Reco_mean)) +
  geom_ribbon(aes(ymin = Reco_mean - Reco_sd/sqrt(10), ymax = Reco_mean + Reco_sd/sqrt(10)),
              alpha = 0.3, fill = "brown") +
  geom_line(size = 1, color = "brown") +
  geom_point(size = 2, shape = 21, fill = "white") +
  facet_wrap(~Month, ncol = 3) +
  labs(
    x = "Hour of day",
    y = expression(bold("Reco")~"("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Diurnal Ecosystem Respiration Cycle by Month"
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
# 5.5 Light Response Curves
# -----------------------------------------------------------------------------

# Prepare half-hourly data for light curves
LightCurve_data <- Results %>%
  filter(Rg_f > 10) %>%  # Daytime only
  mutate(
    Rg_bin = cut(Rg_f, breaks = seq(0, max(Rg_f, na.rm = TRUE) + 100, by = 100)),
    Month = month(DateTime)
  )

# Light curve for the whole season
plot_light_curve <- ggplot(Results %>% filter(Rg_f > 10 & !is.na(NEE_f)),
                           aes(x = Rg_f, y = -NEE_f)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "nls",
              formula = y ~ (a * x) / (b + x),
              method.args = list(start = list(a = 30, b = 200)),
              se = FALSE, color = "red", size = 1.5) +
  labs(
    x = expression(bold("Rg")~"(W"~m^{-2}*")"),
    y = expression(bold("-NEE")~"("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Light Response Curve (All Season)"
  ) +
  theme_flux

# Light curves by month
plot_light_curve_monthly <- ggplot(Results %>%
                                     filter(Rg_f > 10 & !is.na(NEE_f)) %>%
                                     mutate(Month = month(DateTime)),
                                   aes(x = Rg_f, y = -NEE_f)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "loess", span = 0.5, color = "red", se = FALSE) +
  facet_wrap(~Month, ncol = 3) +
  labs(
    x = expression("Rg (W"~m^{-2}*")"),
    y = expression("-NEE ("*mu*"mol"~CO[2]~m^{-2}~s^{-1}*")"),
    title = "Light Response Curves by Month"
  ) +
  theme_flux

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

# Create output directory
dir.create("output_Kursk", showWarnings = FALSE)

# Save processed data
write.csv(Results, "output_Kursk/Kursk_REddyProc_results_halfhourly.csv", row.names = FALSE)
write.csv(Daily, "output_Kursk/Kursk_REddyProc_results_daily.csv", row.names = FALSE)

# Save plots
ggsave("output_Kursk/diurnal_NEE.png", plot_diurnal_NEE, width = 12, height = 10, dpi = 300)
ggsave("output_Kursk/diurnal_GPP.png", plot_diurnal_GPP, width = 12, height = 10, dpi = 300)
ggsave("output_Kursk/diurnal_Reco.png", plot_diurnal_Reco, width = 12, height = 10, dpi = 300)
ggsave("output_Kursk/daily_fluxes.png", plot_daily_fluxes, width = 14, height = 6, dpi = 300)
ggsave("output_Kursk/cumulative_fluxes.png", plot_cumulative, width = 12, height = 6, dpi = 300)
ggsave("output_Kursk/WUE_seasonal.png", plot_WUE, width = 10, height = 6, dpi = 300)
ggsave("output_Kursk/WUE_vs_VPD.png", plot_WUE_VPD, width = 10, height = 6, dpi = 300)
ggsave("output_Kursk/light_curve.png", plot_light_curve, width = 10, height = 8, dpi = 300)
ggsave("output_Kursk/light_curve_monthly.png", plot_light_curve_monthly, width = 12, height = 10, dpi = 300)
ggsave("output_Kursk/Reco_temperature.png", plot_Reco_temp, width = 10, height = 8, dpi = 300)

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

# Display plots
print(plot_diurnal_NEE)
print(plot_daily_fluxes)
print(plot_cumulative)
print(plot_WUE)
print(plot_light_curve)
