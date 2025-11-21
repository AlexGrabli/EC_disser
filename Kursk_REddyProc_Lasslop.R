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

# Define phenophase boundaries by DoY (default values for Kursk 2013)
# These will be overwritten if found in xlsx
phase_bounds <- data.frame(
  Phase_ru = factor(PHASE_RU, levels = PHASE_RU),
  Phase_en = factor(PHASE_EN, levels = PHASE_EN),
  DoY_start = c(133, 145, 160, 175, 185, 200)  # Default values, adjust as needed
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

# Add DoY and assign phenophases
Results$DoY <- yday(Results$DateTime)
Results$Phase_ru <- sapply(Results$DoY, assign_phase)
Results$Phase_ru <- factor(Results$Phase_ru, levels = PHASE_RU)
Results$Phase_en <- factor(Results$Phase_ru, levels = PHASE_RU, labels = PHASE_EN)

# Convert Rg to PPFD using bigleaf (for light curves with PPFD)
Results$PPFD <- bigleaf::Rg.to.PPFD(Results$Rg_f)

# Calculate E (evapotranspiration in mmol m-2 s-1) from LE
# LE in W m-2, convert to mmol H2O m-2 s-1
Results$E_mmol <- LE.to.ET(Results$LE_f, Results$Tair_f) * 1000 / 18.015  # kg to mmol

# Calculate instantaneous WUE and IWUE
Results$WUE_inst <- ifelse(Results$E_mmol > 0 & !is.na(Results$GPP_DT),
                           Results$GPP_DT / Results$E_mmol, NA)
Results$IWUE <- ifelse(Results$E_mmol > 0 & !is.na(Results$GPP_DT) & Results$VPD_f > 0,
                       Results$GPP_DT * Results$VPD_f / Results$E_mmol, NA)

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

# Create output directory
dir.create("output_Kursk", showWarnings = FALSE)

# Save processed data
write.csv(Results, "output_Kursk/Kursk_REddyProc_results_halfhourly.csv", row.names = FALSE)
write.csv(Daily, "output_Kursk/Kursk_REddyProc_results_daily.csv", row.names = FALSE)

# Save plots
ggsave("output_Kursk/diurnal_NEE.png", plot_diurnal_NEE, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output_Kursk/diurnal_GPP.png", plot_diurnal_GPP, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output_Kursk/diurnal_Reco.png", plot_diurnal_Reco, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output_Kursk/daily_fluxes.png", plot_daily_fluxes, width = 14, height = 6, dpi = 300, bg = "white")
ggsave("output_Kursk/cumulative_fluxes.png", plot_cumulative, width = 12, height = 6, dpi = 300, bg = "white")
ggsave("output_Kursk/WUE_seasonal.png", plot_WUE, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("output_Kursk/WUE_vs_VPD.png", plot_WUE_VPD, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("output_Kursk/light_curve_by_phase.png", plot_light_curve_phase, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("output_Kursk/Reco_temperature.png", plot_Reco_temp, width = 10, height = 8, dpi = 300, bg = "white")

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
print(plot_diurnal_GPP)
print(plot_diurnal_Reco)
print(plot_daily_fluxes)
print(plot_cumulative)
print(plot_WUE)
print(plot_light_curve_phase)
