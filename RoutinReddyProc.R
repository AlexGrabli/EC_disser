doReddyProcRoutine  = function(data,
  station_name,
  input_vars=c("NEE","Tair","rH","LE","H","Ustar","VPD","Rg","PPFD","Rn","Tsoil"),
  LatD, LongD,TZ,
  seasons = c(120,150,240),
  year,
  ustar_probs=c(0.05, 0.5, 0.95),
  Keenan = F,
  plots_dir = NULL,
  export = TRUE,
  ustarScenarios.df = NULL 
   ){

# --- ИСПРАВЛЕНИЕ: Используем новые имена аргументов (Data, ColNames) и добавляем ID ---
EdP = sEddyProc$new(station_name,
  Data = data, 
  ColNames = input_vars)
  EdP$sSetLocationInfo(LatDeg = LatD,
  LongDeg = LongD,
  TimeZoneHour = TZ)

uStarSuffixes = sprintf("U%.2d",c(0.05, 0.5, 0.95)*100)
  
EdP$sMDSGapFill('Rg', FillAll = FALSE)
EdP$sCalcPotRadiation(useSolartime = TRUE)
EdP$sMDSGapFill('Rn', FillAll = FALSE)
EdP$sMDSGapFill('PPFD', FillAll = FALSE)
EdP$sMDSGapFill('Tsoil', FillAll = FALSE)
EdP$sMDSGapFill('Tair', FillAll = FALSE)
EdP$sMDSGapFill('VPD', FillAll = FALSE)
EdP$sMDSGapFill('LE', FillAll = FALSE)
EdP$sMDSGapFill('H', FillAll = FALSE)

seasonStarts = data.frame(V1 = seasons, V2 = year)
seasonFactor <- usCreateSeasonFactorYdayYear(
  data$DateTime - 15*60, starts = seasonStarts)
EdP$sSetUStarSeasons(seasonFactor)

if (!is.null(ustarScenarios.df)) {
  # Теперь, когда сезоны определены, можно установить заранее рассчитанные пороги
  EdP$sSetUstarScenarios(ustarScenarios.df)
} else {
  # Иначе, рассчитываем пороги с нуля, используя тот же сезонный фактор
  EdP$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 100L, probs = ustar_probs)
}

# Последующие функции теперь будут использовать корректно установленные сценарии u*.
EdP$sMDSGapFillUStarScens("NEE")

EdP$sMRFluxPartitionUStarScens()

if(Keenan){EdP$sTKFluxPartitionUStarScens()} else {EdP$sGLFluxPartitionUStarScens()}

if(!is.null(plots_dir)){
EdP$sPlotHHFluxes("GPP_DT_U50", Dir=plots_dir)
EdP$sPlotHHFluxes("Reco_DT_U50", Dir=plots_dir)
}

if(export){
FilledEddyData = cbind(EdP$sExportData()[[1]],EdP$sExportResults())
# Приводим имя столбца к стандартному виду
names(FilledEddyData)[1] <- "DateTime"
FilledEddyData <- FilledEddyData %>%
  rename(timestamp = DateTime) %>%
  tidyr::fill(starts_with("FP"),.direction="downup")
return(FilledEddyData)
} else {
return(EdP)
}
}


doETSepRoutine = function(data, reddy){
FilledEddyData = data

# Проверяем наличие необходимых столбцов в 'reddy', чтобы избежать ошибок
required_cols <- c("Pa", "WS", "CO2", "Ustar")
if (!all(required_cols %in% names(reddy))) {
missing_cols <- required_cols[!required_cols %in% names(reddy)]
message(paste("Required columns missing in 'reddy' for ET partitioning:", paste(missing_cols, collapse = ", ")))
return(FilledEddyData)
}

FilledEddyData$P = reddy$Pa
FilledEddyData$Pa_kPa = reddy$Pa/1000
FilledEddyData$WS = reddy$WS
FilledEddyData$CO2 = reddy$CO2
FilledEddyData$Ustar = reddy$Ustar
FilledEddyData$ETkg  = bigleaf::LE.to.ET(FilledEddyData$LE_f, 
              FilledEddyData$Tair_f) #kg m-2 s-1

#Пропускаем разделение ET, если GPP недоступен
if (!("GPP_DT_U50" %in% names(FilledEddyData)) || !("VPD_f" %in% names(FilledEddyData))) {
message("GPP_DT_U50 or VPD_f not found, skipping ET partitioning.")
return(FilledEddyData)
}

WUEo = ETpartitioning::calculate_WUE_o(
data=FilledEddyData
,ColPhotos="GPP_DT_U50"
,ColVPD="VPD_f"
,ColTair="Tair_f"
,C = 1.189
,Z = 0.17
)
Chio = ETpartitioning::calculate_chi_o(
data=FilledEddyData
,ColPhotos="GPP_DT_U50"
,ColVPD="VPD_f"
,ColTair="Tair_f"
,C = 1.189
,Z = 0.17
)

par_etsep =  ETpartitioning::optimal_parameters(par_lower = c(0, 0, 10, 0)
,par_upper = c(400,0.4, 30, 1)
,data = FilledEddyData 
,ColPhotos = "GPP_DT_U50"              # umol CO2 m-2 s-1
,ColPhotos_unc = "GPP_DT_U50_SD"       # umol CO2 m-2 s-1
,ColH = "H_f"                           # w+1 m-2
,ColQ = "PPFD_f"                       # umol photons m-2 s-1
,ColVPD = "VPD_f"                    # hPa
,ColTair = "Tair_f"                  # C
,ColPair = "Pa_kPa"                     # kPa                         # umol m-2 s-1
,ColCa = "CO2"                          # umol Co2 mol air-1)
,ColUstar = "Ustar"                     # W m-2
,ColWS = "WS"                           # m s-1
,ColSW_in = "Rg_f"                      # W m-2
,Chi_o = Chio
,WUE_o = WUEo)

Transp = ETpartitioning::transpiration_model(
par = par_etsep %>% as.numeric
,data = FilledEddyData 
,ColPhotos = "GPP_DT_U50"
,ColH = "H_f"
,ColVPD = "VPD_f"
,ColTair = "Tair_f"
,ColPair = "Pa_kPa"
,ColQ = "PPFD_f"
,ColCa = "CO2"
,ColUstar = "Ustar"
,ColWS = "WS"
,ColSW_in = "Rg_f"
,Chi_o = Chio
,WUE_o = WUEo
) # umol h2o s-1m-2
FilledEddyData$Transp = as.numeric(Transp)
FilledEddyData$ETmmol = FilledEddyData$ETkg/18.01528*1000*1000
FilledEddyData$Trnsp = case_when(FilledEddyData$Transp > 
           FilledEddyData$ETmmol ~ FilledEddyData$ETmmol,
         TRUE ~ FilledEddyData$Transp)
FilledEddyData$Evapmmol = FilledEddyData$ETmmol - FilledEddyData$Trnsp
FilledEddyData$Evaporation = FilledEddyData$Evapmmol
FilledEddyData$ET = FilledEddyData$ETmmol
FilledEddyData$Transpiraton = FilledEddyData$Trnsp

FilledEddyData$TET = FilledEddyData$Transp/FilledEddyData$ETmmol
FilledEddyData$TET[FilledEddyData$TET<0]=0
FilledEddyData$TET[FilledEddyData$TET>1]=1
FilledEddyData$NEE = FilledEddyData$NEE_uStar_orig
FilledEddyData$VPD_kPa = FilledEddyData$VPD_f/10

Ga = bigleaf::aerodynamic.conductance(FilledEddyData,
              Tair = "Tair_f",
              pressure = "Pa_kPa",
              wind = "WS",
              ustar = "Ustar",
              H = "H_f",
              Rb_model="Thom_1972")
FilledEddyData = cbind(FilledEddyData,Ga)
FilledEddyData = FilledEddyData %>% mutate(G = Rn_f - LE_f - H_f)
Gs_PM = bigleaf::surface.conductance(FilledEddyData,
             Tair="Tair_f",
             pressure="Pa_kPa",
             Rn="Rn_f",
             G="G",
             S=NULL,
             LE = "LE_f",
             VPD="VPD_kPa",
             Ga="Ga_h",
             formulation="Penman-Monteith")

FilledEddyData = cbind(FilledEddyData,Gs_PM)

PET = bigleaf::potential.ET(FilledEddyData,
    Tair = "Tair_f",
    pressure = "Pa_kPa",
    VPD = "VPD_kPa",
    Ga = "Ga_h",
    G = "G",
    Rn = "Rg_f",
    approach="Penman-Monteith")

FilledEddyData = cbind(FilledEddyData, PET)

return(FilledEddyData)
}
