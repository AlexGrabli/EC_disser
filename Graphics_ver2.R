install.packages(c("tidyverse", "dplyr", "ggpubr", "ggpmisc", "tsibble", "mgcv", "ggstatsplot"))
library(openeddy)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggpmisc)
library(REddyProc)
library(tsibble)
library(mgcv) 
library(ETpartitioning)
library(ggstatsplot)

#################НОВЫЙ ВАРИАНТ КОДА##############################
# --- ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ ---
# --- ГЛОБАЛЬНЫЕ НАСТРОЙКИ ---
# Переменная для управления размером шрифта на всех графиках
base_font_size <- 15
# Настройка цветов и типов линий для графиков
color_map_biomass <- c("high" = "#00BFC4", "low" = "#F8766D", "orig" = "#7CAE00")
linetype_map_biomass <- c("high" = "solid", "low" = "solid", "orig" = "dashed")

# --- ЕДИНАЯ ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ ---
# Чтение данных из CSV файлов
data_50 = read_csv("ИТОГ2_BarleyFilledAllScen_50p_biom_thrash_new2505.csv")
data_65 = read_csv("ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv")
data_75 = read_csv("ИТОГ2_BarleyFilledAllScen_75p_biom_thrash_new2505.csv")

# Добавление столбца с порогом
data_50$threshold = 50
data_65$threshold = 65
data_75$threshold = 75

# Объединение всех данных в один датафрейм
AllData = rbind(data_50, data_65, data_75)

# Расчет NEE, установка порядка фенологических фаз и добавление английских названий
# NEE = Reco - GPP. Положительные значения означают эмиссию CO2 в атмосферу.
english_levels <- c("Sowing", "Emergence", "Tillering", "Stem elongation", "Heading", "Flowering", "Ripening", "Harvesting")
AllData <- AllData %>%
  mutate(
    NEE_DT_U50 = Reco_DT_U50 - GPP_DT_U50,
    phase_ru = factor(phase_ru, levels = c("Посев","Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание","Уборка")),
    phase_en = factor(phase_ru, levels = levels(phase_ru), labels = english_levels)
  )

# --- ГРАФИКИ СУТОЧНОГО ХОДА (ПОРОГ 65%, БЕЗ 'orig') ---
plot_data_daily <- AllData %>% 
           filter(threshold == 65, biomass_f != "orig") %>%
           mutate(hour = as.numeric(hour(timestamp))) %>%
           filter(!phase_ru %in% c("Посев", "Уборка"))

color_mapping_daily <- c("high" = "#00AFBB", "low" = "#FC4E07")

# График по фенофазам Reco (на русском)
ggplot(plot_data_daily, aes(x = hour, y = Reco_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Часы суток") + ylab("Экосистемное дыхание, мкмоль CO₂ м⁻² с⁻¹") +
  facet_wrap(~phase_ru) +
  theme_bw(base_size = base_font_size)

# График по фенофазам GPP (на русском)
ggplot(plot_data_daily, aes(x = hour, y = GPP_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Часы суток") + ylab("Общая первичная продуктивность, мкмоль CO₂ м⁻² с⁻¹") +
  facet_wrap(~phase_ru) +
  theme_bw(base_size = base_font_size)

# График по фенофазам NEE (на русском)
ggplot(plot_data_daily, aes(x = hour, y = NEE_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Часы суток") + ylab("Чистый экосистемный обмен, мкмоль CO₂ м⁻² с⁻¹") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~phase_ru) +
  theme_bw(base_size = base_font_size)

####  Diurnal - подписи на английском
# График по фенофазам Reco (на английском)
ggplot(plot_data_daily, aes(x = hour, y = Reco_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Hour of day") + ylab("Reco, μmol CO₂ m⁻² s⁻¹") +
  facet_wrap(~phase) +
  theme_bw(base_size = base_font_size)

# График по фенофазам GPP (на английском)
ggplot(plot_data_daily, aes(x = hour, y = GPP_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Hour of day") + ylab("GPP, μmol CO₂ m⁻² s⁻¹") +
  facet_wrap(~phase) +
  theme_bw(base_size = base_font_size)

# График по фенофазам NEE (на английском)
ggplot(plot_data_daily, aes(x = hour, y = NEE_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") + stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping_daily) + scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Hour of day") + ylab("NEE, μmol CO₂ m⁻² s⁻¹") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~phase) +
  theme_bw(base_size = base_font_size)

# --- ГРАФИКИ ДИНАМИКИ СУТОЧНЫХ ПОТОКОВ (ПОРОГ 65%, С 'orig') ---
plot_data_65 <- AllData %>%
  filter(threshold == 65) %>%
  mutate(
    gC_per_interval_Reco = Reco_DT_U50 * 1800 * 12.01 / 1e6,
    gC_per_interval_GPP = GPP_DT_U50 * 1800 * 12.01 / 1e6,
    gC_per_interval_NEE = NEE_DT_U50 * 1800 * 12.01 / 1e6
  ) %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date, biomass_f) %>%
  summarise(
    Reco = sum(gC_per_interval_Reco, na.rm = TRUE),
    GPP = sum(gC_per_interval_GPP, na.rm = TRUE),
    NEE = sum(gC_per_interval_NEE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(day_of_season = as.numeric(difftime(date, min(date), units = "days")) + 1)

phase_data <- AllData %>%
  filter(threshold == 65) %>%
  group_by(phase_ru) %>%
  summarise(start_date = min(as.Date(timestamp)), .groups = 'drop') %>%
  mutate(day_of_season = as.numeric(difftime(start_date, min(plot_data_65$date), units = "days")) + 1) %>%
  arrange(day_of_season)

# График динамики GPP
ggplot(plot_data_65, aes(x = day_of_season, y = GPP, color = biomass_f)) +
  geom_smooth(aes(linetype = biomass_f), se = FALSE, span = 0.2, linewidth = 1) +
  geom_vline(data = phase_data, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7) +
  geom_text(data = phase_data, aes(x = day_of_season + 1, y = 8, label = phase_ru), inherit.aes = FALSE, angle = 90, hjust = 1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Плотность биомассы", values = color_map_biomass) +
  scale_linetype_manual(name = "Плотность биомассы", values = linetype_map_biomass) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(x = "День вегетации", y = expression(paste("GPP, г C ", м^-2, " ", день^-1))) +
  theme_classic(base_size = base_font_size) +
  theme(legend.position = "top")

# --- КУМУЛЯТИВНЫЕ ГРАФИКИ ПОТОКОВ (ПОРОГ 65%, С 'orig' И ФАЗАМИ) ---
cumulative_data <- AllData %>%
  filter(threshold == 65) %>%
  arrange(biomass_f, timestamp) %>%
  group_by(biomass_f) %>%
  mutate(
    day_of_season = as.numeric(difftime(timestamp, first(timestamp), units = "days")) + 1,
    cumulative_GPP = cumsum(GPP_DT_U50 * 1800 * 12.01 / 1e6),
    cumulative_Reco = cumsum(Reco_DT_U50 * 1800 * 12.01 / 1e6),
    cumulative_NEE = cumsum(NEE_DT_U50 * 1800 * 12.01 / 1e6)
  ) %>%
  ungroup()

phase_data_en <- AllData %>%
  filter(threshold == 65) %>%
  group_by(phase_en) %>%
  summarise(start_date = min(as.Date(timestamp)), .groups = 'drop') %>%
  mutate(day_of_season = as.numeric(difftime(start_date, min(cumulative_data$timestamp), units = "days")) + 1) %>%
  arrange(day_of_season)

# График кумулятивного GPP
ggplot(cumulative_data, aes(x = day_of_season, y = cumulative_GPP, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Cumulative GPP over the Growing Season", x = "Day of Growing Season", y = expression(paste("Cumulative GPP (g C ", m^-2, ")"))) +
  theme_bw(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# График кумулятивного Reco
ggplot(cumulative_data, aes(x = day_of_season, y = cumulative_Reco, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Cumulative Reco over the Growing Season", x = "Day of Growing Season", y = expression(paste("Cumulative Reco (g C ", m^-2, ")"))) +
  theme_bw(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# График кумулятивного NEE
ggplot(cumulative_data, aes(x = day_of_season, y = cumulative_NEE, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Cumulative NEE over the Growing Season", x = "Day of Growing Season", y = expression(paste("Cumulative NEE (g C ", m^-2, ")"))) +
  theme_bw(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# --- КУМУЛЯТИВНЫЕ ГРАФИКИ, НОРМАЛИЗОВАННЫЕ НА БИОМАССУ ---
cumulative_norm_data <- AllData %>%
    filter(threshold == 65) %>%
    filter(biomass_t65 > 0 & !is.na(biomass_t65)) %>%
    arrange(biomass_f, timestamp) %>%
    group_by(biomass_f) %>%
    mutate(
      specific_GPP_interval = (GPP_DT_U50 * 1800 * 12.01 / 1e6) / biomass_t65,
      specific_Reco_interval = (Reco_DT_U50 * 1800 * 12.01 / 1e6) / biomass_t65,
      specific_NEE_interval = (NEE_DT_U50 * 1800 * 12.01 / 1e6) / biomass_t65,
      cumulative_GPP_norm = cumsum(specific_GPP_interval),
      cumulative_Reco_norm = cumsum(specific_Reco_interval),
      cumulative_NEE_norm = cumsum(specific_NEE_interval),
      day_of_period = as.numeric(difftime(timestamp, first(timestamp), units = "days")) + 1
    ) %>%
    ungroup()

# График кумулятивного GPP, нормализованного на биомассу
ggplot(cumulative_norm_data, aes(x = day_of_period, y = cumulative_GPP_norm, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Specific Gross Primary Production", x = "Day of Growing Season", y = "Specific GPP (g C / g biomass)") +
  theme_classic(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# График кумулятивного Reco, нормализованного на биомассу
ggplot(cumulative_norm_data, aes(x = day_of_period, y = cumulative_Reco_norm, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Specific Ecosystem respiration", x = "Day of Growing Season", y = "Specific Reco (g C / g biomass)") +
  theme_classic(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

# График кумулятивного NEE, нормализованного на биомассу
ggplot(cumulative_norm_data, aes(x = day_of_period, y = cumulative_NEE_norm, color = biomass_f)) +
  geom_line(aes(linetype = biomass_f), linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(data = phase_data_en, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7, linetype = "dotted") +
  geom_text(data = phase_data_en, aes(x = day_of_season + 1, y = Inf, label = phase_en), inherit.aes = FALSE, angle = 90, vjust = 1.2, hjust = 1.1, color = "black", size = base_font_size / 2.5) +
  scale_color_manual(name = "Biomass Density", values = color_map_biomass) +
  scale_linetype_manual(name = "Biomass Density", values = linetype_map_biomass) +
  labs(title = "Specific Net Ecosystem Exchange", x = "Day of Growing Season", y = "Specific NEE (g C / g biomass)") +
  theme_classic(base_size = base_font_size) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")






##############    СТАРЫЙ КОД   #######################
data1 = read_csv("ИТОГ2_BarleyFilledAllScen_50p_biom_thrash_new2505.csv")
data2 = read_csv("ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv")
data3 = read_csv("ИТОГ2_BarleyFilledAllScen_75p_biom_thrash_new2505.csv")

data1$threshold = 50
data1 = data1 %>% filter(biomass_f != "orig")
data2$threshold = 65
data2 = data2 %>% filter(biomass_f != "orig")
data3$threshold = 75
data3 = data3 %>% filter(biomass_f != "orig")
FilledEddyData = rbind(data1,data2,data3)
FilledEddyData = FilledEddyData %>% mutate(phase_ru = factor(phase_ru,levels = 
                                                               c("Посев","Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание","Уборка")))
FilledEddyData <- FilledEddyData %>%
  mutate(across(
    starts_with("GPP_DT_U"),
    ~ .x * (12/44),
    .names = "{.col}_Cmass"
  ))

color_mapping <- c("high" = "#00AFBB", "low" = "#FC4E07")

# ЛУЧШИЙ ВАРИАНТ МОДИФИКАЦИИ: Подготовка данных для графиков
plot_data <- FilledEddyData %>% 
           mutate(hour = as.numeric(hour(timestamp))) %>%  # Убедимся, что час является числом
           filter(threshold == 65, !phase_ru %in% c("Посев", "Уборка"))

# График по фенофазам Reco, построенный с помощью ggplot2 для исправления ошибки
ggplot(plot_data, aes(x = hour, y = Reco_DT_U50, color = biomass_f)) +
  # МОДИФИКАЦИЯ: Убран аргумент fun.args, так как 95% CI - это значение по умолчанию
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Часы суток") + ylab("Экосистемное дыхание, мкмоль CO₂ м⁻² с⁻¹") +
  facet_wrap(~phase_ru) +
  theme_bw()

# График по фенофазам GPP, построенный с помощью ggplot2 для исправления ошибки
ggplot(plot_data, aes(x = hour, y = GPP_DT_U50, color = biomass_f)) +
  # МОДИФИКАЦИЯ: Убран аргумент fun.args, так как 95% CI - это значение по умолчанию
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  xlab("Часы суток") + ylab("Общая первичная продуктивность, мкмоль CO₂ м⁻² с⁻¹") +
  facet_wrap(~phase_ru) +
  theme_bw()


english_levels <- c("Sowing", "Emergence", "Tillering", "Stem elongation", "Heading", "Flowering", "Ripening", "Harvesting")
FilledEddyData <- FilledEddyData %>%
  mutate(phase_en = factor(phase_ru, levels = levels(phase_ru), labels = english_levels))

# МОДИФИКАЦИЯ: Определяем цвета для классов в соответствии с референсом
# Замените "high" и "low" на фактические значения в вашем столбце 'biomass_f', если они отличаются
# "high" -> бирюзовый, "low" -> красно-оранжевый
color_mapping <- c("high" = "#00AFBB", "low" = "#FC4E07")

# МОДИФИКАЦИЯ: Подготовка данных для графиков
plot_data <- FilledEddyData %>% 
           mutate(hour = as.numeric(hour(timestamp))) %>%  # Убедимся, что час является числом
           filter(threshold == 65, !phase_ru %in% c("Посев", "Уборка"))

# График по фенофазам Reco, построенный с помощью ggplot2
ggplot(plot_data, aes(x = hour, y = Reco_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  # МОДИФИКАЦИЯ: Обновлены подписи на английском
  xlab("Hour of day") + ylab("Reco, μmol C m⁻² s⁻¹") +
  facet_wrap(~phase_en) + # МОДИФИКАЦИЯ: Используем английские названия
  theme_bw()

# График по фенофазам GPP, построенный с помощью ggplot2
ggplot(plot_data, aes(x = hour, y = GPP_DT_U50, color = biomass_f)) +
  stat_summary(fun.data = "mean_ci", geom = "errorbar", width = 0.5) +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun = "mean", geom = "point") +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  # МОДИФИКАЦИЯ: Обновлены подписи на английском
  xlab("Hour of day") + ylab("GPP, μmol C m⁻² s⁻¹") +
  facet_wrap(~phase_en) + # МОДИФИКАЦИЯ: Используем английские названия
  theme_bw()

# Подготовка данных для кумулятивных графиков
# Предполагается, что данные имеют 30-минутный интервал (1800 секунд)
cumulative_data <- FilledEddyData %>%
  # Сортируем по типу биомассы и времени для корректного расчета кумулятивной суммы
  arrange(biomass_f, timestamp) %>%
  # Группируем по типу биомассы
  group_by(biomass_f) %>%
  # МОДИФИКАЦИЯ: Добавляем столбец с днем вегетационного периода
  mutate(
    # Вычисляем дни с начала периода для каждой группы
    day_of_season = as.numeric(difftime(timestamp, first(timestamp), units = "days")) + 1,
    # Пересчитываем поток в гC/м² за интервал и считаем кумулятивную сумму
    cumulative_GPP = cumsum(GPP_DT_U50 * 1800 * 12.01 / 1e6),
    cumulative_Reco = cumsum(Reco_DT_U50 * 1800 * 12.01 / 1e6)
  ) %>%
  # Разгруппировываем для дальнейшего использования в ggplot
  ungroup()

# График кумулятивного GPP
ggplot(cumulative_data, aes(x = day_of_season, y = cumulative_GPP, color = biomass_f)) +
  # МОДИФИКАЦИЯ: 'size' заменено на 'linewidth' для соответствия ggplot2 > 3.4.0
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    name = "Biomass Density", 
    values = color_mapping
  ) +
  # МОДИФИКАЦИЯ: Обновлена подпись оси X
  labs(
    title = "Cumulative GPP over the Growing Season",
    x = "Day of Growing Season",
    y = expression(paste("Cumulative GPP (g C ", m^-2, ")"))
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# График кумулятивного Reco
ggplot(cumulative_data, aes(x = day_of_season, y = cumulative_Reco, color = biomass_f)) +
  # МОДИФИКАЦИЯ: 'size' заменено на 'linewidth' для соответствия ggplot2 > 3.4.0
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    name = "Biomass Density", 
    values = color_mapping
  ) +
  # МОДИФИКАЦИЯ: Обновлена подпись оси X
  labs(
    title = "Cumulative Reco over the Growing Season",
    x = "Day of Growing Season",
    y = expression(paste("Cumulative Reco (g C ", m^-2, ")"))
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# --- НОВЫЙ РАЗДЕЛ: ГРАФИК ДИНАМИКИ ПОТОКОВ ---
# Добавляем столбец с порогом
data_50 = read_csv("ИТОГ2_BarleyFilledAllScen_50p_biom_thrash_new2505.csv")
data_65 = read_csv("ИТОГ2_BarleyFilledAllScen_65p_biom_thrash_new2505.csv")
data_75 = read_csv("ИТОГ2_BarleyFilledAllScen_75p_biom_thrash_new2505.csv")

data_50$threshold = 50
data_65$threshold = 65
data_75$threshold = 75

# Объединяем все данные в один датафрейм
AllData = rbind(data_50, data_65, data_75)

# Установка порядка фенологических фаз
AllData <- AllData %>% mutate(phase_ru = factor(phase_ru, levels = 
                                             c("Посев", "Всходы","Кущение","Выход в трубку","Колошение","Цветение","Созревание","Уборка")))

english_levels <- c("Sowing", "Emergence", "Tillering", "Stem elongation", "Heading", "Flowering", "Ripening", "Harvesting")
AllData <- AllData %>%
  mutate(phase_en = factor(phase_ru, levels = levels(phase_ru), labels = english_levels))


# --- НОВЫЙ РАЗДЕЛ: ГРАФИКИ ДИНАМИКИ ПОТОКОВ ПО НОВОМУ РЕФЕРЕНСУ ---

# 1. Фильтруем данные для порога 65% и готовим их к построению
# ИСПРАВЛЕННАЯ ЛОГИКА ПЕРЕСЧЕТА
plot_data_65 <- AllData %>%
  filter(threshold == 65) %>%
  # Сначала конвертируем каждое 30-минутное измерение в граммы C за этот интервал
  mutate(
    # Коэффициент для 30-минутного (1800 с) интервала
    gC_per_interval_Reco = Reco_DT_U50 * 1800 * 12.01 / 1e6,
    gC_per_interval_GPP = GPP_DT_U50 * 1800 * 12.01 / 1e6
  ) %>%
  # Добавляем столбец с датой для группировки
  mutate(date = as.Date(timestamp)) %>%
  # Группируем по дате и типу биомассы
  group_by(date, biomass_f) %>%
  # Суммируем все интервалы за день, чтобы получить г/м²/день
  summarise(
    Reco = sum(gC_per_interval_Reco, na.rm = TRUE),
    GPP = sum(gC_per_interval_GPP, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Добавляем день вегетационного периода для оси X
  mutate(day_of_season = as.numeric(difftime(date, min(date), units = "days")) + 1)


# 2. Подготовка данных для отметок фенологических фаз
phase_data <- AllData %>%
  filter(threshold == 65) %>%
  group_by(phase_ru) %>% # Группируем по английским названиям
  summarise(start_date = min(as.Date(timestamp)), .groups = 'drop') %>%
  # Вычисляем день на основе общей минимальной даты из отфильтрованных данных
  mutate(day_of_season = as.numeric(difftime(start_date, min(plot_data_65$date), units = "days")) + 1) %>%
  arrange(day_of_season)

# 3. Настройка цветов, как на референсе
color_map_biomass <- c("high" = "#00BFC4", "low" = "#F8766D", "orig" = "#7CAE00")

# --- ГРАФИК ДЛЯ RECO ---
ggplot(plot_data_65, aes(x = day_of_season, y = Reco, color = biomass_f)) +
  geom_smooth(se = FALSE, span = 0.2, linewidth = 1) +
  geom_vline(data = phase_data, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7) +
  geom_text(
    data = phase_data,
    aes(x = day_of_season + 1, y = 6, label = phase_ru),
    inherit.aes = FALSE, angle = 90, hjust = 1, color = "black", size = 4.5
  ) +
  scale_color_manual(name = "Плотность биомассы", values = color_map_biomass) +
  coord_cartesian(ylim = c(0, 6)) +
  labs(
    x = "День вегетации",
    y = expression(paste("Reco, г C ", м^-2, " ", день^-1))
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# --- ГРАФИК ДЛЯ GPP ---
ggplot(plot_data_65, aes(x = day_of_season, y = GPP, color = biomass_f)) +
  geom_smooth(se = FALSE, span = 0.2, linewidth = 1) +
  geom_vline(data = phase_data, aes(xintercept = day_of_season), color = "gray60", linewidth = 0.7) +
  geom_text(
    data = phase_data,
    aes(x = day_of_season + 1, y = 8, label = phase_ru),
    inherit.aes = FALSE, angle = 90, hjust = 1, color = "black", size = 4.5
  ) +
  scale_color_manual(name = "Плотность биомассы", values = color_map_biomass) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(
    x = "День вегетации",
    y = expression(paste("GPP, г C ", м^-2, " ", день^-1))
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# --- НОВЫЙ РАЗДЕЛ: КУМУЛЯТИВНЫЕ ГРАФИКИ, НОРМАЛИЗОВАННЫЕ НА БИОМАССУ ---

# 2.1 Подготовка данных: ИСПРАВЛЕННАЯ ЛОГИКА
cumulative_norm_data <- AllData %>%
    filter(threshold == 65) %>%
    # Убираем строки, где биомасса равна 0 или NA, чтобы избежать деления на ноль
    filter(biomass_t65 > 0 & !is.na(biomass_t65)) %>%
    # Сортируем данные для корректного расчета кумулятивной суммы
    arrange(biomass_f, timestamp) %>%
    # Группируем по типу биомассы
    group_by(biomass_f) %>%
    mutate(
      # Сначала рассчитываем УДЕЛЬНЫЙ поток за 30-минутный интервал (г C / г биомассы)
      specific_GPP_interval = (GPP_DT_U50 * 1800 * 12.01 / 1e6) / biomass_t65,
      specific_Reco_interval = (Reco_DT_U50 * 1800 * 12.01 / 1e6) / biomass_t65,
      # Затем считаем кумулятивную сумму от этих УДЕЛЬНЫХ значений
      cumulative_GPP_norm = cumsum(specific_GPP_interval),
      cumulative_Reco_norm = cumsum(specific_Reco_interval),
      # Добавляем день периода для оси X
      day_of_period = as.numeric(difftime(timestamp, first(timestamp), units = "days")) + 1
    ) %>%
    # Убираем группировку
    ungroup()
  
  # 2.2 График кумулятивного GPP, нормализованного на биомассу
  ggplot(cumulative_norm_data, aes(x = day_of_period, y = cumulative_GPP_norm, color = biomass_f)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(name = "Biomass Type", values = color_map_biomass) +
    labs(
      title = "Удельная общая первичная продуктивность",
      x = "День вегетации",
      y = "Удельное GPP (г С / г биомассы)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
  
  # 2.3 График кумулятивного Reco, нормализованного на биомассу
  ggplot(cumulative_norm_data, aes(x = day_of_period, y = cumulative_Reco_norm, color = biomass_f)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(name = "Biomass Type", values = color_map_biomass) +
    labs(
      title = "Cumulative Respiration per gram of Biomass",
      x = "Day of Period",
      y = "Cumulative Respiration (g C / g biomass)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
  