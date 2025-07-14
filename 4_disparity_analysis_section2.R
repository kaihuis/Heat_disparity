# ------------------------------------------------------------------------------
# Program Name: 3.1_merge_socioecon_multimodel.R
# Date Last Modified: June, 2023
# Program Purpose: disparity analysis and visualization of Fig 3 and Fig 5
# Author: Kaihui Song
# Contact: kaihuis@berkeley.edu
# Affiliation: University of California, Berkeley
# ------------------------------------------------------------------------------


# 0. Global setting -----

# libraries

rm(list=ls())
{
  library(tidyverse)
  library(openxlsx)
  library(ncdf4)
  library(raster)
  #library(rgdal)
  library(sp)
  library("epwshiftr")
  library(eplusr)
  library(geodata)
  library(curl)
  library(RCurl)
  library(lubridate)
  library(httr)
  library(RNetCDF)
  library(ncdf4)
  library(zoo)
  library(readr)
  library(usmap)
  library(terra)
  library(rasterVis)
  library(tmap)
  library(broom)
  library(sf)
  library(raster)
  library(weathermetrics)
  library(readxl)
  library(tabularaster)
  library(ggpubr)
  library(ggpmisc)       
}

# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 
`%!in%` <- compose(`!`, `%in%`)

# 0. Read data -----

## socioeconomic data ------

socioecon <- read_csv("~/Documents/ResearchUNC/Heat/data/socioecon.csv")


setwd("~/Documents/ResearchUNC/Heat/data/all_models/")

HI_all_models_ssp119 <- read_csv("HI_all_models_ssp119.csv") %>%
  mutate(SSP = case_when(SSP == "ssp119" ~ "SSP1-RCP1.9"))
HI_all_models_ssp126 <- read_csv("HI_all_models_ssp126.csv") %>%
  mutate(SSP = case_when(SSP == "ssp126" ~ "SSP1-RCP2.6"))
HI_all_models_ssp245 <- read_csv("HI_all_models_ssp245.csv") %>%
  mutate(SSP = case_when(SSP == "ssp245" ~ "SSP2-RCP4.5"))
HI_all_models_ssp370 <- read_csv("HI_all_models_ssp370.csv") %>%
  mutate(SSP = case_when(SSP == "ssp370" ~ "SSP3-RCP7.0"))
HI_all_models_ssp585 <- read_csv("HI_all_models_ssp585.csv") %>%
  mutate(SSP = case_when(SSP == "ssp585" ~ "SSP5-RCP8.5"))


county_boundary <- st_read("/Users/songkaihui/Documents/CMIP6/data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
  filter(STATEFP %!in% c("02", "15","60","66","69", "72","77"))
# 1. Race ----

## 1.1 Race - ssp 126 ----
HI_all_models_ssp126 <- read_csv("HI_all_models_ssp126.csv") %>%
  mutate(SSP = case_when(SSP == "ssp126" ~ "SSP1-RCP2.6"))

HI_all_models_ssp126 %>% 
  filter(Year %in% c(2015:2020, 2045:2050, 2095:2100)) %>%
  filter(Mon %in% c("06","07","08")) %>%
  mutate(Year = case_when(Year %in% c(2015:2020) ~ "2020",
                                Year %in% c(2045:2050) ~ "2050",
                                Year %in% c(2095:2100) ~ "2100")) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, model, SSP, GEOID, STATE_NAME, NAME, lat,lon) %>%
  summarise(tasF.summer = mean(tas.F),
            hurs.summer = mean(hurs),
            HI.summer = mean(HeatIndex), 
            .groups = "drop") -> HI.byperiod_ssp126

#write.csv(HI.byperiod_ssp126,"HI.byperiod_ssp126.csv", row.names = FALSE)

ssp126_bymodel <- HI.byperiod_ssp126 %>%
  left_join(socioecon, by = c("Year", "SSP", "GEOID")) 

### 1.1.1 extreme caution ----

ssp126_bymodel %>%
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, xcaution_plus_flag) %>%
  mutate(total_tot_xcaution = sum(TotPop, na.rm = T),    
         total_white_xcaution = sum(WhitePop, na.rm = T),
         total_black_xcaution = sum(BlackPop, na.rm = T),
         total_hispanic_xcaution = sum(HispanicPop, na.rm = T),
         total_otherrace_xcaution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_xcaution = total_tot_xcaution/total_pop,
         q_white_xcaution = total_white_xcaution/total_white,
         q_black_xcaution = total_black_xcaution/total_black,
         q_hispanic_xcaution = total_hispanic_xcaution/total_hispanic,
         q_otherrace_xcaution = total_otherrace_xcaution/total_otherrace) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_126

temp_xcaution_126 %>%
  dplyr::select(Year, model, SSP, q_tot_xcaution, q_white_xcaution, q_black_xcaution, q_hispanic_xcaution, q_otherrace_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_xcaution+q_black_xcaution+q_hispanic_xcaution+q_otherrace_xcaution)/4,
         miu_q2 = q_tot_xcaution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_xcaution_126


#### absolute ------
disparity_q_xcaution_126 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_xcaution" ~ "Non-Hispanic White",
                              variable == "q_black_xcaution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_xcaution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_xcaution" ~ "Non-Hispanic Other Races")) -> abs_race_bar_126_data
abs_race_bar_126_data %>%
  group_by(Year, model, SSP) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") +
  labs(subtitle = "SSP1-RCP2.6") +
  theme(plot.subtitle = element_text(face = "bold"))-> abs_race_bar_126
abs_race_bar_126


disparity_q_xcaution_126 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model, SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") -> abs_race_total_bar_126_data

abs_race_total_bar_126_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0))+
  labs(subtitle = "SSP1-RCP2.6") +
  theme(plot.subtitle = element_text(face = "bold")) ->abs_race_total_bar_126
abs_race_total_bar_126

#### relative -----

disparity_q_xcaution_126 %>%
  mutate(Black = q_black_xcaution/miu_q,
         White = q_white_xcaution/miu_q,
         Hispanic = q_hispanic_xcaution/miu_q,
         Others = q_otherrace_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races")) -> rel_race_bar_126_data
  
rel_race_bar_126_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP1-RCP2.6") +
  theme(plot.subtitle = element_text(face = "bold"))+
  ylab("Relative disparity") -> rel_race_bar_126
rel_race_bar_126 

disparity_q_xcaution_126 %>%
  pivot_longer(q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) -> rel_race_total_bar_126_data

rel_race_total_bar_126_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw()  +
  labs(subtitle = "SSP1-RCP2.6") +
  theme(plot.subtitle = element_text(face = "bold"))-> rel_race_total_bar_126

rel_race_total_bar_126

### 1.1.2 caution ----

ssp126_bymodel %>%
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, caution_plus_flag) %>%
  mutate(total_tot_caution = sum(TotPop, na.rm = T),    
         total_white_caution = sum(WhitePop, na.rm = T),
         total_black_caution = sum(BlackPop, na.rm = T),
         total_hispanic_caution = sum(HispanicPop, na.rm = T),
         total_otherrace_caution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_caution = total_tot_caution/total_pop,
         q_white_caution = total_white_caution/total_white,
         q_black_caution = total_black_caution/total_black,
         q_hispanic_caution = total_hispanic_caution/total_hispanic,
         q_otherrace_caution = total_otherrace_caution/total_otherrace) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_126

temp_caution_126 %>%
  dplyr::select(Year, model, SSP, q_tot_caution, q_white_caution, q_black_caution, q_hispanic_caution, q_otherrace_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_caution+q_black_caution+q_hispanic_caution+q_otherrace_caution)/4,
         miu_q2 = q_tot_caution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_caution_126


#### absolute ------
disparity_q_caution_126 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_caution" ~ "Non-Hispanic White",
                              variable == "q_black_caution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_caution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_caution" ~ "Non-Hispanic Other Races")) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") -> abs_race_bar_126_caution
abs_race_bar_126_caution


disparity_q_caution_126 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable) %>%
  group_by(Year, model) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 2, by = 0.5)), limits = c(0, 2.5), expand = c(0, 0)) -> abs_race_total_bar_126_caution
abs_race_total_bar_126_caution

#### relative -----

disparity_q_caution_126 %>%
  mutate(Black = q_black_caution/miu_q,
         White = q_white_caution/miu_q,
         Hispanic = q_hispanic_caution/miu_q,
         Others = q_otherrace_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() -> rel_race_bar_126_caution
rel_race_bar_126_caution

disparity_q_caution_126 %>%
  pivot_longer(q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() -> rel_race_total_bar_126_caution

rel_race_total_bar_126_caution

## 1.2 Race - ssp 245 ----

HI_all_models_ssp245 <- read_csv("HI_all_models_ssp245.csv") %>%
  mutate(SSP = case_when(SSP == "ssp245" ~ "SSP2-RCP4.5")) %>%
  mutate()
HI_all_models_ssp245 %>% 
  filter(Year %in% c(2015:2020, 2045:2050, 2095:2100)) %>%
  filter(Mon %in% c("06","07","08")) %>%
  mutate(Year = case_when(Year %in% c(2015:2020) ~ "2020",
                          Year %in% c(2045:2050) ~ "2050",
                          Year %in% c(2095:2100) ~ "2100")) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, model, SSP, GEOID, STATE_NAME, NAME, lat,lon) %>%
  summarise(tasF.summer = mean(tas.F),
            hurs.summer = mean(hurs),
            HI.summer = mean(HeatIndex)) -> HI.byperiod_ssp245

#write.csv(HI.byperiod_ssp245,"HI.byperiod_ssp245.csv", row.names = FALSE)

ssp245_bymodel <- HI.byperiod_ssp245 %>%
  left_join(socioecon, by = c("Year", "SSP", "GEOID")) 

### 1.2.1 extreme caution ----

ssp245_bymodel %>%
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, xcaution_plus_flag) %>%
  mutate(total_tot_xcaution = sum(TotPop, na.rm = T),    
         total_white_xcaution = sum(WhitePop, na.rm = T),
         total_black_xcaution = sum(BlackPop, na.rm = T),
         total_hispanic_xcaution = sum(HispanicPop, na.rm = T),
         total_otherrace_xcaution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_xcaution = total_tot_xcaution/total_pop,
         q_white_xcaution = total_white_xcaution/total_white,
         q_black_xcaution = total_black_xcaution/total_black,
         q_hispanic_xcaution = total_hispanic_xcaution/total_hispanic,
         q_otherrace_xcaution = total_otherrace_xcaution/total_otherrace) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_245

temp_xcaution_245 %>%
  dplyr::select(Year, model, SSP, q_tot_xcaution, q_white_xcaution, q_black_xcaution, q_hispanic_xcaution, q_otherrace_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_xcaution+q_black_xcaution+q_hispanic_xcaution+q_otherrace_xcaution)/4,
         miu_q2 = q_tot_xcaution)  %>%
  filter(SSP != "SSP1-RCP1.9")  -> disparity_q_xcaution_245

#### absolute ------
disparity_q_xcaution_245 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_xcaution" ~ "Non-Hispanic White",
                              variable == "q_black_xcaution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_xcaution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_xcaution" ~ "Non-Hispanic Other Races")) -> abs_race_bar_245_data
abs_race_bar_245_data %>%
  group_by(Year, model, SSP) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold"))-> abs_race_bar_245
abs_race_bar_245


disparity_q_xcaution_245 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model, SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") -> abs_race_total_bar_245_data

abs_race_total_bar_245_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity")+
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0)) +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold")) ->abs_race_total_bar_245
abs_race_total_bar_245

#### relative -----

disparity_q_xcaution_245 %>%
  mutate(Black = q_black_xcaution/miu_q,
         White = q_white_xcaution/miu_q,
         Hispanic = q_hispanic_xcaution/miu_q,
         Others = q_otherrace_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races")) -> rel_race_bar_245_data
  
rel_race_bar_245_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_245
rel_race_bar_245

disparity_q_xcaution_245 %>%
  pivot_longer(q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_245_data

rel_race_total_bar_245_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold"))-> rel_race_total_bar_245

rel_race_total_bar_245

#write.csv(HI.byperiod_ssp245,"HI.byperiod_ssp245.csv", row.names = FALSE)

### 1.2.2 caution -----

ssp245_bymodel %>%
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, caution_plus_flag) %>%
  mutate(total_tot_caution = sum(TotPop, na.rm = T),    
         total_white_caution = sum(WhitePop, na.rm = T),
         total_black_caution = sum(BlackPop, na.rm = T),
         total_hispanic_caution = sum(HispanicPop, na.rm = T),
         total_otherrace_caution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_caution = total_tot_caution/total_pop,
         q_white_caution = total_white_caution/total_white,
         q_black_caution = total_black_caution/total_black,
         q_hispanic_caution = total_hispanic_caution/total_hispanic,
         q_otherrace_caution = total_otherrace_caution/total_otherrace) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_245

temp_caution_245 %>%
  dplyr::select(Year, model, SSP, q_tot_caution, q_white_caution, q_black_caution, q_hispanic_caution, q_otherrace_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_caution+q_black_caution+q_hispanic_caution+q_otherrace_caution)/4,
         miu_q2 = q_tot_caution)  %>%
  filter(SSP != "SSP1-RCP1.9")  -> disparity_q_caution_245

#### absolute ------
disparity_q_caution_245 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_caution" ~ "Non-Hispanic White",
                              variable == "q_black_caution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_caution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_caution" ~ "Non-Hispanic Other Races")) -> abs_race_bar_245_data

abs_race_bar_245_data %>%
  group_by(Year, model, SSP) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold"))-> abs_race_bar_245
abs_race_bar_245


disparity_q_caution_245 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model, SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") -> abs_race_total_bar_245_data

abs_race_total_bar_245_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity")+
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0)) +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold")) ->abs_race_total_bar_245
abs_race_total_bar_245

#### relative -----

disparity_q_caution_245 %>%
  mutate(Black = q_black_caution/miu_q,
         White = q_white_caution/miu_q,
         Hispanic = q_hispanic_caution/miu_q,
         Others = q_otherrace_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races")) -> rel_race_bar_245_data

rel_race_bar_245_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_245
rel_race_bar_245

disparity_q_caution_245 %>%
  pivot_longer(q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_245_data

rel_race_total_bar_245_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP2-RCP4.5") +
  theme(plot.subtitle = element_text(face = "bold"))-> rel_race_total_bar_245

rel_race_total_bar_245


## 1.3 Race ssp 370 ----
HI_all_models_ssp370 <- read_csv("HI_all_models_ssp370.csv") %>%
  mutate(SSP = case_when(SSP == "ssp370" ~ "SSP3-RCP7.0"))

HI_all_models_ssp370 %>% 
  filter(Year %in% c(2015:2020, 2045:2050, 2095:2100)) %>%
  filter(Mon %in% c("06","07","08")) %>%
  mutate(Year = case_when(Year %in% c(2015:2020) ~ "2020",
                                Year %in% c(2045:2050) ~ "2050",
                                Year %in% c(2095:2100) ~ "2100")) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, model, SSP, GEOID, STATE_NAME, NAME, lat,lon) %>%
  summarise(tasF.summer = mean(tas.F),
            hurs.summer = mean(hurs),
            HI.summer = mean(HeatIndex),
            .groups = "drop") -> HI.byperiod_ssp370

#write.csv(HI.byperiod_ssp370,"HI.byperiod_ssp370.csv", row.names = FALSE)

ssp370_bymodel <- HI.byperiod_ssp370 %>%
  left_join(socioecon, by = c("Year", "SSP", "GEOID")) 

### 1.3.1 extreme caution ----

ssp370_bymodel %>%
  dplyr::select(Year, model,SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, caution_plus_flag) %>%
  mutate(total_tot_xcaution = sum(TotPop, na.rm = T),    
         total_white_xcaution = sum(WhitePop, na.rm = T),
         total_black_xcaution = sum(BlackPop, na.rm = T),
         total_hispanic_xcaution = sum(HispanicPop, na.rm = T),
         total_otherrace_xcaution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_xcaution = total_tot_xcaution/total_pop,
         q_white_xcaution = total_white_xcaution/total_white,
         q_black_xcaution = total_black_xcaution/total_black,
         q_hispanic_xcaution = total_hispanic_xcaution/total_hispanic,
         q_otherrace_xcaution = total_otherrace_xcaution/total_otherrace) %>%
  filter(caution_plus_flag == "Yes") -> temp_xcaution_370

temp_xcaution_370 %>%
  dplyr::select(Year, model, SSP, q_tot_xcaution, q_white_xcaution, q_black_xcaution, q_hispanic_xcaution, q_otherrace_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_xcaution+q_black_xcaution+q_hispanic_xcaution+q_otherrace_xcaution)/4,
         miu_q2 = q_tot_xcaution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_xcaution_370

#### absolute ------
disparity_q_xcaution_370 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_xcaution" ~ "Non-Hispanic White",
                              variable == "q_black_xcaution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_xcaution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_xcaution" ~ "Non-Hispanic Other Races")) -> abs_race_bar_370_data

abs_race_bar_370_data %>%
  group_by(Year, model, SSP) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold")) -> abs_race_bar_370
abs_race_bar_370

disparity_q_xcaution_370 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model,SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") ->abs_race_total_bar_370_data

abs_race_total_bar_370_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0))+
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold"))-> abs_race_total_bar_370
abs_race_total_bar_370

#### relative -----

disparity_q_xcaution_370 %>%
  mutate(Black = q_black_xcaution/miu_q,
         White = q_white_xcaution/miu_q,
         Hispanic = q_hispanic_xcaution/miu_q,
         Others = q_otherrace_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio")  %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races")) -> rel_race_bar_370_data

rel_race_bar_370_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_370
rel_race_bar_370

disparity_q_xcaution_370 %>%
  pivot_longer(q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_370_data
rel_race_total_bar_370_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold"))-> rel_race_total_bar_370

rel_race_total_bar_370

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/CoV_caution.png", width = 6, height = 4)

### 2.3.2 caution ------

ssp370_bymodel %>%
  dplyr::select(Year, model,SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, caution_plus_flag) %>%
  mutate(total_tot_caution = sum(TotPop, na.rm = T),    
         total_white_caution = sum(WhitePop, na.rm = T),
         total_black_caution = sum(BlackPop, na.rm = T),
         total_hispanic_caution = sum(HispanicPop, na.rm = T),
         total_otherrace_caution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_caution = total_tot_caution/total_pop,
         q_white_caution = total_white_caution/total_white,
         q_black_caution = total_black_caution/total_black,
         q_hispanic_caution = total_hispanic_caution/total_hispanic,
         q_otherrace_caution = total_otherrace_caution/total_otherrace) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_370

temp_caution_370 %>%
  dplyr::select(Year, model, SSP, q_tot_caution, q_white_caution, q_black_caution, q_hispanic_caution, q_otherrace_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_caution+q_black_caution+q_hispanic_caution+q_otherrace_caution)/4,
         miu_q2 = q_tot_caution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_caution_370

#### absolute ------
disparity_q_caution_370 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_caution" ~ "Non-Hispanic White",
                              variable == "q_black_caution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_caution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_caution" ~ "Non-Hispanic Other Races")) -> abs_race_bar_370_data

abs_race_bar_370_data %>%
  group_by(Year, model, SSP) %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  ylab("Absolute disparity") +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold")) -> abs_race_bar_370
abs_race_bar_370

disparity_q_caution_370 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model,SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") ->abs_race_total_bar_370_data

abs_race_total_bar_370_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw()+
  ylab("Total absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0))+
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold"))-> abs_race_total_bar_370
abs_race_total_bar_370

#### relative -----

disparity_q_caution_370 %>%
  mutate(Black = q_black_caution/miu_q,
         White = q_white_caution/miu_q,
         Hispanic = q_hispanic_caution/miu_q,
         Others = q_otherrace_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio")  %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races")) -> rel_race_bar_370_data

rel_race_bar_370_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_370
rel_race_bar_370

disparity_q_caution_370 %>%
  pivot_longer(q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_370_data
rel_race_total_bar_370_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP3-RCP7.0") +
  theme(plot.subtitle = element_text(face = "bold"))-> rel_race_total_bar_370

rel_race_total_bar_370

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/CoV_caution.png", width = 6, height = 4)


## 1.4 Race ssp585 ----
HI_all_models_ssp585 <- read_csv("HI_all_models_ssp585.csv") %>%
  mutate(SSP = case_when(SSP == "ssp585" ~ "SSP5-RCP8.5"))

HI_all_models_ssp585 %>% 
  filter(Year %in% c(2015:2020, 2045:2050, 2095:2100)) %>%
  filter(Mon %in% c("06","07","08")) %>%
  mutate(Year = case_when(Year %in% c(2015:2020) ~ "2020",
                                Year %in% c(2045:2050) ~ "2050",
                                Year %in% c(2095:2100) ~ "2100")) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, model, SSP, GEOID, STATE_NAME, NAME, lat,lon) %>%
  summarise(tasF.summer = mean(tas.F),
            hurs.summer = mean(hurs),
            HI.summer = mean(HeatIndex)) -> HI.byperiod_ssp585

#write.csv(HI.byperiod_ssp585,"HI.byperiod_ssp585.csv", row.names = FALSE)

ssp585_bymodel <- HI.byperiod_ssp585 %>%
  left_join(socioecon, by = c("Year", "SSP", "GEOID")) 

### 1.4.1 extreme caution ----

ssp585_bymodel %>%
  dplyr::select(Year, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(model, Year, SSP, xcaution_flag) %>%
  mutate(total_tot_xcaution = sum(TotPop, na.rm = T),    
         total_white_xcaution = sum(WhitePop, na.rm = T),
         total_black_xcaution = sum(BlackPop, na.rm = T),
         total_hispanic_xcaution = sum(HispanicPop, na.rm = T),
         total_otherrace_xcaution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_xcaution = total_tot_xcaution/total_pop,
         q_white_xcaution = total_white_xcaution/total_white,
         q_black_xcaution = total_black_xcaution/total_black,
         q_hispanic_xcaution = total_hispanic_xcaution/total_hispanic,
         q_otherrace_xcaution = total_otherrace_xcaution/total_otherrace) %>%
  filter(xcaution_flag == "Yes") -> temp_xcaution_585

temp_xcaution_585 %>%
  dplyr::select(Year, model, SSP, q_tot_xcaution, q_white_xcaution, q_black_xcaution, q_hispanic_xcaution, q_otherrace_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_xcaution+q_black_xcaution+q_hispanic_xcaution+q_otherrace_xcaution)/4,
         miu_q2 = q_tot_xcaution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_xcaution_585

#### absolute ------

disparity_q_xcaution_585 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_xcaution" ~ "Non-Hispanic White",
                              variable == "q_black_xcaution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_xcaution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_xcaution" ~ "Non-Hispanic Other Races")) ->abs_race_bar_585_data

abs_race_bar_585_data %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) -> abs_race_bar_585
abs_race_bar_585

disparity_q_xcaution_585 %>% 
  pivot_longer(cols = q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model, SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") ->abs_race_total_bar_585_data

abs_race_total_bar_585_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  ylab("Total absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0)) +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) ->abs_race_total_bar_585
abs_race_total_bar_585

#### relative -----

disparity_q_xcaution_585 %>%
  mutate(Black = q_black_xcaution/miu_q,
         White = q_white_xcaution/miu_q,
         Hispanic = q_hispanic_xcaution/miu_q,
         Others = q_otherrace_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races"))-> rel_race_bar_585_data

rel_race_bar_585_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_585
rel_race_bar_585

disparity_q_xcaution_585 %>%
  pivot_longer(q_white_xcaution:q_otherrace_xcaution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_585_data
rel_race_total_bar_585_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) -> rel_race_total_bar_585

rel_race_total_bar_585

### 1.4.2 caution ------

ssp585_bymodel %>%
  dplyr::select(Year, SSP, GEOID, HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(model, Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                   TRUE ~"No")) %>%
  group_by(model, Year, SSP, caution_flag) %>%
  mutate(total_tot_caution = sum(TotPop, na.rm = T),    
         total_white_caution = sum(WhitePop, na.rm = T),
         total_black_caution = sum(BlackPop, na.rm = T),
         total_hispanic_caution = sum(HispanicPop, na.rm = T),
         total_otherrace_caution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_caution = total_tot_caution/total_pop,
         q_white_caution = total_white_caution/total_white,
         q_black_caution = total_black_caution/total_black,
         q_hispanic_caution = total_hispanic_caution/total_hispanic,
         q_otherrace_caution = total_otherrace_caution/total_otherrace) %>%
  filter(caution_flag == "Yes") -> temp_caution_585

temp_caution_585 %>%
  dplyr::select(Year, model, SSP, q_tot_caution, q_white_caution, q_black_caution, q_hispanic_caution, q_otherrace_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_caution+q_black_caution+q_hispanic_caution+q_otherrace_caution)/4,
         miu_q2 = q_tot_caution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_caution_585

#### absolute ------

disparity_q_caution_585 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  mutate(variable = case_when(variable == "q_white_caution" ~ "Non-Hispanic White",
                              variable == "q_black_caution" ~ "Non-Hispanic Black",
                              variable == "q_hispanic_caution" ~ "Hispanic (all races)",
                              variable == "q_otherrace_caution" ~ "Non-Hispanic Other Races")) ->abs_race_bar_585_data

abs_race_bar_585_data %>%
  ggplot(aes(x = Year, y = q_minus_miu*100, color = variable))+
  geom_boxplot() +
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) -> abs_race_bar_585
abs_race_bar_585

disparity_q_caution_585 %>% 
  pivot_longer(cols = q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, q_minus_miu, variable, SSP) %>%
  group_by(Year, model, SSP) %>%
  summarise(total_absolute = sum(abs(q_minus_miu))/4, .groups = "drop") ->abs_race_total_bar_585_data

abs_race_total_bar_585_data %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  ylab("Overall absolute disparity") +
  scale_y_continuous(breaks = c(seq(0, 10, by = 2)), limits = c(0, 10), expand = c(0, 0)) +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) ->abs_race_total_bar_585
abs_race_total_bar_585

#### relative -----

disparity_q_caution_585 %>%
  mutate(Black = q_black_caution/miu_q,
         White = q_white_caution/miu_q,
         Hispanic = q_hispanic_caution/miu_q,
         Others = q_otherrace_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Black, White, Hispanic, Others, miu_q) %>%
  pivot_longer(cols = Black:Others, names_to = "race", values_to = "q_ratio") %>%
  mutate(race = case_when(race == "White" ~ "Non-Hispanic White",
                          race == "Black" ~ "Non-Hispanic Black",
                          race == "Hispanic" ~ "Hispanic (all races)",
                          race == "Others" ~ "Non-Hispanic Other Races"))-> rel_race_bar_585_data

rel_race_bar_585_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = race))+
  geom_boxplot()+
  scale_color_discrete(name = "Race/Ethnicity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) +
  ylab("Relative disparity") -> rel_race_bar_585
rel_race_bar_585

disparity_q_caution_585 %>%
  pivot_longer(q_white_caution:q_otherrace_caution, names_to = "variable", values_to = "q") %>%
  mutate(Year = as.character(Year)) %>%
  group_by(model, Year, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) ->rel_race_total_bar_585_data
rel_race_total_bar_585_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  guides(color= guide_legend(title="SSP-RCP scenarios"))+
  ylab("Relative disparity") +
  theme_bw() +
  labs(subtitle = "SSP5-RCP8.5") +
  theme(plot.subtitle = element_text(face = "bold")) -> rel_race_total_bar_585

rel_race_total_bar_585
  
ggarrange(abs_race_bar_126, abs_race_bar_245, abs_race_bar_370, abs_race_bar_585,
          #labels = c("A", "B", "C", "D"),
          common.legend = T,
          ncol = 4,
          legend = "bottom") ->abs_race_bar
abs_race_bar
ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/abs_race_bar.png", width = 8, height = 3)

#### vis combine ------

##### absolute combine  ----
abs_race_bar_spp_combine <- abs_race_bar_126_data %>%
  rbind(abs_race_bar_245_data)%>%
  rbind(abs_race_bar_370_data)%>%
  rbind(abs_race_bar_585_data)

abs_race_total_bar_ssp_data_combine <- abs_race_total_bar_126_data %>%
  rbind(abs_race_total_bar_245_data) %>%
  rbind(abs_race_total_bar_370_data) %>%
  rbind(abs_race_total_bar_585_data) 

abs_race_bar_spp_combine %>%
  mutate(variable = case_when(variable == "Hispanic (all races)" ~ "Hispanic",
                              variable == "Non-Hispanic Black" ~ "NH Black",
                              variable == "Non-Hispanic Other Races" ~ "NH Others",
                              variable == "Non-Hispanic White" ~ "NH White")) %>%
  ggplot(aes(x = variable, y = q_minus_miu*100, color = Year))+
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  facet_wrap(~SSP, ncol = 4)+
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank())-> abs_race_bar_spp_combine_plot
abs_race_bar_spp_combine_plot

abs_race_total_bar_ssp_data_combine %>%
  mutate(label = "Overall absolute disparity") %>%
  ggplot(aes(x = SSP, y = total_absolute*100, color = Year))+
  geom_boxplot() +
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Year") +
  facet_wrap(~label, ncol = 1)+
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(seq(0, 10, by = 5)), limits = c(0, 10), expand = c(0, 0)) -> abs_race_total_bar_ssp_data_combine_plot
abs_race_total_bar_ssp_data_combine_plot

fig3_ab <- ggarrange(abs_race_bar_spp_combine_plot, abs_race_total_bar_ssp_data_combine_plot,
          labels = c("A", "B"),
          widths = c(3, 1.2),  
          common.legend = T,
          legend = "bottom")
fig3_ab

##### relative combine ----
rel_race_bar_spp_combine <- rel_race_bar_126_data %>%
  rbind(rel_race_bar_245_data)%>%
  rbind(rel_race_bar_370_data)%>%
  rbind(rel_race_bar_585_data)

rel_race_total_bar_ssp_data_combine <- rel_race_total_bar_126_data %>%
  rbind(rel_race_total_bar_245_data) %>%
  rbind(rel_race_total_bar_370_data) %>%
  rbind(rel_race_total_bar_585_data) 

rel_race_bar_spp_combine %>%
  mutate(race = case_when(race == "Hispanic (all races)" ~ "Hispanic",
                          race == "Non-Hispanic Black" ~ "NH Black",
                          race == "Non-Hispanic Other Races" ~ "NH Others",
                          race == "Non-Hispanic White" ~ "NH White")) %>%
  ggplot(aes(x = race, y = q_ratio, color = as.character(Year)))+
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")+
  ylab("Relative disparity") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  facet_wrap(~SSP, ncol = 4)+
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(breaks = c(seq(0, 2, by = 0.5)), limits = c(0, 2.3), expand = c(0, 0))  -> rel_race_bar_spp_combine_plot
rel_race_bar_spp_combine_plot

rel_race_total_bar_ssp_data_combine %>%
  mutate(label = "Overall relative disparity") %>%
  ggplot(aes(x = SSP, y = CoV, color = Year))+
  geom_boxplot() +
  ylab("Relative disparity") +
  scale_color_discrete(name = "Year") +
  facet_wrap(~label, ncol = 1)+
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(seq(0, 2, by = 0.5)), limits = c(0, 2.3), expand = c(0, 0)) -> rel_race_total_bar_ssp_data_combine_plot
rel_race_total_bar_ssp_data_combine_plot

fig3_cd <- ggarrange(rel_race_bar_spp_combine_plot, rel_race_total_bar_ssp_data_combine_plot,
          labels = c("C", "D"),
          widths = c(3, 1.2),  
          common.legend = T,
          legend = "bottom")

##### figure3abcd ------
fig3 <- ggarrange(fig3_ab, fig3_cd,
          ncol = 1)
fig3

ggsave("~/Documents/ResearchUNC/Heat/Figures/fig3.pdf", width = 12.5, height = 8.75)
#ggsave("~/Documents/ResearchUNC/Heat/Figures/fig3 .png", width = 10, height = 7)

# 2. Age ----
## 2.1 Age ssp126 -----

### 2.1.1 extreme caution-----

ssp126_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                               TRUE ~"No")) %>%
  group_by(Year,model, SSP, xcaution_plus_flag) %>%
  mutate(total_young_xcaution = sum(YoungPop, na.rm = T),
         total_adult_xcaution = sum(AdultPop, na.rm = T),
         total_elderly_xcaution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_xcaution = total_young_xcaution/total_young,
         q_adult_xcaution = total_adult_xcaution/total_adult,
         q_elderly_xcaution = total_elderly_xcaution/total_elderly) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_age_126

temp_xcaution_age_126 %>%
  dplyr::select(Year, model,SSP, q_young_xcaution, q_adult_xcaution, q_elderly_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_xcaution+q_adult_xcaution+q_elderly_xcaution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> xcaution_age_126

##### absolute -----

xcaution_age_126 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_xcaution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_xcaution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_xcaution" ~ "Young (<20 y/o)")) -> abs_age_bar_126_data
abs_age_bar_126_data %>% 
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_126
abs_age_bar_126


xcaution_age_126 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") ->abs_age_total_bar_126_data

abs_age_total_bar_126_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_126
abs_age_total_bar_126

##### relative  -----

xcaution_age_126 %>%
  mutate(Young = q_young_xcaution/miu_q,
         Adult = q_adult_xcaution/miu_q,
         Elderly = q_elderly_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_xcaution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_xcaution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_xcaution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) ->rel_age_bar_126_data

rel_age_bar_126_data  %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_126
rel_age_bar_126

xcaution_age_126 %>%
  pivot_longer(q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) ->rel_age_total_bar_126_data
rel_age_total_bar_126_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_126
rel_age_total_bar_126


### 2.1.2 caution ------

ssp126_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, caution_plus_flag) %>%
  mutate(total_young_caution = sum(YoungPop, na.rm = T),
         total_adult_caution = sum(AdultPop, na.rm = T),
         total_elderly_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_caution = total_young_caution/total_young,
         q_adult_caution = total_adult_caution/total_adult,
         q_elderly_caution = total_elderly_caution/total_elderly) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_age_126

temp_caution_age_126 %>%
  dplyr::select(Year, model,SSP, q_young_caution, q_adult_caution, q_elderly_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_caution+q_adult_caution+q_elderly_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> caution_age_126

##### absolute -----

caution_age_126 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_caution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_caution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_caution" ~ "Young (<20 y/o)")) -> abs_age_bar_126_data
abs_age_bar_126_data %>% 
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_126
abs_age_bar_126


caution_age_126 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") ->abs_age_total_bar_126_data

abs_age_total_bar_126_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_126
abs_age_total_bar_126

##### relative  -----

caution_age_126 %>%
  mutate(Young = q_young_caution/miu_q,
         Adult = q_adult_caution/miu_q,
         Elderly = q_elderly_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_xcaution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_xcaution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_xcaution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) ->rel_age_bar_126_data

rel_age_bar_126_data  %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_126
rel_age_bar_126

caution_age_126 %>%
  pivot_longer(q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) ->rel_age_total_bar_126_data
rel_age_total_bar_126_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_126
rel_age_total_bar_126


## 2.2 Age - ssp 245 ----

### 2.2.1 extreme caution ------
ssp245_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, xcaution_plus_flag) %>%
  mutate(total_young_xcaution = sum(YoungPop, na.rm = T),
         total_adult_xcaution = sum(AdultPop, na.rm = T),
         total_elderly_xcaution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_xcaution = total_young_xcaution/total_young,
         q_adult_xcaution = total_adult_xcaution/total_adult,
         q_elderly_xcaution = total_elderly_xcaution/total_elderly) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_age_245

temp_xcaution_age_245 %>%
  dplyr::select(Year, model,SSP, q_young_xcaution, q_adult_xcaution, q_elderly_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_xcaution+q_adult_xcaution+q_elderly_xcaution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> xcaution_age_245

##### absolute -----

xcaution_age_245 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_xcaution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_xcaution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_xcaution" ~ "Young (<20 y/o)")) -> abs_age_bar_245_data
abs_age_bar_245_data %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_245
abs_age_bar_245


xcaution_age_245 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") ->abs_age_total_bar_245_data

abs_age_total_bar_245_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_245
abs_age_total_bar_245

##### relative  -----

xcaution_age_245 %>%
  mutate(Young = q_young_xcaution/miu_q,
         Adult = q_adult_xcaution/miu_q,
         Elderly = q_elderly_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_xcaution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_xcaution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_xcaution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) ->rel_age_bar_245_data
rel_age_bar_245_data  %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_245
rel_age_bar_245

xcaution_age_245 %>%
  pivot_longer(q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_245_data
rel_age_total_bar_245_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_245
rel_age_total_bar_245

### 2.2.2 caution -----

ssp245_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, caution_plus_flag) %>%
  mutate(total_young_caution = sum(YoungPop, na.rm = T),
         total_adult_caution = sum(AdultPop, na.rm = T),
         total_elderly_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_caution = total_young_caution/total_young,
         q_adult_caution = total_adult_caution/total_adult,
         q_elderly_caution = total_elderly_caution/total_elderly) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_age_245

temp_caution_age_245 %>%
  dplyr::select(Year, model,SSP, q_young_caution, q_adult_caution, q_elderly_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_caution+q_adult_caution+q_elderly_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> caution_age_245

##### absolute -----

caution_age_245 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_caution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_caution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_caution" ~ "Young (<20 y/o)")) -> abs_age_bar_245_data
abs_age_bar_245_data %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_245
abs_age_bar_245


caution_age_245 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") ->abs_age_total_bar_245_data

abs_age_total_bar_245_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_245
abs_age_total_bar_245

##### relative  -----

caution_age_245 %>%
  mutate(Young = q_young_caution/miu_q,
         Adult = q_adult_caution/miu_q,
         Elderly = q_elderly_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_caution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_caution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_caution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) ->rel_age_bar_245_data
rel_age_bar_245_data  %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_245
rel_age_bar_245

caution_age_245 %>%
  pivot_longer(q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_245_data
rel_age_total_bar_245_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_245
rel_age_total_bar_245


## 2.3 Age 370 ----
### 2.3.1 extreme caution -----
ssp370_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, xcaution_plus_flag) %>%
  mutate(total_young_xcaution = sum(YoungPop, na.rm = T),
         total_adult_xcaution = sum(AdultPop, na.rm = T),
         total_elderly_xcaution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_xcaution = total_young_xcaution/total_young,
         q_adult_xcaution = total_adult_xcaution/total_adult,
         q_elderly_xcaution = total_elderly_xcaution/total_elderly) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_age_370

temp_xcaution_age_370 %>%
  dplyr::select(Year, model,SSP, q_young_xcaution, q_adult_xcaution, q_elderly_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_xcaution+q_adult_xcaution+q_elderly_xcaution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> xcaution_age_370

##### absolute -----

xcaution_age_370 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_xcaution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_xcaution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_xcaution" ~ "Young (<20 y/o)")) -> abs_age_bar_370_data
abs_age_bar_370_data %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_370
abs_age_bar_370


xcaution_age_370 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") -> abs_age_total_bar_370_data

abs_age_total_bar_370_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_370
abs_age_total_bar_370

##### relative  -----

xcaution_age_370 %>%
  mutate(Young = q_young_xcaution/miu_q,
         Adult = q_adult_xcaution/miu_q,
         Elderly = q_elderly_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_xcaution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_xcaution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_xcaution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) -> rel_age_bar_370_data
rel_age_bar_370_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color  = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_370
rel_age_bar_370

xcaution_age_370 %>%
  pivot_longer(q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_370_data
rel_age_total_bar_370_data  %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_370
rel_age_total_bar_370


### 2.3.2 caution -----
ssp370_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, caution_plus_flag) %>%
  mutate(total_young_caution = sum(YoungPop, na.rm = T),
         total_adult_caution = sum(AdultPop, na.rm = T),
         total_elderly_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_caution = total_young_caution/total_young,
         q_adult_caution = total_adult_caution/total_adult,
         q_elderly_caution = total_elderly_caution/total_elderly) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_age_370

temp_caution_age_370 %>%
  dplyr::select(Year, model,SSP, q_young_caution, q_adult_caution, q_elderly_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_caution+q_adult_caution+q_elderly_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> caution_age_370

##### absolute -----

caution_age_370 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_caution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_caution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_caution" ~ "Young (<20 y/o)")) -> abs_age_bar_370_data
abs_age_bar_370_data %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_370
abs_age_bar_370


caution_age_370 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") -> abs_age_total_bar_370_data

abs_age_total_bar_370_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_370
abs_age_total_bar_370

##### relative  -----

caution_age_370 %>%
  mutate(Young = q_young_caution/miu_q,
         Adult = q_adult_caution/miu_q,
         Elderly = q_elderly_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_caution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_caution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_caution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) -> rel_age_bar_370_data
rel_age_bar_370_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color  = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_370
rel_age_bar_370

caution_age_370 %>%
  pivot_longer(q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_370_data
rel_age_total_bar_370_data  %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_370
rel_age_total_bar_370



## 2.4 Age 585 ----

### 2.4.1 extreme caution -----
ssp585_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(xcaution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, xcaution_plus_flag) %>%
  mutate(total_young_xcaution = sum(YoungPop, na.rm = T),
         total_adult_xcaution = sum(AdultPop, na.rm = T),
         total_elderly_xcaution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_xcaution = total_young_xcaution/total_young,
         q_adult_xcaution = total_adult_xcaution/total_adult,
         q_elderly_xcaution = total_elderly_xcaution/total_elderly) %>%
  filter(xcaution_plus_flag == "Yes") -> temp_xcaution_age_585

temp_xcaution_age_585 %>%
  dplyr::select(Year, model,SSP, q_young_xcaution, q_adult_xcaution, q_elderly_xcaution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_xcaution+q_adult_xcaution+q_elderly_xcaution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> xcaution_age_585

##### absolute -----

xcaution_age_585 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_xcaution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_xcaution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_xcaution" ~ "Young (<20 y/o)"))  -> abs_age_bar_585_data
abs_age_bar_585_data  %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_585
abs_age_bar_585


xcaution_age_585 %>% 
  pivot_longer(cols = q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") -> abs_age_total_bar_585_data

abs_age_total_bar_585_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_585
abs_age_total_bar_585

##### relative  -----

xcaution_age_585 %>%
  mutate(Young = q_young_xcaution/miu_q,
         Adult = q_adult_xcaution/miu_q,
         Elderly = q_elderly_xcaution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_xcaution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_xcaution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_xcaution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) -> rel_age_bar_585_data
rel_age_bar_585_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_585
rel_age_bar_585

xcaution_age_585 %>%
  pivot_longer(q_young_xcaution:q_elderly_xcaution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_585_data
rel_age_total_bar_585_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_585
rel_age_total_bar_585

### 2.4.2 caution -----

ssp585_bymodel %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, model, SSP, GEOID, HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop) %>%
  mutate(HI_level = case_when(HI.summer<80 ~ "Safe",
                              HI.summer>=80 & HI.summer<90 ~ "Caution",
                              HI.summer>=90 & HI.summer<103 ~ "Extreme Caution",
                              HI.summer>= 103 ~ "Danger")) %>%
  group_by(Year, model, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                        TRUE ~"No")) %>%
  group_by(Year,model, SSP, caution_plus_flag) %>%
  mutate(total_young_caution = sum(YoungPop, na.rm = T),
         total_adult_caution = sum(AdultPop, na.rm = T),
         total_elderly_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_caution = total_young_caution/total_young,
         q_adult_caution = total_adult_caution/total_adult,
         q_elderly_caution = total_elderly_caution/total_elderly) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_age_585

temp_caution_age_585 %>%
  dplyr::select(Year, model,SSP, q_young_caution, q_adult_caution, q_elderly_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_caution+q_adult_caution+q_elderly_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> caution_age_585

##### absolute -----

caution_age_585 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(variable = case_when(variable == "q_adult_caution" ~ "Adult (20-64 y/o)",
                              variable == "q_elderly_caution" ~ "Elderly (65+ y/o)",
                              variable == "q_young_caution" ~ "Young (<20 y/o)"))  -> abs_age_bar_585_data
abs_age_bar_585_data  %>%
  ggplot(aes(x = Year, y = q_minus_miu * 100, color = variable))+
  geom_boxplot()+
  facet_wrap(~SSP) +
  theme_bw() +
  scale_color_discrete(name = "Age") +
  ylab("Absolute disparity")  -> abs_age_bar_585
abs_age_bar_585


caution_age_585 %>% 
  pivot_longer(cols = q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q")  %>%
  mutate(q_minus_miu = q - miu_q) %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Year, model, SSP, q_minus_miu, variable) %>%
  group_by(Year, model, SSP) %>%
  mutate(N = n_distinct(.$model)) %>%
  summarise(total_absolute = sum(abs(q_minus_miu)/N), .groups = "drop") -> abs_age_total_bar_585_data

abs_age_total_bar_585_data  %>%
  ggplot(aes(x = Year, y = total_absolute*100))+
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~SSP)+
  ylab("Total absolute disparity")+
  theme(legend.position = "none")-> abs_age_total_bar_585
abs_age_total_bar_585

##### relative  -----

caution_age_585 %>%
  mutate(Young = q_young_caution/miu_q,
         Adult = q_adult_caution/miu_q,
         Elderly = q_elderly_caution/miu_q) %>%
  dplyr::select(Year, model, SSP, Young, Adult, Elderly, miu_q) %>%
  pivot_longer(cols = Young:Elderly, names_to = "Age", values_to = "q_ratio") %>%
  mutate(Age = replace(Age, Age == "q_young_caution", "Young (<20 y/o)"),
         Age = replace(Age, Age == "q_adult_caution", "Adult (20-64 y/o)"),
         Age = replace(Age, Age == "q_elderly_caution", "Elderly (65+ y/o)")) %>%
  mutate(Year = as.character(Year)) -> rel_age_bar_585_data
rel_age_bar_585_data %>%
  ggplot(aes(x = Year, y = q_ratio-1, color = Age))+
  geom_boxplot() +
  facet_wrap(~SSP) +
  theme_bw() +
  ylab("Relative disparity") -> rel_age_bar_585
rel_age_bar_585

caution_age_585 %>%
  pivot_longer(q_young_caution:q_elderly_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, model, SSP) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  mutate(Year = as.character(Year)) -> rel_age_total_bar_585_data
rel_age_total_bar_585_data %>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = CoV)) +
  theme_bw()+
  facet_wrap(~SSP)+
  ylab("Relative disparity") +
  theme(legend.position = "none")-> rel_age_total_bar_585
rel_age_total_bar_585

#### combine -----

##### absolute combine ----
abs_age_bar_spp_combine <- abs_age_bar_126_data %>%
  rbind(abs_age_bar_245_data)%>%
  rbind(abs_age_bar_370_data)%>%
  rbind(abs_age_bar_585_data)

abs_age_total_bar_ssp_data_combine <- abs_age_total_bar_126_data %>%
  rbind(abs_age_total_bar_245_data) %>%
  rbind(abs_age_total_bar_370_data) %>%
  rbind(abs_age_total_bar_585_data) 

abs_age_bar_spp_combine %>%
  ggplot(aes(x = variable, y = q_minus_miu*100, color = Year))+
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  facet_wrap(~SSP, ncol = 4)+
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank())-> abs_age_bar_spp_combine_plot
abs_age_bar_spp_combine_plot

abs_age_total_bar_ssp_data_combine %>%
  mutate(label = "Overall absolute disparity") %>%
  ggplot(aes(x = SSP, y = total_absolute*100, color = Year))+
  geom_boxplot() +
  ylab("Absolute disparity") +
  scale_color_discrete(name = "Year") +
  facet_wrap(~label, ncol = 1)+
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(seq(0, 0.3, by = 0.1)), limits = c(0, 0.3), expand = c(0, 0)) -> abs_age_total_bar_ssp_data_combine_plot
abs_age_total_bar_ssp_data_combine_plot

fig5_ab <- ggarrange(abs_age_bar_spp_combine_plot, abs_age_total_bar_ssp_data_combine_plot,
                     labels = c("A", "B"),
                     widths = c(3, 1.2),  
                     common.legend = T,
                     legend = "bottom")
fig5_ab

##### relative combine ----
rel_age_bar_spp_combine <- rel_age_bar_126_data %>%
  rbind(rel_age_bar_245_data)%>%
  rbind(rel_age_bar_370_data)%>%
  rbind(rel_age_bar_585_data)

rel_age_total_bar_ssp_data_combine <- rel_age_total_bar_126_data %>%
  rbind(rel_age_total_bar_245_data) %>%
  rbind(rel_age_total_bar_370_data) %>%
  rbind(rel_age_total_bar_585_data) 

rel_age_bar_spp_combine %>%
  ggplot(aes(x = Age, y = q_ratio, color = as.character(Year)))+
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")+
  ylab("Relative disparity") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  facet_wrap(~SSP, ncol = 4)+
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank())-> rel_age_bar_spp_combine_plot
rel_age_bar_spp_combine_plot

rel_age_total_bar_ssp_data_combine %>%
  mutate(label = "Overall relative disparity") %>%
  ggplot(aes(x = SSP, y = CoV, color = Year))+
  geom_boxplot() +
  ylab("Relative disparity") +
  scale_color_discrete(name = "Year") +
  facet_wrap(~label, ncol = 1)+
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(axis.title.x = element_blank()) -> rel_age_total_bar_ssp_data_combine_plot
rel_age_total_bar_ssp_data_combine_plot

fig5_cd <- ggarrange(rel_age_bar_spp_combine_plot, rel_age_total_bar_ssp_data_combine_plot,
                     labels = c("C", "D"),
                     widths = c(3, 1.2),  
                     common.legend = T,
                     legend = "bottom")
fig5_cd 

fig5 <- ggarrange(fig5_ab, fig5_cd,
                  ncol = 1)
fig5

ggsave("~/Documents/ResearchUNC/Heat/Figures/figs5.pdf", width = 12.5, height = 8.75)

#ggsave("~/Documents/ResearchUNC/Heat/Figures/figs5.png", width = 10, height = 7)
