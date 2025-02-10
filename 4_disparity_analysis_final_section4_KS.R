# Program Name: 4_disparity_analysis_final_section4_KS.R
# Date Last Modified: Jan, 2025
# Program Purpose: Disparity in heat exposure,
#                  Regional disparity
# Input Files:  CMIP6: merged.new2.csv
#               county-level geometry data 
#               monthly HI data: iam_HI_data.csv
# Author: Kaihui Song
# Contact: kaihuis@berkeley.edu
# Affiliation: Energy and Resources Group, University of California, Berkeley
# ------------------------------------------------------------------------------

#1. global setting ----

# library
rm(list=ls())
{
  library(tidyverse)
  library(raster)
  library(sp)
  library("epwshiftr")
  library(readr)
  library(usmap)
  library(sf)
  library(raster)
  library(weathermetrics)
  library(hrbrthemes)
  library(RColorBrewer)
  library(scales)
  library(maditr)
  library(reshape)
  library(reshape2)
  library(ggpubr) 
  library(forcats)
  library(DescTools)
  library(scales)
  library(tmap)
  library(tmaptools)
  library(tidycensus)
  library(readxl)
  library(twfy)
  library(viridis)
}


# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 
`%!in%` <- compose(`!`, `%in%`)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")


# directory

dir = "/Users/songkaihui/Documents/CMIP6/data"
setwd(dir)

# figure labels
ssp_names <- list(
  'ssp119'="SSP1-RCP1.9",
  'ssp126'="SSP1-RCP2.6",
  'ssp245'="SSP2-RCP4.5",
  'ssp370'="SSP3-RCP7.0",
  'ssp585'="SSP5-RCP8.5"
)
ssp_labeller <- function(variable,value){
  return(ssp_names[value])
}
HI.color = c("Safe" = "lightgoldenrod1",
             "Caution" = "gold1",
             "Extreme Caution" = "orange1",
             "Danger" = "darkorange1")

time.color = c("Safe" = "lightgoldenrod1",
             "Caution" = "gold1",
             "Extreme Caution" = "orange1",
             "Danger" = "darkorange1")

facet.labs <- c("June", "July", "August")
names(facet.labs) <- c("06", "07", "08")
Months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# data input:
county_boundary <- st_read("/Users/songkaihui/Documents/CMIP6/data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
  filter(STATEFP %!in% c("02", "15","60","66","69", "72","77"))
state_boundary = us_map("states") %>%
  filter(abbr %!in% c("AK", "HI"))
uscb_region_state <- read_csv("uscb_region_state.csv")
merge <- read_csv("~/Documents/CMIP6/data/merged.new2.csv")
iam_HI_data <- read_csv("~/Documents/CMIP6/data/iam_HI_data_v3.csv")


# 2. analyses ----

merge <- merge %>% 
  left_join(uscb_region_state, by = c("STATE_NAME" = "State")) %>%
  mutate(HI_level = case_when(median.HI.summer<80 ~ "Safe",
                              median.HI.summer>=80 & median.HI.summer<90 ~ "Caution",
                              median.HI.summer>=90 & median.HI.summer<103 ~ "Extreme Caution",
                              median.HI.summer>= 103 ~ "Danger")) %>%
  mutate(SSP = case_when(SSP == "ssp119" ~ "SSP1-RCP1.9",
                         SSP == "ssp126" ~ "SSP1-RCP2.6",
                         SSP == "ssp245" ~ "SSP2-RCP4.5",
                         SSP == "ssp370" ~ "SSP3-RCP7.0",
                         SSP == "ssp585" ~ "SSP5-RCP8.5"))

iam_HI_data <- iam_HI_data %>%
  mutate(SSP = case_when(SSP == "ssp119" ~ "SSP1-RCP1.9",
                         SSP == "ssp126" ~ "SSP1-RCP2.6",
                         SSP == "ssp245" ~ "SSP2-RCP4.5",
                         SSP == "ssp370" ~ "SSP3-RCP7.0",
                         SSP == "ssp585" ~ "SSP5-RCP8.5"))

## 2.1 Regional disparity (race/ethnicity) -----

merge %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop, HI_level, Region) %>%
  group_by(Year, SSP, Region) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(extreme_caution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(Year, SSP, extreme_caution_plus_flag, Region) %>%
  mutate(total_tot_extreme_caution = sum(TotPop, na.rm = T),    
         total_white_extreme_caution = sum(WhitePop, na.rm = T),
         total_black_extreme_caution = sum(BlackPop, na.rm = T),
         total_hispanic_extreme_caution = sum(HispanicPop, na.rm = T),
         total_otherrace_extreme_caution = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_extreme_caution = total_tot_extreme_caution/total_pop,
         q_white_extreme_caution = total_white_extreme_caution/total_white,
         q_black_extreme_caution = total_black_extreme_caution/total_black,
         q_hispanic_extreme_caution = total_hispanic_extreme_caution/total_hispanic,
         q_otherrace_extreme_caution = total_otherrace_extreme_caution/total_otherrace) %>%
  filter(extreme_caution_plus_flag == "Yes") -> temp_extreme_caution

temp_extreme_caution %>%
  dplyr::select(Year, SSP, Region, q_tot_extreme_caution, q_white_extreme_caution, 
                q_black_extreme_caution, q_hispanic_extreme_caution, q_otherrace_extreme_caution) %>%
  distinct() %>%
  group_by(Region, SSP) %>%
  mutate(miu_q = (q_white_extreme_caution+q_black_extreme_caution+q_hispanic_extreme_caution+q_otherrace_extreme_caution)/4,
         miu_q2 = q_tot_extreme_caution)  %>%
  ungroup() %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_extreme_caution_region


#### total absolute disparity --------
disparity_q_extreme_caution_region %>%
  group_by(Region, SSP) %>%
  mutate(total_q_abs = abs(q_black_extreme_caution - miu_q) + abs(q_hispanic_extreme_caution - miu_q) +
           abs(q_otherrace_extreme_caution - miu_q) + abs(q_white_extreme_caution - miu_q))  %>%
  ungroup() %>%
  mutate(black_qper = abs(q_black_extreme_caution - miu_q)/total_q_abs*100,
         hispanic_qper = abs(q_hispanic_extreme_caution - miu_q)/total_q_abs*100,
         others_qper =  abs(q_otherrace_extreme_caution - miu_q)/total_q_abs*100,
         white_qper = abs(q_white_extreme_caution - miu_q)/total_q_abs*100)-> total_q

#write_csv(total_q, "/Users/songkaihui/Downloads/total_q.csv")

total_q %>%
  dplyr::select(SSP, Year, Region,total_q_abs) %>%
  mutate(Year = as.character(Year)) %>%
  pivot_wider(names_from = "Region",values_from = total_q_abs) %>%
  mutate_at(vars(South:Northeast),  replace_na, 0) %>%
  pivot_longer(cols = South:Northeast, names_to = "Region", values_to = "total_q_abs")%>%
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(x = Year, y = total_q_abs, color = Region, group = Region)) + 
  geom_line() +
  facet_wrap(~SSP) +
  theme_bw() +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "right",
        plot.subtitle = element_text(size = 10, face = "bold")) +
  xlab("Year") +
  ylab("Total regional absolute disparity")

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/regiona_disparity_abs_race.png", width = 7, height = 6)


#### total relative disparity --------

disparity_q_extreme_caution_region %>%
  dplyr::select(-miu_q2) %>%
  pivot_longer(q_white_extreme_caution:q_otherrace_extreme_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, SSP, Region) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  dplyr::select(Year, SSP, Region, CoV) %>%
  distinct() %>%
  pivot_wider(names_from = "Region",values_from = CoV) %>%
  mutate_at(vars(South:Northeast),  replace_na, 0) %>%
  pivot_longer(cols = South:Northeast, names_to = "Region", values_to = "CoV") %>%
  ggplot()+
  geom_line(aes(x = Year, y = CoV, color = Region)) +
  facet_wrap(~SSP) +
  guides(color= guide_legend(title="Region"))+
  ylab("Relative disparity across all race/ethnicity") +
  theme_bw() + 
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "right",
        plot.subtitle = element_text(size = 10, face = "bold"))-> relative_disparity_all_extreme_caution
relative_disparity_all_extreme_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/regiona_disparity_rel_race.png", width = 7, height = 6)


## 2.2 Regional disparity (age) -----

merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(Year, SSP, GEOID, median.HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop, HI_level) %>%
  group_by(Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult= sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(extreme_caution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(Year, SSP, extreme_caution_plus_flag) %>%
  mutate(total_young_extreme_caution = sum(YoungPop, na.rm = T),
         total_adult_extreme_caution = sum(AdultPop, na.rm = T),
         total_elderly_extreme_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_extreme_caution = total_young_extreme_caution/total_young,
         q_adult_extreme_caution = total_adult_extreme_caution/total_adult,
         q_elderly_extreme_caution = total_elderly_extreme_caution/total_elderly) %>%
  filter(extreme_caution_plus_flag == "Yes") -> temp_extreme_caution

temp_extreme_caution %>%
  dplyr::select(Year, SSP, q_young_extreme_caution, q_adult_extreme_caution, q_elderly_extreme_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_extreme_caution+q_adult_extreme_caution+q_elderly_extreme_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_extreme_caution_age


map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, Region, median.HI.summer, TotPop, YoungPop, AdultPop, ElderlyPop, Young_per, Adult_per, Elderly_per, HI_level)

map_age_per %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, TotPop, YoungPop, AdultPop ,ElderlyPop, HI_level, Region) %>%
  group_by(Year, SSP, Region) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_young = sum(YoungPop, na.rm = T),
         total_adult = sum(AdultPop, na.rm = T),
         total_elderly = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(extreme_caution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                               TRUE ~"No")) %>%
  group_by(Year, SSP, extreme_caution_plus_flag, Region) %>%
  mutate(total_tot_extreme_caution = sum(TotPop, na.rm = T),    
         total_young_extreme_caution = sum(YoungPop, na.rm = T),
         total_adult_extreme_caution = sum(AdultPop, na.rm = T),
         total_elderly_extreme_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_tot_extreme_caution = total_tot_extreme_caution/total_pop,
         q_young_extreme_caution = total_young_extreme_caution/total_young,
         q_adult_extreme_caution = total_adult_extreme_caution/total_adult,
         q_elderly_extreme_caution = total_elderly_extreme_caution/total_elderly) %>%
  filter(extreme_caution_plus_flag == "Yes") -> temp_extreme_caution

temp_extreme_caution %>%
  dplyr::select(Year, SSP, Region, q_tot_extreme_caution, q_young_extreme_caution, 
                q_adult_extreme_caution, q_elderly_extreme_caution) %>%
  distinct() %>%
  group_by(Region, SSP) %>%
  mutate(miu_q = (q_young_extreme_caution+q_adult_extreme_caution+q_elderly_extreme_caution)/3,
         miu_q2 = q_tot_extreme_caution)  %>%
  ungroup() %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_extreme_caution_region


#### total absolute disparity --------
disparity_q_extreme_caution_region %>%
  group_by(Region, SSP) %>%
  mutate(total_q_abs = abs(q_young_extreme_caution - miu_q) + abs(q_adult_extreme_caution - miu_q) +
           abs(q_elderly_extreme_caution - miu_q))  %>%
  ungroup() %>%
  mutate(young_qper = abs(q_young_extreme_caution - miu_q)/total_q_abs*100,
         adult_qper = abs(q_adult_extreme_caution - miu_q)/total_q_abs*100,
         elderly_qper =  abs(q_elderly_extreme_caution - miu_q)/total_q_abs*100)-> total_q

#write_csv(total_q, "/Users/songkaihui/Downloads/total_q.csv")

total_q %>%
  dplyr::select(SSP, Year, Region,total_q_abs) %>%
  mutate(Year = as.character(Year)) %>%
  pivot_wider(names_from = "Region",values_from = total_q_abs) %>%
  mutate_at(vars(South:Northeast),  replace_na, 0) %>%
  pivot_longer(cols = South:Northeast, names_to = "Region", values_to = "total_q_abs")%>%
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(x = Year, y = total_q_abs, color = Region, group = Region)) + 
  geom_line() +
  facet_wrap(~SSP) +
  theme_bw() +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "right",
        plot.subtitle = element_text(size = 10, face = "bold")) +
  xlab("Year") +
  ylab("Total regional absolute disparity")

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/regiona_disparity_abs_age.png", width = 7, height = 6)

#### total relative disparity --------

disparity_q_extreme_caution_region %>%
  dplyr::select(-miu_q2) %>%
  pivot_longer(q_young_extreme_caution:q_elderly_extreme_caution, names_to = "variable", values_to = "q") %>%
  group_by(Year, SSP, Region) %>%
  mutate(sqrt_var_q = sqrt(var(q))) %>%
  ungroup () %>%
  dplyr::select(-q, -variable) %>%
  distinct() %>%
  mutate(CoV = sqrt_var_q/miu_q) %>%
  dplyr::select(Year, SSP, Region, CoV) %>%
  distinct() %>%
  pivot_wider(names_from = "Region",values_from = CoV) %>%
  mutate_at(vars(South:Northeast),  replace_na, 0) %>%
  pivot_longer(cols = South:Northeast, names_to = "Region", values_to = "CoV") %>%
  ggplot()+
  geom_line(aes(x = Year, y = CoV, color = Region)) +
  facet_wrap(~SSP) +
  guides(color= guide_legend(title="Region"))+
  ylab("Relative disparity across all age") +
  theme_bw() + 
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "right",
        plot.subtitle = element_text(size = 10, face = "bold"))-> relative_disparity_all_extreme_caution
relative_disparity_all_extreme_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/regiona_disparity_rel_age.png", width = 7, height = 6)

