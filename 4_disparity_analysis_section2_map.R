# Program Name: 4_disparity_analysis_final_section2_KS.R
# Date Last Modified: June, 2025
# Program Purpose: Disparity in heat exposure,
#                  producing Figures 3&5 and SI Figures
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


# 2. Population-weighted Heat Index ----

## 2.0 rename -----
merge <- merge %>% 
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


## 2.1 By race -----

merge %>%
  dplyr::select(Time.label, GEOID, SSP, HI_level, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate_at(vars(TotPop), ~replace_na(., 0)) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_Tot = sum(TotPop),
         Tot_per = TotPop/TOTALPOP_Tot * median.HI.summer) %>%
  summarise(Tot_weighted = sum(Tot_per))->total_weighted

merge %>%
  dplyr::select(Time.label, GEOID, SSP, HI_level, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate_at(vars(WhitePop), ~replace_na(., 0)) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_White = sum(WhitePop),
         White_per = WhitePop/TOTALPOP_White * median.HI.summer) %>%
  summarise(White_weighted = sum(White_per))->white_weighted

merge %>%
  dplyr::select(Time.label, GEOID, SSP, HI_level, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate_at(vars(BlackPop), ~replace_na(., 0)) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_Black = sum(BlackPop),
         Black_per = BlackPop/TOTALPOP_Black * median.HI.summer) %>%
  summarise(Black_weighted = sum(Black_per))->black_weighted

merge %>%
  dplyr::select(Time.label, GEOID, SSP, HI_level, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate_at(vars(HispanicPop), ~replace_na(., 0)) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_Hispanic = sum(HispanicPop),
         Hispanic_per = HispanicPop/TOTALPOP_Hispanic * median.HI.summer) %>%
  summarise(Hispanic_weighted = sum(Hispanic_per))-> hispanic_weighted

merge %>%
  dplyr::select(Time.label, GEOID, SSP, HI_level, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop) %>%
  mutate_at(vars(OtherRacePop), ~replace_na(., 0)) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_OtherRace = sum(OtherRacePop),
         OtherRace_per = OtherRacePop/TOTALPOP_OtherRace * median.HI.summer) %>%
  summarise(OtherRace_weighted = sum(OtherRace_per))-> otherrace_weighted

total_weighted %>%
  left_join(white_weighted, by = c("SSP", "Time.label")) %>%
  left_join(black_weighted, by = c("SSP", "Time.label")) %>%
  left_join(hispanic_weighted, by = c("SSP", "Time.label")) %>%
  left_join(otherrace_weighted, by = c("SSP", "Time.label")) %>%
  pivot_longer(cols = Tot_weighted:OtherRace_weighted, names_to = "Race", values_to = "HI") %>%
  mutate(HI = round(HI, digits = 1)) %>%
  pivot_wider(names_from = "Time.label", 
            values_from = "HI") %>%
  filter(SSP != "SSP1-RCP1.9") %>%
  filter(Race != "Tot_weighted") %>%
  mutate(Race = replace(Race, Race == "White_weighted", "Non-Hispanic White"),
         Race = replace(Race, Race == "Black_weighted", "Non-Hispanic Black"),
         Race = replace(Race, Race == "Hispanic_weighted", "Hispanic (all races)"),
         Race = replace(Race, Race == "OtherRace_weighted", "Non-Hispanic other races")) %>%
  dplyr::rename("2020" = "Base") %>%
  dplyr::rename("2050" = "Mid") %>%
  dplyr::rename("2100" = "End")-> TableS8_pop_weighted_HI_byrace



## 2.2 By Age -----

merge %>%
  replace(is.na(.), 0) %>%
  dplyr::select(Time.label, SSP, GEOID, median.HI.summer,HI_level,
                TotPop,starts_with("Age")) %>%
  mutate(G1 = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # young
         G2 = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         G3 = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(-starts_with("Age")) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_old = sum(G3),
         old_per = G3/TOTALPOP_old * median.HI.summer) %>%
  summarise(Old_weighted = round(sum(old_per), digits = 2)) %>%
  pivot_wider(names_from = Time.label, values_from = Old_weighted) %>%
  mutate(Age = "Elderly") ->old_weighted

merge %>%
  replace(is.na(.), 0) %>%
  dplyr::select(Time.label, SSP, GEOID, median.HI.summer,HI_level,
                TotPop,starts_with("Age")) %>%
  mutate(G1 = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # young
         G2 = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         G3 = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(-starts_with("Age")) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_adult = sum(G2),
         adult_per = G2/TOTALPOP_adult * median.HI.summer) %>%
  summarise(Adult_weighted = round(sum(adult_per), digits = 2)) %>%
  pivot_wider(names_from = Time.label, values_from = Adult_weighted) %>%
  mutate(Age = "Adult") ->adult_weighted

merge %>%
  replace(is.na(.), 0) %>%
  dplyr::select(Time.label, SSP, GEOID, median.HI.summer,HI_level,
                TotPop,starts_with("Age")) %>%
  mutate(G1 = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # young
         G2 = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         G3 = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  dplyr::select(-starts_with("Age")) %>%
  group_by(SSP, Time.label) %>%
  mutate(TOTALPOP_young = sum(G1),
         young_per = G1/TOTALPOP_young * median.HI.summer) %>%
  summarise(Young_weighted = round(sum(young_per), digits = 2)) %>%
  pivot_wider(names_from = Time.label, values_from = Young_weighted) %>%
  mutate(Age = "Young") ->young_weighted


old_weighted %>%
  rbind(adult_weighted) %>%
  rbind(young_weighted) %>%
  dplyr::select(Age, "SSP-RCP scenarios" = SSP, "2020" = Base, "2050" = Mid, "2100" = End) %>%
  filter(`SSP-RCP scenarios` != "SSP1-RCP1.9")-> TableS10_popweighted_HI_age

  
# 3 Percent under risks -----

## 3.1 Race ----


### 3.1.1 Percentage of pop above Caution by racial groups (q) ----

merge %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop, HI_level) %>%
  group_by(Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(Year, SSP, caution_plus_flag) %>%
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
  filter(caution_plus_flag == "Yes") -> temp_caution_race

temp_caution_race %>%
  dplyr::select(Year, SSP, q_tot_caution, q_white_caution, q_black_caution, q_hispanic_caution, q_otherrace_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_caution+q_black_caution+q_hispanic_caution+q_otherrace_caution)/4,
         miu_q2 = q_tot_caution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_caution_race
  

#### plot of q  --------

disparity_q_caution_race %>%
  pivot_longer(cols = q_tot_caution:miu_q2, names_to = "var", values_to = "q") %>%
  filter(var %in% c("q_black_caution", "q_hispanic_caution", "q_otherrace_caution", "q_white_caution", "miu_q")) %>%
  mutate(var = replace(var, var == "q_black_caution", "Non-Hispanic Black"),
         var = replace(var, var == "q_white_caution", "Non-Hispanic White"),
         var = replace(var, var == "q_hispanic_caution", "Hispanic (all races)"),
         var = replace(var, var == "q_otherrace_caution", "Non-Hispanic other races"),
         var = replace(var, var == "miu_q", "Average")) -> q_caution_plot

ggplot() +
  geom_line(data = q_caution_plot, aes(x = Year, y = q*100, color = var)) +
  facet_wrap(~SSP) + 
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("Average" = "black", 
                                "Non-Hispanic White" = "#729ECE", 
                                "Non-Hispanic Black" = "#FF9E4A", 
                                "Hispanic (all races)" = "#67BF5C", 
                                "Non-Hispanic other races" = "#AD8BC9")) +
  guides(color=guide_legend(title="Race/Ethnicity")) +
  ylab("Population exposed to Caution heat risk (%)")+
  xlab("Year") -> FigS11_q_caution_race

FigS11_q_caution_race

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS7_q_caution_race.png", width = 7, height =6)


#### maps 2020, 2050, 2100 (SSP2-RCP4.5) -----

merge_race_per <- merge %>%
  mutate(White_per = WhitePop/TotPop,
         Black_per = BlackPop/TotPop,
         Hispanic_per = HispanicPop/TotPop,
         Other_per = OtherRacePop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, White_per, Black_per, Hispanic_per, Other_per, HI_level)

merge_race_per %>% dplyr::select(GEOID) %>% unique() -> n_county

merge_race_per_data <- left_join(county_boundary, merge_race_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_white = order(order(White_per, decreasing = F))/nrow(county_boundary),
         rank_black = order(order(Black_per, decreasing = F))/nrow(county_boundary),
         rank_hispanic = order(order(Hispanic_per, decreasing = F))/nrow(county_boundary),
         rank_otherrace = order(order(Other_per, decreasing = F))/nrow(county_boundary))


# maps 2020

merge_race_per_data %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2020")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2020

map_race_caution_2020

# map 2050 
  
merge_race_per_data %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2050")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2050

map_race_caution_2050

# map 2100 
merge_race_per_data  %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2100")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2100
map_race_caution_2100

map_race_caution <- ggarrange(map_race_caution_2020, map_race_caution_2050, map_race_caution_2100,
                              labels = c("2020", "2050", "2100"),
                              nrow = 3,
                              common.legend = T,
                              legend = "bottom")

annotate_figure(map_race_caution, top = text_grob("Population percentile in exposure to HI above Caution under SSP2-RCP4.5", 
                                      color = "black", face = "bold", size = 11))
map_race_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS9_map_caution_race_SSP2RCP4.5.png", width = 10, height = 8)
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/map_caution_race_SSP5RCP8.5.png", width = 10, height = 8)


#### maps 2020, 2050, 2100 (SSP5-RCP8.5) -----

merge_race_per <- merge %>%
  mutate(White_per = WhitePop/TotPop,
         Black_per = BlackPop/TotPop,
         Hispanic_per = HispanicPop/TotPop,
         Other_per = OtherRacePop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, White_per, Black_per, Hispanic_per, Other_per, HI_level)

merge_race_per %>% dplyr::select(GEOID) %>% unique() -> n_county

merge_race_per_data <- left_join(county_boundary, merge_race_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_white = order(order(White_per, decreasing = F))/nrow(county_boundary),
         rank_black = order(order(Black_per, decreasing = F))/nrow(county_boundary),
         rank_hispanic = order(order(Hispanic_per, decreasing = F))/nrow(county_boundary),
         rank_otherrace = order(order(Other_per, decreasing = F))/nrow(county_boundary))

# maps 2020

merge_race_per_data %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2020")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2020

map_race_caution_2020

# map 2050 

merge_race_per_data %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2050")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2050

map_race_caution_2050

# map 2100 
merge_race_per_data  %>%
  left_join(temp_caution_race %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2100")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_caution_2100
map_race_caution_2100

map_race_caution <- ggarrange(map_race_caution_2020, map_race_caution_2050, map_race_caution_2100,
                              labels = c("2020", "2050", "2100"),
                              nrow = 3,
                              common.legend = T,
                              legend = "bottom")

annotate_figure(map_race_caution, top = text_grob("Population percentile in exposure to HI above Caution under SSP2-RCP4.5", 
                                                  color = "black", face = "bold", size = 11))
map_race_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS10_map_caution_race_SSP5RCP8.5.png", width = 10, height = 8)



### 3.1.2 Percentage of pop above Extreme Caution zone by racial groups (q) ----

merge %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, TotPop, WhitePop, BlackPop, HispanicPop, OtherRacePop, HI_level) %>%
  group_by(Year, SSP) %>%
  mutate(total_pop = sum(TotPop, na.rm = T),
         total_black = sum(BlackPop, na.rm = T),
         total_white = sum(WhitePop, na.rm = T),
         total_hispanic = sum(HispanicPop, na.rm = T),
         total_otherrace  = sum(OtherRacePop, na.rm = T)) %>%
  ungroup() %>%
  mutate(extreme_caution_plus_flag = case_when(HI_level %in% c("Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(Year, SSP, extreme_caution_plus_flag) %>%
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
  filter(extreme_caution_plus_flag == "Yes") -> temp_extreme_caution_race
  
temp_extreme_caution_race %>%
  dplyr::select(Year, SSP, q_white_extreme_caution, q_black_extreme_caution, 
                q_hispanic_extreme_caution, q_otherrace_extreme_caution, q_tot_extreme_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_white_extreme_caution + q_black_extreme_caution + q_hispanic_extreme_caution + q_otherrace_extreme_caution)/4,
         miu_q2 = q_tot_extreme_caution)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_extreme_caution_race

###### plot of q --------

disparity_q_extreme_caution_race %>%
  pivot_longer(cols = q_white_extreme_caution:miu_q2, names_to = "var", values_to = "q") %>%
  filter(var %in% c("q_black_extreme_caution", "q_hispanic_extreme_caution", "q_otherrace_extreme_caution", "q_white_extreme_caution", "miu_q")) %>%
  mutate(var = replace(var, var == "q_black_extreme_caution", "Non-Hispanic Black"),
         var = replace(var, var == "q_white_extreme_caution", "Non-Hispanic White"),
         var = replace(var, var == "q_hispanic_extreme_caution", "Hispanic (all races)"),
         var = replace(var, var == "q_otherrace_extreme_caution", "Non-Hispanic other races"),
         var = replace(var, var == "miu_q", "Average")) -> q_extreme_caution_race_plot


ggplot() +
  geom_line(data = q_extreme_caution_race_plot, aes(x = Year, y = q*100, color = var)) +
  facet_wrap(~SSP) + 
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("Average" = "black", 
                                "Non-Hispanic White" = "#729ECE", 
                                "Non-Hispanic Black" = "#FF9E4A", 
                                "Hispanic (all races)" = "#67BF5C", 
                                "Non-Hispanic other races" = "#AD8BC9")) +
  guides(color=guide_legend(title="Race/Ethnicity")) +
  ylab("Population exposed to Extreme Caution heat risk (%)")+
  xlab("Year") -> FigS7_q_extreme_caution_race
FigS7_q_extreme_caution_race

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS7_q_extreme_caution_race.png", width = 7, height =6)

#### maps 2020, 2050, 2100 (SSP2-RCP4.5) -----

# map 2020

map_race_per <- merge %>%
  mutate(White_per = WhitePop/TotPop,
         Black_per = BlackPop/TotPop,
         Hispanic_per = HispanicPop/TotPop,
         Other_per = OtherRacePop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, White_per, Black_per, Hispanic_per, Other_per)
map_race_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_race_per_data <- left_join(county_boundary, map_race_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_white = order(order(White_per, decreasing = F))/nrow(county_boundary),
         rank_black = order(order(Black_per, decreasing = F))/nrow(county_boundary),
         rank_hispanic = order(order(Hispanic_per, decreasing = F))/nrow(county_boundary),
         rank_otherrace = order(order(Other_per, decreasing = F))/nrow(county_boundary))

map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2020")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2020
map_race_extreme_caution_2020

# map 2050 

map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2050")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2050
map_race_extreme_caution_2050

# map 2100 
map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2100")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2100
map_race_extreme_caution_2100

map_race_extreme_caution <- ggarrange(map_race_extreme_caution_2020, map_race_extreme_caution_2050, map_race_extreme_caution_2100,
                              labels = c("2020", "2050", "2100"),
                              nrow = 3,
                              common.legend = T,
                              legend = "bottom")

annotate_figure(map_race_extreme_caution, top = text_grob("Population percentile in exposure to HI above Extreme Caution under SSP5-RCP8.5", 
                                                  color = "black", face = "bold", size = 14))
map_race_extreme_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS5_map_extreme_caution_race_SSP2RCP4.5.png", width = 10, height = 8)
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/map_extreme_caution_race_SSP5RCP8.5.png", width = 10, height = 8)


#### maps 2020, 2050, 2100 (SSP5-RCP8.5) -----

# map 2020

map_race_per <- merge %>%
  mutate(White_per = WhitePop/TotPop,
         Black_per = BlackPop/TotPop,
         Hispanic_per = HispanicPop/TotPop,
         Other_per = OtherRacePop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, White_per, Black_per, Hispanic_per, Other_per)
map_race_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_race_per_data <- left_join(county_boundary, map_race_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_white = order(order(White_per, decreasing = F))/nrow(county_boundary),
         rank_black = order(order(Black_per, decreasing = F))/nrow(county_boundary),
         rank_hispanic = order(order(Hispanic_per, decreasing = F))/nrow(county_boundary),
         rank_otherrace = order(order(Other_per, decreasing = F))/nrow(county_boundary))

map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2020")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2020
map_race_extreme_caution_2020

# map 2050 

map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2050")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2050
map_race_extreme_caution_2050

# map 2100 
map_race_per_data  %>%
  left_join(temp_extreme_caution_race %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2100")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_white", "rank_black", "rank_hispanic","rank_otherrace"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_white", "rank_black", "rank_hispanic", "rank_otherrace")) %>% 
  mutate(var = case_when(var == "rank_white" ~ "Non-Hispanic White",
                         var == "rank_black" ~ "Non-Hispanic Black",
                         var == "rank_hispanic" ~ "Hispanic (all races)",
                         var == "rank_otherrace" ~ "Non-Hispanic other races")) %>%
  filter(var!="Non-Hispanic other races") %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_race_extreme_caution_2100
map_race_extreme_caution_2100

map_race_extreme_caution <- ggarrange(map_race_extreme_caution_2020, map_race_extreme_caution_2050, map_race_extreme_caution_2100,
                                      labels = c("2020", "2050", "2100"),
                                      nrow = 3,
                                      common.legend = T,
                                      legend = "bottom")

annotate_figure(map_race_extreme_caution, top = text_grob("Population percentile in exposure to HI above Extreme Caution under SSP5-RCP8.5", 
                                                          color = "black", face = "bold", size = 14))
map_race_extreme_caution

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS6_map_extreme_caution_race_SSP5RCP8.5.png", width = 10, height = 8)

## 3.2 Age ----

### 3.2.1 Percentage of pop above Caution zone by age groups (q) ----

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
  mutate(caution_plus_flag = case_when(HI_level %in% c("Caution", "Extreme Caution", "Danger") ~ "Yes",
                                       TRUE ~"No")) %>%
  group_by(Year, SSP, caution_plus_flag) %>%
  mutate(total_young_caution = sum(YoungPop, na.rm = T),
         total_adult_caution = sum(AdultPop, na.rm = T),
         total_elderly_caution = sum(ElderlyPop, na.rm = T)) %>%
  ungroup() %>%
  mutate(q_young_caution = total_young_caution/total_young,
         q_adult_caution = total_adult_caution/total_adult,
         q_elderly_caution = total_elderly_caution/total_elderly) %>%
  filter(caution_plus_flag == "Yes") -> temp_caution_age

temp_caution_age %>%
  dplyr::select(Year, SSP, q_young_caution, q_adult_caution, q_elderly_caution) %>%
  distinct() %>%
  mutate(miu_q = (q_young_caution+q_adult_caution+q_elderly_caution)/3)  %>%
  filter(SSP != "SSP1-RCP1.9") -> disparity_q_caution_age


#### plot of q --------

disparity_q_caution_age %>%
  pivot_longer(cols = q_young_caution:miu_q, names_to = "var", values_to = "q") %>%
  filter(var %in% c("q_young_caution", "q_adult_caution", "q_elderly_caution", "miu_q")) %>%
  mutate(var = replace(var, var == "q_young_caution", "Young (<20 y/o)"),
         var = replace(var, var == "q_adult_caution", "Adult (20-65 y/o)"),
         var = replace(var, var == "q_elderly_caution", "Elderly (65+ y/o)"),
         var = replace(var, var == "miu_q", "Average")) -> q_caution_age_plot


ggplot() +
  geom_line(data = q_caution_age_plot, aes(x = Year, y = q*100, color = var)) +
  facet_wrap(~SSP) + 
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("Average" = "black", 
                                "Young (<20 y/o)" = "#729ECE", 
                                "Adult (20-65 y/o)" = "#FF9E4A", 
                                "Elderly (65+ y/o)" = "#67BF5C")) +
  guides(color=guide_legend(title="Race/Ethnicity")) +
  ylab("Population exposed to Extreme Caution heat risk (%)")+
  xlab("Year") -> q_caution_age
q_caution_age

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS7_q_extreme_caution_race.png", width = 7, height =6)


#### maps 2020, 2050, 2100 (SSP2-RCP4.5) -----

# 2020
map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, Young_per, Adult_per, Elderly_per, HI_level)
map_age_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_age_per_data <- left_join(county_boundary, map_age_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_young = order(order(Young_per, decreasing = F))/nrow(county_boundary),
         rank_adult = order(order(Adult_per, decreasing = F))/nrow(county_boundary),
         rank_elderly = order(order(Elderly_per, decreasing = F))/nrow(county_boundary))

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2020")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2020
map_age_caution_2020

# map 2050 

map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, Young_per, Adult_per, Elderly_per, HI_level)
map_age_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_age_per_data <- left_join(county_boundary, map_age_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_young = order(order(Young_per, decreasing = F))/nrow(county_boundary),
         rank_adult = order(order(Adult_per, decreasing = F))/nrow(county_boundary),
         rank_elderly = order(order(Elderly_per, decreasing = F))/nrow(county_boundary))

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2050")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2050
map_age_caution_2050

# map 2100 

map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, Young_per, Adult_per, Elderly_per, HI_level)
map_age_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_age_per_data <- left_join(county_boundary, map_age_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_young = order(order(Young_per, decreasing = F))/nrow(county_boundary),
         rank_adult = order(order(Adult_per, decreasing = F))/nrow(county_boundary),
         rank_elderly = order(order(Elderly_per, decreasing = F))/nrow(county_boundary))

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2100")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2100
map_age_caution_2100

map_age_caution <- ggarrange(map_age_caution_2020, map_age_caution_2050, map_age_caution_2100,
                              labels = c("2020", "2050", "2100"),
                              nrow = 3,
                              common.legend = T,
                              legend = "bottom")

annotate_figure(map_age_caution, top = text_grob("Population percentile in exposure to HI above Caution under SSP2-RCP4.5", 
                                                  color = "black", face = "bold", size = 14))
map_age_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/map_caution_age_SSP2RCP4.5.png", width = 10, height = 8)


#### maps 2020, 2050, 2100 (SSP5-RCP8.5) -----

map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, Young_per, Adult_per, Elderly_per, HI_level)
map_age_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_age_per_data <- left_join(county_boundary, map_age_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_young = order(order(Young_per, decreasing = F))/nrow(county_boundary),
         rank_adult = order(order(Adult_per, decreasing = F))/nrow(county_boundary),
         rank_elderly = order(order(Elderly_per, decreasing = F))/nrow(county_boundary))

# 2020

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2020")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2020
map_age_caution_2020

# map 2050 

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2050")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2050
map_age_caution_2050

# map 2100 

map_age_per_data  %>%
  left_join(temp_caution_age %>% dplyr::select(Year, SSP, GEOID, caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2100")) %>%
  filter(caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=9),
        legend.title = element_text(size = 9),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_caution_2100
map_age_caution_2100

map_age_caution <- ggarrange(map_age_caution_2020, map_age_caution_2050, map_age_caution_2100,
                             labels = c("2020", "2050", "2100"),
                             nrow = 3,
                             common.legend = T,
                             legend = "bottom")

annotate_figure(map_age_caution, top = text_grob("Population percentile in exposure to HI above Caution under SSP5-RCP8.5", 
                                                 color = "black", face = "bold", size = 14))
map_age_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/map_caution_age_SSP5RCP8.5.png", width = 10, height = 8)

### 3.2.2 Percentage of pop above Extreme Caution zone by age groups (q) ----

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

#### plot of q --------

disparity_q_extreme_caution_age %>%
  pivot_longer(cols = q_young_extreme_caution:miu_q, names_to = "var", values_to = "q") %>%
  filter(var %in% c("q_young_extreme_caution", "q_adult_extreme_caution", "q_elderly_extreme_caution", "miu_q")) %>%
  mutate(var = replace(var, var == "q_young_extreme_caution", "Young (<20 y/o)"),
         var = replace(var, var == "q_adult_extreme_caution", "Adult (20-65 y/o)"),
         var = replace(var, var == "q_elderly_extreme_caution", "Elderly (65+ y/o)"),
         var = replace(var, var == "miu_q", "Average")) -> q_extreme_caution_age_plot


ggplot() +
  geom_line(data = q_extreme_caution_age_plot, aes(x = Year, y = q*100, color = var)) +
  facet_wrap(~SSP) + 
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("Average" = "black", 
                                "Young (<20 y/o)" = "#729ECE", 
                                "Adult (20-65 y/o)" = "#FF9E4A", 
                                "Elderly (65+ y/o)" = "#67BF5C")) +
  guides(color=guide_legend(title="Race/Ethnicity")) +
  ylab("Population exposed to Extreme Caution heat risk (%)")+
  xlab("Year") -> q_extreme_caution_age
q_extreme_caution_age

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS7_q_extreme_caution_race.png", width = 7, height =6)


#### maps 2020, 2050, 2100 (SSP2-RCP4.5) -----

map_age_per <- merge %>%
  mutate(YoungPop = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         AdultPop = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12, # adult
         ElderlyPop = Age_group_13 +Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) %>% # old
  mutate(Young_per = YoungPop/TotPop,
         Adult_per = AdultPop/TotPop,
         Elderly_per = ElderlyPop/TotPop) %>%
  dplyr::select(Year, SSP, GEOID, median.HI.summer, Young_per, Adult_per, Elderly_per, HI_level)
map_age_per %>% dplyr::select(GEOID) %>% unique() -> n_county

map_age_per_data <- left_join(county_boundary, map_age_per, by=c("GEOID")) %>%
  group_by(SSP, Year) %>%
  mutate(rank_young = order(order(Young_per, decreasing = F))/nrow(county_boundary),
         rank_adult = order(order(Adult_per, decreasing = F))/nrow(county_boundary),
         rank_elderly = order(order(Elderly_per, decreasing = F))/nrow(county_boundary))

# map 2020 

map_age_per_data  %>%
  left_join(temp_extreme_caution %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP2-RCP4.5") %>% filter(Year == "2020")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_extreme_caution_2020
map_age_extreme_caution_2020

# map 2050 

map_age_per_data  %>%
  left_join(temp_extreme_caution %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2050")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_extreme_caution_2050
map_age_extreme_caution_2050

# map 2100 

map_age_per_data  %>%
  left_join(temp_extreme_caution %>% dplyr::select(Year, SSP, GEOID, extreme_caution_plus_flag) %>% 
              filter(SSP == "SSP5-RCP8.5") %>% filter(Year == "2100")) %>%
  filter(extreme_caution_plus_flag == "Yes") %>%
  pivot_longer(cols = c("rank_young", "rank_adult", "rank_elderly"), names_to = "var", values_to = "value") %>%
  filter(var %in% c("rank_young", "rank_adult", "rank_elderly")) %>% 
  mutate(var = case_when(var == "rank_young" ~ "Young",
                         var == "rank_adult" ~ "Adult",
                         var == "rank_elderly" ~ "Elderly")) %>%
  ggplot() + 
  geom_sf(aes(fill = value*100, group = var), color = NA)+
  geom_sf(data = state_boundary, fill = NA, color = "grey40") +
  scale_fill_viridis(direction = -1, discrete = F,
                     limits = c(0, 100),
                     breaks = c(0, 20, 40,60, 80, 100), ) +
  facet_wrap(~var) +
  theme_bw() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
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
        legend.position = "bottom") +
  labs(fill = "Population percentile") ->map_age_extreme_caution_2100
map_age_extreme_caution_2100

map_age_extreme_caution <- ggarrange(map_age_extreme_caution_2020, map_age_extreme_caution_2050, map_age_extreme_caution_2100,
                                      labels = c("2020", "2050", "2100"),
                                      nrow = 3,
                                      common.legend = T,
                                      legend = "bottom")

annotate_figure(map_age_extreme_caution, top = text_grob("Population percentile in exposure to HI above Extreme Caution under SSP2-RCP4.5", 
                                                          color = "black", face = "bold", size = 11))
map_age_extreme_caution

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/map_extreme_caution_age_SSP2RCP4.5.png", width = 10, height = 8)
##  3.3 bar chart for race composition and age composition -----

### 3.3.1 race composition ----

SSP_race <- merge %>% 
  replace(is.na(.), 0) %>%
  group_by(Time.label, SSP, HI_level) %>%
  summarise(Pop_sum=sum(TotPop, na.rm=TRUE),
            White_sum= sum(WhitePop, na.rm = TRUE),
            Black_sum = sum(BlackPop, na.rm = TRUE),
            Hispanic_sum = sum(HispanicPop, na.rm = TRUE),
            Other_sum = sum(OtherRacePop, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(HI_level %in% c("Safe", "Caution", "Extreme Caution", "Danger")) %>%
  group_by(SSP, Time.label, HI_level) %>%
  mutate(n_ssp=sum(Pop_sum),
         white_per=round(White_sum/n_ssp*100,2),
         hispanic_per = round(Hispanic_sum/n_ssp*100,2),
         black_per = round(Black_sum/n_ssp*100,2),
         other_per = round(Other_sum/n_ssp*100, 2))

race.color = c("Black(%)" = "tan",
               "Hispanic(%)" =  "slategray2",
               "White(%)" = "thistle3",
               "Others(%)" = "coral2")

SSP_race %>%
  arrange(Time.label, SSP) %>% 
  mutate(SSP=factor(SSP, levels = c("SSP5-RCP8.5", "SSP3-RCP7.0", 
                                    "SSP2-RCP4.5", "SSP1-RCP2.6", "SSP1-RCP1.9"))) %>%
  mutate(HI_level=factor(HI_level, levels = c("Safe", "Caution", "Extreme Caution", "Danger"))) %>%
  mutate(Time.label = replace(Time.label, Time.label == "Base", "2020"),
         Time.label = replace(Time.label, Time.label == "Mid", "2050"),
         Time.label = replace(Time.label, Time.label == "End", "2100")) %>%
  mutate(Time.label=factor(Time.label, levels = c("2020", "2050", "2100"))) %>%
  mutate(Time.label = as.character(Time.label)) %>%
  arrange(HI_level) %>%
  pivot_longer(cols = c("white_per", "hispanic_per", "black_per", "other_per"), names_to = "var") %>%
  mutate(var = case_when(var == "white_per"~ "White(%)",
                         var == "black_per"~ "Black(%)",
                         var == "hispanic_per"~ "Hispanic(%)",
                         var == "other_per"~ "Others(%)")) %>%
  ggplot(aes(x=SSP, y=value, fill=var, group=Time.label)) +
  geom_bar(position="stack", stat="identity", aes(fill = var)) +
  coord_flip() + 
  facet_grid(HI_level~Time.label) +
  ylab("Racial composition") + 
  xlab("SSP-RCP scenarios") + 
  theme(panel.spacing = unit(0, "lines")) +
  scale_fill_manual(name="Race/Ethnicity", 
                    values = race.color,
                    breaks = c("Black(%)","Hispanic(%)","White(%)","Others(%)")) +
  labs(x="", y="Percentage of total population") +
  theme_ipsum() +
  theme(legend.position="right",
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x =element_text(size=12),
        axis.title.y =element_text(size=12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) #,face="bold")) +

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS5_race_composition_ssp.png", width = 8, height = 7)
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS5_race_composition_ssp.pdf", width = 8, height = 7)

### 3.3.2 age composition ----

SSP_age <- merge %>% 
  replace(is.na(.), 0) %>%
  dplyr::select(Time.label, SSP, GEOID, median.HI.summer,HI_level,lat,
                TotPop,starts_with("Age")) %>%
  mutate(G1 = Age_group_1 + Age_group_2 + Age_group_3 + Age_group_4, # child+teen
         G2 = Age_group_5 + Age_group_6 + Age_group_7 + Age_group_8 +
           Age_group_9 + Age_group_10 + Age_group_11 + Age_group_12 + Age_group_13, # adult
         G3 = Age_group_14 +Age_group_15 +Age_group_16 +
           Age_group_17 + Age_group_18) # old

SSP_age <-SSP_age %>%
  replace(is.na(.), 0) %>%
  group_by(Time.label, SSP, HI_level) %>%
  summarise(Pop_sum=sum(TotPop, na.rm=TRUE),
            G1_sum= sum(G1, na.rm = TRUE),
            G2_sum = sum(G2, na.rm = TRUE),
            G3_sum = sum(G3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(HI_level %in% c("Safe", "Caution", "Extreme Caution", "Danger")) %>%
  group_by(SSP, Time.label, HI_level) %>%
  mutate(n_ssp=sum(Pop_sum),
         G1_per=round(G1_sum/n_ssp*100,2),
         G2_per = round(G2_sum/n_ssp*100,2),
         G3_per = round(G3_sum/n_ssp*100,2))


age.color = c("Young (0-19y/o)" = "tan",
              "Adult (20-64y/o)" =  "slategray2",
              "Elder (65+ y/o)" = "thistle3")

SSP_age %>%
  arrange(Time.label, SSP) %>%  
  mutate(SSP=factor(SSP, levels = c("SSP5-RCP8.5", "SSP3-RCP7.0", "SSP2-RCP4.5",
                                    "SSP1-RCP2.6", "SSP1-RCP1.9"))) %>%
  mutate(HI_level=factor(HI_level, levels = c("Safe", "Caution", "Extreme Caution", "Danger"))) %>%
  mutate(Time.label = replace(Time.label, Time.label == "Base", "2020"),
         Time.label = replace(Time.label, Time.label == "Mid", "2050"),
         Time.label = replace(Time.label, Time.label == "End", "2100")) %>%
  mutate(Time.label=factor(Time.label, levels = c("2020", "2050", "2100"))) %>%
  mutate(Time.label = as.character(Time.label)) %>%
  arrange(HI_level) %>%
  pivot_longer(cols = c("G1_per", "G2_per", "G3_per"), names_to = "var") %>%
  mutate(var = case_when(var == "G1_per" ~ "Young (0-19y/o)",
                         var == "G2_per" ~ "Adult (20-64y/o)",
                         var == "G3_per" ~ "Elder (65+ y/o)")) %>%
  mutate(var = factor(var, levels = c("Young (0-19y/o)", 
                                      "Adult (20-64y/o)", "Elder (65+ y/o)")))%>%
  ggplot(aes(x=SSP, y=value, fill=var, group=Time.label)) +
  geom_bar(position="stack", stat="identity", aes(fill = var)) +
  coord_flip() + 
  facet_grid(HI_level~Time.label) +
  theme(panel.spacing = unit(0, "lines")) +
  scale_fill_manual(name="Age group", 
                    values = age.color,
                    breaks = c("Young (0-19y/o)","Adult (20-64y/o)","Elder (65+ y/o)")) +
  labs(x="", y="Percentage of total population") +
  theme_ipsum() +
  theme(legend.position="right",
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size=12),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size=12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS6_Population_by_age.png", width = 8, height = 7)

