# Program Name: 4_disparity_analysis_final_section3_KS.R
# Date Last Modified: June, 2025
# Program Purpose: Disparity in heat exposure. Figure 6. 
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


## 2.1 Fig 6 climate RCP scenario combined with different SSPs ----

merge %>%
  dplyr::select(Time.label, SSP,GEOID, STATE_NAME, median.HI.summer, HI_level) %>%
  mutate(RCP_temp = substrRight(SSP, 3)) %>%
  mutate(RCP = sub("^", "RCP", RCP_temp)) %>%
  dplyr::select(-RCP_temp,-SSP) -> HI.data

merge %>%
  dplyr::select(SSP, Time.label, GEOID, TotPop) %>%
  mutate(SSP = case_when(SSP == "SSP1-RCP1.9" ~ "SSP1",
                         SSP == "SSP1-RCP2.6" ~ "SSP1",
                         SSP == "SSP2-RCP4.5" ~ "SSP2",
                         SSP == "SSP3-RCP7.0" ~ "SSP3",
                         SSP == "SSP5-RCP8.5" ~ "SSP5"))  %>%
  dplyr::select(GEOID, SSP, Time.label, TotPop) %>%
  unique() -> merge_data_temp

# bring in ssp4
totpop_ssp4_raw <- read_excel("socioecon/SEDAC_popdynamics-us-county-xlsx/SEDAC_georeferenced_county_population_proj/hauer_county_totpop_SSPs.xlsx")

totpop_ssp4 <- totpop_ssp4_raw %>%
  dplyr::select(GEOID = GEOID10, ssp42020, ssp42050, ssp42100) %>%
  pivot_longer(cols = starts_with("ssp4"),
               names_to = "var",
               values_to = "value") %>%
  mutate(Time.label = substrRight(var, 4)) %>%
  mutate(SSP = substr(var, 1,4)) %>%
  dplyr::select(-var) %>%
  dplyr::rename(TotPop = value) %>%
  mutate(Time.label = case_when(Time.label == 2020 ~ "Base",
                                Time.label == 2050 ~ "Mid",
                                Time.label == 2100 ~ "End")) %>%
  mutate(SSP = toupper(SSP)) %>%
  dplyr::select(GEOID, SSP, Time.label, TotPop) 

merge_data_temp %>%
  rbind(totpop_ssp4) %>%
  right_join(HI.data, by = c("GEOID", "Time.label")) %>%
  replace(is.na(.), 0) %>%
  group_by(Time.label, SSP, RCP, HI_level) %>%
  unique() %>%
  summarise(sum_totpop = sum(TotPop)) %>%
  ungroup() %>%
  dcast(Time.label + SSP + RCP ~ HI_level, 
        value.var = "sum_totpop") %>%
  pivot_longer(cols = c("Safe", "Caution", "Extreme Caution", "Danger"), 
               names_to = "HI_level",
               values_to = "sum_totpop") %>%
  replace(is.na(.), 0) %>%
  mutate(Time.label=factor(Time.label, levels = c("Base", "Mid", "End")))%>%
  mutate(HI_level=factor(HI_level, levels = c("Safe", "Caution", "Extreme Caution", "Danger")))%>%
  group_by(HI_level, SSP) -> data_temp

data_temp %>%
  ggplot() +
  geom_line(aes(x = Time.label, y = sum_totpop/1000000, color = SSP, group = SSP), alpha = 0.5) +
  geom_point(aes(x = Time.label, y = sum_totpop/1000000, color = SSP, group = SSP)) +
  facet_grid(HI_level~RCP) +
  theme_bw() +
  xlab("Time") +
  ylab("Number of people with color being affected (million)") ->ppl_by_scenario
ppl_by_scenario


data_temp %>% mutate(Time.label = case_when(Time.label == "Base" ~ 2020,
                                            Time.label == "Mid" ~ 2050,
                                            Time.label == "End" ~ 2100)) %>%
  mutate(Time.label = as.numeric(Time.label)) %>%
  mutate(HI_level=factor(HI_level, 
                         levels = c("Safe", "Caution", 
                                    "Extreme Caution", "Danger"))) %>%
  dcast(Time.label + SSP + RCP ~ HI_level, 
        value.var = "sum_totpop") %>%
  pivot_longer(cols = c("Safe", "Caution", "Extreme Caution", "Danger"), 
               names_to = "HI_level",
               values_to = "sum_totpop") %>%
  replace(is.na(.), 0) %>%
  mutate(HI_level=factor(HI_level, 
                         levels = c("Safe", "Caution", 
                                    "Extreme Caution", "Danger"))) -> data1
data1 %>%
  mutate(yvar = sum_totpop/1000000) %>%
  group_by(HI_level, RCP, Time.label) %>%
  mutate(ymaxv = max(yvar),
         yminv = min(yvar)) %>%
  ungroup()  -> data2


ggplot() +
  geom_line(data = data2, aes(x = Time.label, y = yvar, group = SSP), color = "grey60") +
  geom_ribbon(data = data2, aes(x = Time.label, ymin=yminv, ymax=ymaxv, 
                             fill = HI_level), 
              alpha=0.8,       #transparency
              size=1.25) +          #border line size
  geom_line(data = p3, 
            aes(x = Time.label, y = yvar,
                group = SSP),
            color = "black",
            size = 0.5) +
  scale_color_manual(name="Heat Index levels", 
                     values = HI.color, 
                     breaks = c("Safe","Caution","Extreme Caution","Danger")) +
  scale_fill_manual(name="Heat Index levels", 
                    values = HI.color, 
                    breaks = c("Safe","Caution","Extreme Caution","Danger")) +
  scale_x_continuous(breaks = c(2020, 2050, 2100), labels =  c(2020,2050,2100),
                     limits = c(2020, 2100)) +
  facet_grid(HI_level~RCP) +
  theme_bw() +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.5, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11),
        legend.text=element_text(size=11),
        legend.title = element_text(size = 11),
        legend.position = "bottom") +
  theme(panel.spacing.y = unit(1.5, "lines"),
        panel.spacing.x = unit(1.5, "lines"))+
  xlab("Time") +
  ylab("Number of people being affected (million)") ->Fig6_ppl_by_scenario

Fig6_ppl_by_scenario

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/Fig6_ppl_by_scenario.png", width = 9, height = 8)

