# Program Name: 4_disparity_analysis_final_section1_KS.R
# Date Last Modified: June, 2025
# Program Purpose: Disparity in heat exposure,
#                  producing Figures 1&2, Figures S2-S4, Tables S4-S6
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

## 2.1 Fig 1 -----

### 2.1.1 Fig 1 (AB) map increase per decade -----

merge %>%
  dplyr::select(GEOID, NAME, Time.label, SSP, median.HI.summer, median.tas.summer, median.hurs.summer, TotPop, WhitePop, HI_level) %>%
  pivot_wider(names_from = Time.label, 
              values_from = c("TotPop", "WhitePop", "median.HI.summer","median.tas.summer","median.hurs.summer", "HI_level")) %>%
  mutate(increase_end_base_HI_total = median.HI.summer_End - median.HI.summer_Base, 
         increase_end_base_tas_total = median.tas.summer_End - median.tas.summer_Base,
         increase_end_base_hurs_total = median.hurs.summer_End - median.hurs.summer_Base) %>%
  mutate(increase_rate_end_base_HI = increase_end_base_HI_total/8,
         increase_rate_end_base_tas = increase_end_base_tas_total/8,
         increase_rate_end_base_hurs = increase_end_base_hurs_total/8) %>%
  mutate(increase_mid_base_HI_total = median.HI.summer_Mid - median.HI.summer_Base, 
         increase_mid_base_tas_total = median.tas.summer_Mid - median.tas.summer_Base,
         increase_mid_base_hurs_total = median.hurs.summer_Mid - median.hurs.summer_Base) %>%
  mutate(increase_rate_mid_base_HI = increase_mid_base_HI_total/8,
         increase_rate_mid_base_tas = increase_mid_base_tas_total/8,
         increase_rate_mid_base_hurs = increase_mid_base_hurs_total/8) %>%
  mutate(increase_mid_base_tas_c = increase_mid_base_tas_total *5/9,
         increase_mid_base_HI_c = increase_mid_base_HI_total *5/9) -> changes

changes_cty <- left_join(county_boundary, changes, by = "GEOID") 

changes_cty %>%
  ggplot() +
  geom_sf(aes(fill = increase_rate_end_base_tas), color = NA) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13),
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size=13, face = "bold")) +
  facet_wrap(~SSP, ncol = 1)+
  scale_fill_gradient2(name =  "Changes \n(°F/decade)",
                       breaks = seq(0,2, by = 0.5),
                       labels = seq(0,2, by = 0.5),
                       limits = c(-0.2, 1.8),
                       low= "#018571", mid = "cornsilk", high="#a6611a",
                       midpoint = 0.9)+
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = c(seq(-120, -65, by = 20)), limits = c(-128, -65), expand = c(0, 0))+
  labs(title = "Near-surface air temperature") +
  xlab("Longitude") + 
  ylab("Latitude") -> fig1.a1
fig1.a1

changes_cty %>%        
  ggplot() +
  geom_sf(aes(fill = increase_rate_end_base_HI), color = NA) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x =element_text(size=13), 
        axis.title.y =element_text(size=13), 
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size=13, face = "bold")) +
  facet_wrap(~SSP, ncol = 1)+
  scale_fill_gradient2(name =  "Changes \n(°F/decade)",
                       breaks = seq(0,2.5, by = 0.5),
                       labels = label_at(1),
                       limits = c(-0.2, 2.6),
                       low= "#018571", mid = "cornsilk", high="#a6611a",
                       midpoint = 1)+
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = c(seq(-120, -65, by = 20)), limits = c(-128, -65), expand = c(0, 0))+
  labs(title = "Heat Index") +
  xlab("Longitude") + 
  ylab("Latitude") -> fig1.a2
fig1.a2

figure1ab <- ggarrange(fig1.a1, fig1.a2,
                       labels = c("A", "B"),
                       common.legend = TRUE,
                       legend = "bottom",
                       ncol = 2)
figure1ab


### 2.1.2 Fig 1 (CD) temp vs HI -----

# get median values, 1st quartile, and 3rd quartile for each county
fig3c_temp = iam_HI_data %>%
  dplyr::select(Year, Mon, GEOID, County = NAME, SSP, median.HI, median.tas, lat, lon) %>%
  mutate(Year = replace(Year, Year %in% c(2015:2019), 2020),
         Year = replace(Year, Year %in% c(2045:2049), 2050),
         Year = replace(Year, Year %in% c(2095:2099), 2100)) %>%
  filter(Mon %in% c("06", "07", "08")) %>%
  group_by(Year, GEOID, County, SSP, lat, lon) %>%
  summarise(median.tas.new = mean(median.tas), # median tas in summer months
            median.HI.new = mean(median.HI)) %>% # median HI in summer months
  ungroup() %>%
  pivot_longer(cols = c("median.HI.new", "median.tas.new"), names_to = "var", values_to = "median") 

# get 2020 median values for all counties to prepare calculating changes
fig3c_temp  %>% 
  filter(Year == 2020) %>%
  dplyr::rename("median_2020" = "median") %>%
  dplyr::select(-Year)-> fig3c_2020

# merge 2020 median values back
fig3c_temp %>%
  left_join(fig3c_2020, by = c("GEOID","County", "SSP", "var","lat", "lon")) %>% unique() -> fig3c_temp2 

# calculate - new 
fig3c_data <- fig3c_temp2 %>%  
  mutate(diff = median - median_2020) %>% 
  group_by(var, Year, SSP) %>%
  mutate(ymed = median(diff), # median - solid line
         ymax = quantile(diff, probs = 0.75), # upper bound
         ymin = quantile(diff, probs = 0.25))  %>%  # lower bound
  unique() %>%
  mutate(var = replace(var, var == "median.HI.new", "Heat Index"),
         var = replace(var, var == "median.tas.new", "Near-surface air temperature")) 

ggplot() + 
  geom_line(data = fig3c_data, aes(x = Year, y = ymed, group = var, color = var)) +
  geom_ribbon(data = fig3c_data, aes(x = Year, ymin=ymin, ymax=ymax, group = var, fill = var), alpha = 0.5) +
  facet_wrap(~SSP, ncol = 1)+
  theme_bw()+
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13),
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        plot.title = element_text(size=13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13)) +
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = c(2020, 2050, 2100), labels =  c(2020,2050,2100),
                     limits = c(2020, 2100)) +
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  guides(fill=guide_legend(title="Heat Indicators", ncol = 1),
         color =guide_legend(title="Heat Indicators", ncol = 1) ) +
  labs(title="Changes over decades")+
  xlab("Year")+
  ylab(expression(paste('Changes in Heat Indicators (', ~degree, 'F)', sep = ""))) -> fig1.a3

fig1.a3

fig3c_data %>% 
  filter(Year == 2100) %>%
  group_by(lat, var, Year, SSP)%>%
  mutate(ymed_4d = median(diff),
         ymax_4d = quantile(diff, probs = 0.75),
         ymin_4d = quantile(diff, probs = 0.25))  %>%
  ggplot() + 
  geom_point(aes(x = lat, y = ymed_4d, group = var, color = var), alpha = 0.1)+
  geom_smooth(aes(x = lat, y = ymed_4d, group = var, color = var))+
  facet_wrap(~SSP, ncol = 1)+
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text.x =element_text(size=12),
        axis.text.y =element_text(size=12),
        axis.title.x =element_text(size=13), 
        axis.title.y =element_text(size=13),
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size=13, face = "bold")) +
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(25, 50, by=10), labels = seq(25, 50, by=10),
                     limits = c(25, 50)) +
  scale_color_brewer(palette = "Set2")+
  guides(fill=guide_legend(title="Heat Indicators", ncol = 1),
         color =guide_legend(title="Heat Indicators", ncol = 1)) +
  labs(title = "Changes by latitude")+
  xlab("Latitude")+
  ylab(expression(paste('Changes in Heat Indicators (', ~degree, 'F)', sep = ""))) ->fig1.a4
fig1.a4

figure1cd <- ggarrange(fig1.a3, fig1.a4,
                       labels = c("C", "D"),
                       common.legend = TRUE,
                       legend = "bottom",
                       ncol = 2)
figure1cd

Fig1_abcd <- ggarrange(figure1ab, figure1cd,
                         common.legend = FALSE,
                         legend = "bottom",
                         ncol = 2,
                         widths = c(1.35,1))
Fig1_abcd
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/Fig1_abcd.png", width = 13, height = 11)
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/Fig1_abcd.pdf", width = 13, height = 11)


### 2.1.3 Fig S2 absolute values of HI vs tas ----

merge %>%
  dplyr::select(GEOID, NAME, Time.label, SSP, median.HI.summer, median.tas.summer, HI_level) %>%
  mutate(diff = median.HI.summer - median.tas.summer) %>%
  dplyr::select(Time.label, SSP, GEOID, diff) ->merge_figS1

HI_tas_cty <- left_join(county_boundary, merge_figS1, by = "GEOID") 
  
HI_tas_cty %>%
  mutate(Time.label = case_when(Time.label == "Base" ~ "2020",
                                Time.label == "Mid" ~ "2050",
                                Time.label == "End" ~ "2100")) %>%
  ggplot() +
  geom_sf(aes(fill = diff), color = NA) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13),
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size=13, face = "bold"),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()) +
  facet_grid(SSP~Time.label) +
  scale_fill_gradient2(name =  "Difference between Heat Index and near-surface air temperature",
                       breaks = seq(-5,20, by = 5),
                       low= "#018571", mid = "cornsilk", high="#a6611a",
                       midpoint = 0) +
  theme(strip.background = element_blank()) +
  xlab("Longitude") + 
  ylab("Latitude")->fig1.S2
fig1.S2

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/fig1.S2.png", width = 11, height = 13)


### 2.1.4 Fig S3 HI rank changes ----

merge %>%
  dplyr::select(SSP, Time.label, GEOID, TotPop, median.HI.summer)%>%
  group_by(SSP, Time.label) %>%
  mutate(ranks = order(order(median.HI.summer, decreasing = F))) %>%
  pivot_wider(names_from = Time.label, values_from = c("median.HI.summer", "ranks", "TotPop")) %>%
  mutate(rank_change_end = (ranks_End - ranks_Base)/3108*100,
         rank_change_mid = (ranks_Mid - ranks_Base)/3108*100) %>%
  dplyr::select(SSP, GEOID, rank_change_mid, rank_change_end) %>%
  pivot_longer(cols = c(rank_change_mid, rank_change_end), names_to = "var", values_to = "values") %>%
  mutate(var = case_when(var == "rank_change_mid" ~ "2050",
                         TRUE ~ "2100 "))-> rank

data_rank <- left_join(county_boundary, rank, by = "GEOID") 

data_rank %>%
  ggplot() +
  geom_sf(aes(fill = values), color = NA) +
  geom_sf(data = state_boundary, color = "grey80", fill = NA)+
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=12), 
        axis.title.y=element_text(size=12),
        strip.background = element_blank(),
        legend.position="bottom",
        legend.text = element_text(size=11),
        legend.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  facet_wrap(~SSP, ncol = 1)+
  facet_grid(SSP~var) +
  scale_fill_gradientn(colours = c("#2B5783", "white","#A3123A"),
                       breaks = c(-30, -15, 0, 15, 30),
                       name = "Change in percentile rank",
                       limits = c(-30,30))+
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = c(seq(-120, -65, by = 20)), limits = c(-128, -65), expand = c(0, 0))+
  xlab("Longitude") + 
  ylab("Latitude") -> FigS3_rank_change

FigS3_rank_change

#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/FigS3_rank_change.png", width = 7, height = 8)


### 2.1.5 Fig S4 changes in relative humidity ----

figS4 <- left_join(county_boundary, changes, by = "GEOID") 
figS4 %>%
  ggplot() +
  geom_sf(aes(fill =increase_rate_end_base_hurs), color = NA) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13), 
        strip.background = element_rect(fill = rgb(0, 0, 0, 0.2)),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size=13, face = "bold")) +
  facet_wrap(~SSP, ncol = 2)+
  scale_fill_gradient2(name =  "RH increase \n(%/decade)",
                       breaks = seq(-2,2, by = 1),
                       labels = label_at(1),
                       limits = c(-2, 2),
                       low= "#018571", mid = "cornsilk", high="#a6611a",
                       midpoint = 0)+
  theme(strip.background = element_blank()) +
  theme(legend.position = c(0.8,0.15)) +
  guides(fill = guide_colourbar(title.position = "left",
                                barwidth = 2,
                                barheight = 8))+
  scale_x_continuous(breaks = c(seq(-120, -65, by = 20)), limits = c(-128, -65), expand = c(0, 0))+
  xlab("Longitude") + 
  ylab("Latitude") -> fig1.hurs
fig1.hurs
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/Figure_S4.png", width = 8, height = 7)


## 2.2 Fig 2 HI zones ----

### 2.2.1 Fig 2a map ----

fig2_map_data <- left_join(county_boundary, merge, by = "GEOID") 
fig2_map_data %>%
  mutate(Time.label = case_when(Time.label == "Base" ~ "2020",
                                Time.label == "Mid" ~ "2050",
                                Time.label == "End" ~ "2100"))%>%
  mutate(Time.label = factor(Time.label, levels = c("2020", "2050", "2100"))) %>%
  ggplot() +
  geom_sf(aes(fill = HI_level), color = NA) +
  facet_wrap( ~ SSP, ncol = 1,labeller = labeller(Mon = facet.labs))+
  facet_grid(SSP~Time.label)+
  scale_fill_manual(values = HI.color, breaks = c("Safe","Caution","Extreme Caution", "Danger")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x =element_text(size=12),
        axis.text.y =element_text(size=12),
        axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13),
        strip.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="bottom") +
  scale_x_continuous(breaks = c(seq(-120, -60, by = 20)), limits = c(-130, -65), expand = c(0, 0))+
  xlab("Longitude") + 
  ylab("Latitude") +
  guides(fill=guide_legend(title="HI (°F)")) ->fig2.part1
fig2.part1
#ggsave("plots/fig2.part1.png", width = 12, height = 9)


### 2.2.2 Fig 2b bar ----

# by region

merge_with_region <- merge %>% left_join(uscb_region_state, by = c("STATE_NAME"="State"))

merge_with_region %>%
  group_by(Time.label, SSP, HI_level, Region) %>%
  summarise(count = n()) %>%
  mutate(Time.label = replace(Time.label, Time.label == "Base", "2020"),
         Time.label = replace(Time.label, Time.label == "Mid", "2050"),
         Time.label = replace(Time.label, Time.label == "End", "2100")) %>%
  mutate(Time.label = as.character(Time.label)) %>%
  pivot_wider(names_from = "HI_level", values_from = "count") -> tableS5

SSP_zones <- merge_with_region %>% 
  group_by(Time.label, SSP, HI_level, Region) %>%
  summarise(n=sum(TotPop, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(SSP,Region, Time.label) %>%
  mutate(n_ssp=sum(n), per=round(n/n_ssp*100,2))

SSP_zones_bytime <- merge_with_region %>% 
  group_by(Time.label, SSP, HI_level, Region) %>%
  summarise(n=sum(TotPop, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(SSP,Region, Time.label) %>%
  mutate(n_ssp=sum(n), per=round(n/n_ssp*100,2))

SSP_zones_bytime %>%
  filter(Time.label == "End") %>%
  arrange(Time.label, SSP, Region, per) %>%
  mutate(Region = factor(Region, levels = c("South", "West", "Midwest", "Northeast"))) %>%
  mutate(Time.label=factor(Time.label, levels = c("Base", "Mid", "End"))) %>%
  mutate(HI_level=factor(HI_level, levels = c("Safe", "Caution", "Extreme Caution", "Danger"))) %>%
  mutate(SSP=factor(SSP, levels = c("SSP1-RCP1.9","SSP1-RCP2.6", "SSP2-RCP4.5",  "SSP3-RCP7.0", "SSP5-RCP8.5"))) %>%
  tidyr::complete(crossing(HI_level)) %>%
  distinct(SSP, Region, Time.label, HI_level,per) %>%
  mutate(per=case_when(is.na(per) ~0, TRUE ~per)) %>%
  mutate(Time.label = case_when(Time.label == "End" ~ "2100")) %>%
  mutate(Time.label = as.factor(Time.label)) -> fig2.part2_data

fig2.part2_data %>%
  ggplot(aes(x=Region, y=per, group=Region, fill=HI_level)) +
  geom_bar(stat="identity", position="stack", colour="white") +
  coord_flip()+
  facet_grid(SSP~Time.label) +
  theme(panel.spacing = unit(-1, "lines")) +
  scale_fill_manual(name="Heat Index levels", 
                    values = HI.color, 
                    breaks = c("Safe","Caution","Extreme Caution","Danger"))+
  labs(x="", y="Percentage of population") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x =element_text(size=12),
        axis.text.y =element_text(size=12),
        axis.title.x =element_text(size=13),
        axis.title.y =element_text(size=13), 
        strip.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="bottom") ->fig2.part2
fig2.part2
#ggsave("plots/fig2.part2.png", width = 3, height = 9)


figure2 <- ggarrange(fig2.part1, fig2.part2,
                     labels = c("A", "B"),
                     common.legend = T,
                     legend = "bottom",
                     ncol = 2,
                     widths = c(1, 0.35))
figure2
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/figure2_final.png", width = 13.5, height = 9)
#ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/figure2_final.pdf", width = 13.5, height = 9)


### 2.2.3 Table S6 ----

merge %>% 
  dplyr::select(Time.label, SSP, GEOID,NAME, lat, lon, HI_level,TotPop, HispanicPop, BlackPop, WhitePop) %>%
  pivot_wider(names_from = Time.label,
              values_from = c(HI_level, TotPop, HispanicPop, BlackPop, WhitePop)) %>%
  unite(col = change_base_mid, c(HI_level_Base, HI_level_Mid), sep = "_", remove = F) %>%
  unite(col = change_mid_end, c(HI_level_Mid, HI_level_End), sep = "_", remove = F) %>%
  unite(col = change_base_end, c(HI_level_Base, HI_level_End), sep = "_", remove = F)-> tableS6

# base to mid
tableS6 %>% 
  mutate(change_base_mid = replace(change_base_mid, 
                                   change_base_mid %in% c("Safe_Safe", "Caution_Caution", 
                                                          "Extreme Caution_Extreme Caution", "Danger_Danger"), "No Movement")) %>%
  group_by(SSP, change_base_mid) %>%
  summarise(count = n(),
            share = count/3108 * 100) %>%
  mutate(level = "county",
         change_period = "2050",
         share = round(share, digits = 2)) %>%
  ungroup() %>%
  dplyr::rename(change_status = "change_base_mid") -> tableS6_county_2050

tableS6 %>%
  replace(is.na(.), 0) %>%
  group_by(SSP,change_base_mid) %>%
  summarise(pop_base = sum(TotPop_Base),
            pop_mid = sum(TotPop_Mid)) %>%
  mutate(change_base_mid = replace(change_base_mid, 
                                   change_base_mid %in% c("Safe_Safe", "Caution_Caution", 
                                                          "Extreme Caution_Extreme Caution", "Danger_Danger"), "No Movement")) %>%
  group_by(SSP,change_base_mid) %>%
  summarise(pop_base2 = sum(pop_base),
            pop_mid2 = sum(pop_mid)) %>%
  mutate(percent = (pop_mid2 - pop_base2)/pop_base2*100) %>%
  dplyr::select(-percent) %>%
  group_by(SSP) %>%
  mutate(totalpopulation_base = sum(pop_base2),
         totalpopulation_mid = sum(pop_mid2)) %>%
  mutate(share_base = pop_base2/totalpopulation_base *100,
         share_mid = pop_mid2/totalpopulation_mid *100) %>%
  ungroup() %>%
  dplyr::select(SSP, change_status = change_base_mid, count = pop_base2, share = share_base) %>%
  mutate(level = "population",
         change_period = "2050",
         share = round(share, digits = 2))-> tableS6_pop_2050

# base to end
tableS6 %>% 
  mutate(change_base_end = replace(change_base_end, 
                                   change_base_end %in% c("Safe_Safe", "Caution_Caution", 
                                                          "Extreme Caution_Extreme Caution", "Danger_Danger"), "No Movement")) %>%
  group_by(SSP, change_base_end) %>%
  summarise(count = n(),
            share = count/3108 * 100) %>%
  dplyr::rename("change_status" = "change_base_end") %>%
  mutate(level = "county",
         change_period = "2100",
         share = round(share, digits = 2)) -> tableS6_county_2100

tableS6 %>%
  replace(is.na(.), 0) %>%
  group_by(SSP,change_base_end) %>%
  summarise(pop_base = sum(TotPop_Base),
            pop_end = sum(TotPop_End)) %>%
  mutate(change_base_end = replace(change_base_end, 
                                   change_base_end %in% c("Safe_Safe", "Caution_Caution", 
                                                          "Extreme Caution_Extreme Caution", "Danger_Danger"), "No Movement")) %>%
  group_by(SSP,change_base_end) %>%
  summarise(pop_base2 = sum(pop_base),
            pop_end2 = sum(pop_end)) %>%
  mutate(percent = (pop_end2 - pop_base2)/pop_base2) %>%
  dplyr::select(-percent) %>%
  group_by(SSP) %>%
  mutate(totalpopulation_base = sum(pop_base2),
         totalpopulation_end = sum(pop_end2)) %>%
  mutate(share_base = pop_base2/totalpopulation_base *100,
         share_end = pop_end2/totalpopulation_end *100) %>%
  dplyr::select(SSP, change_status = change_base_end, count = pop_base2, share = share_base) %>%
  mutate(level = "population",
         change_period = "2100")-> tableS6_pop_2100


tableS6 = tableS6_county_2050 %>%
  rbind(tableS6_pop_2050) %>%
  rbind(tableS6_county_2100) %>%
  rbind(tableS6_pop_2100)
