# ------------------------------------------------------------------------------
# Program Name: 5_daily_data_processing_final_KS.R
# Date Last Modified: Jan, 2023
# Program Purpose: model ensemble 
# Author: Kaihui Song
# Contact: kaihuis@berkeley.edu
# Affiliation: University of California, Berkeley
# ------------------------------------------------------------------------------

# 0. Global setting -----

# library
rm(list=ls())
{
  library(tidyverse)
  library(openxlsx)
  library(ncdf4)
  library(raster)
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
}

# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 
`%!in%` <- compose(`!`, `%in%`)


# 1. Read spatial data -----

## county center point data ----

us_county_boundaries <- read.csv("~/Downloads/us-county-boundaries.csv", sep = ";")
us_county_boundaries %>% 
  dplyr::select(GEOID, STATE_NAME, NAME, Geo.Point) %>%
  separate(Geo.Point, into = c('lat', 'lon'), sep=",") %>%
  filter(STATE_NAME %!in% c("U.S. Virgin Islands", "Alaska","American Samoa",
                            "Northern Mariana Islands","U.S. Virgin Islands", 
                            "Hawaii","Puerto Rico", "Guam")) %>%
  mutate(ID = as.character(row_number())) -> county

lon <- as.numeric(county$lon)
lat <- as.numeric(county$lat)
points.df <- data.frame(lon = lon, lat = lat)
points <- SpatialPoints(as.matrix(points.df)) 

## county polygon data ----

GEOID.file <- read_csv("/Users/songkaihui/Documents/CMIP6/GEOID.csv")
us_counties <- map_data("county") %>%
  filter(region %!in% c("alaska", "hawaii")) %>%
  left_join(GEOID.file, by = c("region", "subregion")) %>%
  mutate(GEOID =  sprintf("%05s", GEOID))

us_counties %>% ggplot(aes(x = long, y = lat, group = group))+geom_polygon()

## county shp data ---

county_boundary <- st_read("/Users/songkaihui/Documents/CMIP6/data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
  filter(STATEFP %!in% c("02", "15","60","66","69", "72","77"))

# 2. IAM ensemble (tas, hurs, HI calculation)  -----

climate.scenarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
ssp_index = "ssp245"
var = c("tasmax", "hursmin")
var = c("tasmax")

for(ssp_index in climate.scenarios)
{
  print(ssp_index)
  
  ##### tas
  
  var = "tasmax"
  
  # set directory
  dir <- '/Users/songkaihui/Documents/CMIP6/data/variant/daily'
  my.file.path <- paste0(dir, "/", ssp_index,"/", var)
  setwd(my.file.path)
  
  # read Earth Systems models                                                      
  model <- c("EC-Earth3")
  
  year <- c(2016, 2017, 2018, 2019, 2020, 
            2046, 2047, 2048, 2049, 2050, 
            2096, 2097, 2098, 2099, 2100)
  
  variant_fix <- c("r1i1p1f1")
  idx <- init_cmip6_index(
    activity = "ScenarioMIP", # only consider ScenarioMIP activity
    variable = var,   # specify dry-bulb temperature and relative humidity
    frequency = "day",    # specify report frequent
    experiment = ssp_index,   # specify experiment name
    source = model,   # specify GCM name
    variant = variant_fix,    # specify variant,
    resolution = c("100 km"),
    years = year,    # specify years of interest
    save = TRUE    # save to data dictionary
  )
  
  # read files
  
  idx_subset <- idx %>% 
    group_by(experiment_id, source_id, institution_id, member_id) %>%
    mutate(n_group = cur_group_id()) %>%
    ungroup()
  
  url_name <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "url"))))
  for(i in 1:length(idx_subset$file_url))
  {
    url_name[i,] <- data.frame(ID =i,
                               url = idx_subset$file_url[i])
  }
  
  #sm <- summary_database(my.file.path, by = c("source", "variable"), mult = "latest", update = TRUE)
  #str(sm)
  
  # get spatial info for all counties
  
  ssp = ssp_index
  time.series.tasmax <-  data.frame(stringsAsFactors=FALSE)
  for(j in 1:max(idx_subset$n_group))
  {
    print(paste0("n_group = ", j))
    idx_subset_temp <- idx_subset %>% filter(n_group==j) %>%
      filter(((datetime_start > "2015-01-01")&(datetime_start < "2020-01-01") |
               (datetime_start > "2045-01-01")&(datetime_start < "2050-01-01") |
               (datetime_start > "2095-01-01")&(datetime_start < "2100-01-01") ))
  
    url_name <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("ID", "url", "model", "member_id"))))
    
    for(i in 1:length(idx_subset_temp$file_url))
    {
      print(i)
      url_name[i,] <- data.frame(ID =i,
                                 url = idx_subset_temp$file_url[i],
                                 model = idx_subset_temp$source_id[i],
                                 member_id = idx_subset_temp$member_id[i]) 
      
      name <- split_path(url_name[i,2])[1]
      if(file.exists(paste0(my.file.path, "/", name)))
      {
        brick.temp <- brick(paste0(my.file.path, "/", name))
        brick.temp = rotate(brick.temp)
        points_data <- brick.temp %>% 
          raster::extract(points, df = T) %>%
          gather(date, value, -ID) %>%
          spread(ID, value) %>%  # Can be skipped if you want a "long" table
          mutate(model = url_name$model[1],
                 member_id = url_name$member_id[1],
                 date = ymd(str_sub(names(brick.temp),2)),
                 SSP = ssp_index) %>% 
          as_tibble() %>%
          separate(date, into = c('Year',"Mon","Day"), sep = "-") %>%
          filter(Year %in% c(seq(2016, 2020, by = 1), 
                             seq(2046, 2050, by = 1), 
                             seq(2096, 2100, by = 1))) %>%
          filter(Mon %in% c("06", "07", "08"))# | (model == "CAMS-CSM1-0" & Year = 2099))
        time.series.tasmax = rbind(time.series.tasmax, points_data)
      }
    }
  }
  time.series.tasmax %>%
    gather(ID, Value, 4:3111) %>%
    left_join(county, by = "ID") %>%
    mutate(GEOID =  sprintf("%05s", GEOID)) %>%
    rename(tasmax= Value) ->l.tasmax
  
  ##### hurs
  
  var = "hursmin"
  
  # set directory
  
  dir <- '/Users/songkaihui/Documents/CMIP6/data/variant/daily'
  my.file.path <- paste0(dir, "/", ssp_index,"/", var)
  setwd(my.file.path)
  
  # read IAM models                                                      
  model <- c("EC-Earth3")
  
  year <- c(2016, 2017, 2018, 2019, 2020, 
            2046, 2047, 2048, 2049, 2050, 
            2096, 2097, 2098, 2099, 2100)
  
  variant_fix <- c("r1i1p1f1")
  
  idx <- init_cmip6_index(
    activity = "ScenarioMIP", # only consider ScenarioMIP activity
    variable = var,   # specify dry-bulb temperature and relative humidity
    frequency = "day",    # specify report frequent
    experiment = ssp_index,   # specify experiment name
    source = model,   # specify GCM name
    variant = variant_fix,    # specify variant,
    resolution = c("100 km"),
    years = year,    # specify years of interest
    save = TRUE    # save to data dictionary
  )
  
  # read files
  
  idx_subset <- idx %>% 
    group_by(experiment_id, source_id, institution_id, member_id) %>%
    mutate(n_group = cur_group_id()) %>%
    ungroup()
  
  url_name <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "url"))))
  for(i in 1:length(idx_subset$file_url))
  {
    url_name[i,] <- data.frame(ID =i,
                               url = idx_subset$file_url[i])
  }
  
  # get spatial info for all counties
  
  ssp = ssp_index
  time.series.hursmin<-  data.frame(stringsAsFactors=FALSE)
  for(j in 1:max(idx_subset$n_group))
  {
    print(paste0("n_group = ", j))
    idx_subset_temp <- idx_subset %>% filter(n_group==j) %>%
      filter(((datetime_start > "2015-01-01")&(datetime_start < "2020-01-01") |
                (datetime_start > "2045-01-01")&(datetime_start < "2050-01-01") |
                (datetime_start > "2095-01-01")&(datetime_start < "2100-01-01") ))
    
    url_name <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("ID", "url", "model", "member_id"))))
    
    for(i in 1:length(idx_subset_temp$file_url))
    {
      print(i)
      url_name[i,] <- data.frame(ID =i,
                                 url = idx_subset_temp$file_url[i],
                                 model = idx_subset_temp$source_id[i],
                                 member_id = idx_subset_temp$member_id[i]) 
      
      name <- split_path(url_name[i,2])[1]
      if(file.exists(paste0(my.file.path, "/", name)))
      {
        brick.temp <- brick(paste0(my.file.path, "/", name))
        brick.temp = rotate(brick.temp)
        points_data <- brick.temp %>% 
          raster::extract(points, df = T) %>%
          gather(date, value, -ID) %>%
          spread(ID, value) %>%  # Can be skipped if you want a "long" table
          mutate(model = url_name$model[1],
                 member_id = url_name$member_id[1],
                 date = ymd(str_sub(names(brick.temp),2)),
                 SSP = ssp_index) %>% 
          as_tibble() %>%
          separate(date, into = c('Year',"Mon","Day"), sep = "-") %>%
          filter(Year %in% c(seq(2016, 2020, by = 1), 
                             seq(2046, 2050, by = 1), 
                             seq(2096, 2100, by = 1))) %>%
          filter(Mon %in% c("06", "07", "08"))# | (model == "CAMS-CSM1-0" & Year = 2099))
        time.series.hursmin = rbind(time.series.hursmin, points_data)
      }
    }
  }
  
  time.series.hursmin %>%
    gather(ID, Value, 4:3111) %>%
    left_join(county, by = "ID") %>%
    mutate(GEOID =  sprintf("%05s", GEOID)) %>%
    rename(hursmin = Value) %>%  
    mutate(hursmin = replace(hursmin,
                          hursmin>100,
                          100)) ->l.hursmin
  
  p.data.final <-  data.frame(stringsAsFactors=FALSE)
  
  for(i in 2096:2100)
  {
    print(i)
    l.tasmax %>% filter(Year == i) -> l.tasmax.temp
    l.hursmin %>% filter(Year == i) %>% filter(GEOID != "000NA") -> l.hursmin.temp
    l.tasmax.temp %>% 
      inner_join(l.hursmin.temp, by = c("Year", "Mon", "Day", "model", "SSP", "GEOID", "STATE_NAME", "NAME", "lat", "lon")) %>%
      mutate(tasmax.F = kelvin.to.fahrenheit(tasmax, round = 3),
             HeatIndex_max = heat.index(t = tasmax.F, 
                                    rh = hursmin, 
                                    temperature.metric = "fahrenheit", output.metric = NULL, round = 3))-> p.temp
    p.temp %>% 
      dplyr::select(Year, Mon, Day, model, SSP, tasmax.F, hursmin,HeatIndex_max, GEOID, STATE_NAME, NAME,lat,lon) -> p.temp.data
    
    p.data.final = rbind(p.data.final, p.temp.data)
  }
  #write.csv(p.data.final, "~/Documents/CMIP6/data/variant/daily/results/daily_2096_2100.csv", row.names = FALSE)
  
  name = paste0("ensemble_HI_", ssp_index, ".csv")
  #write.csv(p.data.final,"ensemble_HI_ssp585_v3.csv", row.names = FALSE)
}

# 3. Data merge -----
daily_2016_2020 <- read_csv("~/Documents/CMIP6/data/variant/daily/results/daily_2016_2020.csv")
daily_2046_2050 <- read_csv("~/Documents/CMIP6/data/variant/daily/results/daily_2046_2050.csv")
daily_2096_2100 <- read_csv("~/Documents/CMIP6/data/variant/daily/results/daily_2096_2100.csv")

daily_2016_2020 %>%
  rbind(daily_2046_2050) %>%
  rbind(daily_2096_2100) -> daily_HI
#write.csv(daily_HI, "~/Documents/CMIP6/data/variant/daily/results/daily_HI.csv", row.names = FALSE)

# 4. Map of tas and HI from assembled models -----

daily_HI <- read_csv("~/Documents/CMIP6/data/variant/daily/results/daily_HI.csv")
#merge <- iam_HI_data
mon_names <- list(
  '01'="Jan",
  '02'="Feb",
  '03'="Mar",
  '04'="Apr",
  '05'="May",
  '06'="Jun",
  '07'="Jul",
  '08'="Aug",
  '09'="Sep",
  '10'="Oct",
  '11'="Nov",
  '12'="Dec"
)
mon_labeller <- function(variable,value){
  return(mon_names[value])
}

ssp_names <- list(
  'ssp119'="SSP1-1.9",
  'ssp126'="SSP1-2.6",
  'ssp245'="SSP2-4.5",
  'ssp370'="SSP3-7.0",
  'ssp585'="SSP5-8.5"
)
ssp_labeller <- function(variable,value){
  return(ssp_names[value])
}


# Map 1: each hear risk category -----

daily_HI %>%
  mutate(timeperiod = case_when(Year %in% c(2016:2020) ~ 2020,
                                Year %in% c(2046:2050) ~ 2050,
                                Year %in% c(2096:2100) ~ 2100)) %>%
  group_by(Mon, Day, SSP, GEOID, STATE_NAME, NAME, lat,lon, timeperiod) %>%
  summarise(mean.tasmax.base = mean(tasmax.F),
            mean.hursmin.base = mean(hursmin),
            mean.HImax.base = mean(HeatIndex_max)) %>% 
  ungroup() -> daily_HI_temp

#write.csv(p,"base_year_data.csv", row.names = FALSE)

daily_HI_temp %>%
  mutate(tier1_code = case_when(mean.HImax.base >=80 & mean.HImax.base <=90 ~ 1,
                                TRUE ~ 0),
         tier2_code = case_when(mean.HImax.base >90 & mean.HImax.base <=103 ~ 1,
                                TRUE ~ 0),
         tier3_code = case_when(mean.HImax.base >103 & mean.HImax.base <=124 ~ 1,
                                TRUE ~ 0),
         tier4_code = case_when(mean.HImax.base >125 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(GEOID, timeperiod) %>%
  summarise(number_day1 = sum(tier1_code),
            number_day2 = sum(tier2_code),
            number_day3 = sum(tier3_code),
            number_day4 = sum(tier4_code)) -> daily_HI_temp2

daily_HI_sp <- left_join(county_boundary, daily_HI_temp2, by = "GEOID") 
daily_HI_sp %>%
  ggplot() + 
  geom_sf(aes(fill = number_day1), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n in Caution") +
  #labs(subtitle = "Caution") +
  theme_bw() +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11, hjust = -0.5)) +
  facet_wrap(~timeperiod) -> caution
caution

daily_HI_sp %>%
  ggplot() + 
  geom_sf(aes(fill = number_day2), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n in Extreme Caution") +
  #labs(subtitle = "Extreme Caution") +
  theme_bw()+
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11, hjust = -0.5)) +
  facet_wrap(~timeperiod) -> extreme_caution
extreme_caution

daily_HI_sp %>%
  ggplot() + 
  geom_sf(aes(fill = number_day3), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n in Danger") +
  #labs(subtitle = "Danger") +
  theme_bw()+
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11, hjust = -0.5)) +
  facet_wrap(~timeperiod) -> danger
danger

library(ggpubr) 
figure <- ggarrange(caution, extreme_caution, danger,
                    labels = c("Caution", "Extreme Caution", "Danger"),
                    font.label = list(size = 11, face = "bold", hjust = -0.5),
                    common.legend = T,
                    legend = "bottom",
                    nrow = 3)
figure
ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/daily_figure.png", width = 13, height = 10)

figure

# Map 2: accumulated hear risk category -----


daily_HI_sp %>%
  mutate(total_days_at_risk = number_day1 + number_day2 + number_day3 + number_day4) %>%
  ggplot() + 
  geom_sf(aes(fill = total_days_at_risk), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n above Risk") +
  labs(subtitle = "Caution") + 
  theme_bw()+
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11)) +
  facet_wrap(~timeperiod, ncol = 1) ->caution_plus
caution_plus

daily_HI_sp %>%
  mutate(total_days_at_risk = number_day2 + number_day3 + number_day4) %>%
  ggplot() + 
  geom_sf(aes(fill = total_days_at_risk ), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n above Risk") +
  labs(subtitle = "Extreme Caution") +
  theme_bw()+
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11)) +
  facet_wrap(~timeperiod, ncol = 1) -> extreme_caution_plus
extreme_caution_plus


daily_HI_sp %>%
  mutate(total_days_at_risk = number_day3+number_day4) %>%
  ggplot() + 
  geom_sf(aes(fill = total_days_at_risk ), color = NA) +
  scale_fill_gradient2(breaks= c(seq(0, 92, by = 20)), limits = c(0, 92),
                       low="#2B5C8A", mid = "grey90", high="#9E3D22",
                       midpoint = 46)+
  labs(fill = "Number of days\n above Risk") +
  labs(subtitle = "Danger") +
  theme_bw()+
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.spacing = unit(0.8, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title = element_text(size = 11),
        legend.position = "bottom",
        plot.subtitle = element_text(face = "bold", size = 11)) +
  facet_wrap(~timeperiod, ncol = 1) -> danger_plus
danger_plus

cumulative_risk = ggarrange(caution_plus, extreme_caution_plus, danger_plus,
                            labels = c("A", "B", "C"),
                            font.label = list(size = 11, face = "bold", hjust = 0.5),
                            common.legend = T,
                            legend = "bottom",
                            ncol = 3)
cumulative_risk

ggsave("/Users/songkaihui/Documents/ResearchUNC/Heat/Figures/daily_figure_cumulative_risk.png", width = 13, height = 10)

