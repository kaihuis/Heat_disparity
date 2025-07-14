# ------------------------------------------------------------------------------
# Program Name: 3_merge_socioecon.R
# Date Last Modified: June, 2023
# Program Purpose: Merging assembled Earth System Model data with projected 
#                  socioeconomic data
# Input Files: HI_data_main_result.csv
#              Socioeconomic data (population/gender/age/race/gdp)
# Output Files: merged.csv 
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
}

# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 
`%!in%` <- compose(`!`, `%in%`)

# 1. Read data -----


dir <- 'data/socioecon/SEDAC_popdynamics-us-county-xlsx/SEDAC_georeferenced_county_population_proj'
setwd( dir )


## 1.1 Pop data -----

pop_data <- read_excel("hauer_county_totpop_SSPs.xlsx")
pop_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, tot_pop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> pop
pop %>%
  dplyr::select(GEOID = GEOID10, Year = year, SSP = ssp, TotPop = tot_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> pop1
pop %>%
  dplyr::select(GEOID = GEOID10, Year = year,SSP = ssp, TotPop = tot_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> pop2
pop_data2 = rbind(pop1, pop2) %>% mutate(Year = as.numeric(Year))


## 1.2 Gender data ----

gender_data <- read_excel("hauer_county_female_pop_SSPs.xlsx")
gender_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, fpop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> gender
gender %>%
  dplyr::select(GEOID = GEOID10, Year =year, SSP = ssp, FePop = fpop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> gender1
gender %>%
  dplyr::select(GEOID = GEOID10, Year = year,SSP = ssp, FePop = fpop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> gender2
gender_data2 = rbind(gender1, gender2) %>% mutate(Year = as.numeric(Year))

## 1.3 Age data ----

county <- st_read("data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
  filter(STATEFP %!in% c("02", "15","60","66","69", "72","77")) 
county %>%
  rename(GEOID10 = "GEOID") -> county

file.list <- list.files(paste0(dir,"/age"))
age_data <- data.frame()

for(i in 1:length(file.list))
{
  print(i)
  print(file.list[i])
  age <- read_excel(paste0(dir, "/age/", file.list[i]))[,c(1:5,19:106)] %>%
    filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
    gather(sspyear, value, 6:90) %>%
    separate(sspyear, into = c("SSP", "Year"), sep = 4, remove = FALSE) %>%
    filter(Year %in% seq(2020, 2100, by = 10)) %>%
    filter(SSP != "ssp4") %>%
    dplyr::select(GEOID = GEOID10, Age_group = age, SSP, Year, Pop = value)
  age_data = rbind(age_data, age)
}
#write.csv(age_data,"county_age_pop_ssp.csv", row.names = FALSE)

age_data %>%
  dplyr::select(GEOID, Year, SSP, Age_group, Age_pop = Pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) ->p1
age_data %>%
  dplyr::select(GEOID, Year, SSP, Age_group, Age_pop = Pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") ->p2
age_data2 = rbind(p1, p2) %>%
  filter(Age_group >0)
age_data3 <- as.data.frame(age_data2) %>%
  mutate(temp = "Age_group") %>%
  unite(Age_group, c("temp", "Age_group")) %>%
  spread(key = Age_group, value = Age_pop) %>%
  mutate(Year= as.numeric(Year))


## 1.4 Race/enthinicty data ----

### white ----
white_data <- read_excel("hauer_county_whiteNH_pop_SSPs.xlsx")
white_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, white_pop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> white
white %>%
  dplyr::select(GEOID = GEOID10, Year =year, SSP = ssp, WhitePop = white_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> white1

white %>%
  dplyr::select(GEOID = GEOID10, Year =year,SSP = ssp, WhitePop = white_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> white2
white_data2 = rbind(white1,white2) %>% mutate(Year = as.numeric(Year))

### black ----
black_data <- read_excel("hauer_county_blackNH_pop_SSPs.xlsx")
black_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, black_pop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> black
black %>%
  dplyr::select(GEOID = GEOID10, Year =year, SSP = ssp, BlackPop = black_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> black1
black %>%
  dplyr::select(GEOID = GEOID10, Year =year,SSP = ssp, BlackPop = black_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> black2
black_data2 = rbind(black1, black2) %>% mutate(Year = as.numeric(Year))

### hispanic ----
hispanic_data <- read_excel("hauer_county_hispanic_pop_SSPs.xlsx")
hispanic_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, hispanic_pop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> hispanic
hispanic %>%
  dplyr::select(GEOID = GEOID10, Year =year, SSP = ssp, HispanicPop = hispanic_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> hispanic1
hispanic %>%
  dplyr::select(GEOID = GEOID10, Year =year,SSP = ssp, HispanicPop = hispanic_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> hispanic2
hispanic_data2 = rbind(hispanic1, hispanic2) %>% mutate(Year = as.numeric(Year))

### other race ----
other_race_data <- read_excel("hauer_county_other_race_pop_SSPs.xlsx")
other_race_data %>%
  filter(STATEFP10 %!in% c("02","15","60","66","69", "72","77")) %>%
  gather(ssp, other_race_pop, 19:103) %>%
  separate(ssp, 
           into = c("ssp", "year"), 
           sep= seq(4,8,by=4)) %>%
  filter(year %in% seq(2020, 2100, by = 10)) %>%
  dplyr::select(-c("LSAD10", "ALAND10","AWATER10","CLASSFP10", "MTFCC10","CSAFP10", "CBSAFP10", 
                   "METDIVFP10", "FUNCSTAT10", "geoid")) -> others
others %>%
  dplyr::select(GEOID = GEOID10, Year =year, SSP = ssp, OtherRacePop = other_race_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> others1
others %>%
  dplyr::select(GEOID = GEOID10, Year =year,SSP = ssp, OtherRacePop = other_race_pop) %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> others2
other_data2 = rbind(others1, others2) %>% mutate(Year = as.numeric(Year))

## 1.5 gdp ----

# read raster
for(ssp in c("SSP1", "SSP2", "SSP3", "SSP5"))
{
  ssp = "SSP1"
  path = paste0("data/socioecon/GDP(SSP1-5)_grid/result(GDP)_",ssp)
  setwd(path)
  print(ssp)
  county <- st_read("data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
    filter(STATEFP %!in% c("02", "15","60","66","69", "72","77")) 
  
  for(year in c(seq(2020, 2100, by = 10)))
  {
    str_name <- paste0("gdp", year, ".tif") 
    tif_gdp_temp <- raster(str_name)
    
    gdp_county <- crop(tif_gdp_temp, extent(county))
    gdp_county <- mask(gdp_county, mask = county)
    crs(gdp_county) <- "+proj=longlat +datum=NAD83 +no_defs" 

    plot(gdp_county)
    plot(st_geometry(county),add = TRUE)
    
    gdpcell <- cellnumbers(gdp_county,county)
    
    gdpcty <- gdpcell %>% 
      mutate(gdpsum = raster::extract(gdp_county, gdpcell$cell_)) %>%
      group_by(object_) %>%
      summarise(gdpsum = sum(gdpsum, na.rm = TRUE)) %>%
      tidyr::complete(object_ = min(object_):max(object_)) %>%
      mutate(gdpsum = replace(gdpsum, is.na(gdpsum), 0))
    
    county$gdpsum <- gdpcty$gdpsum 
    
    county %>%
      ggplot()+
      geom_sf(aes(fill = log(gdpsum)), color = "grey50", size = 0.1)+
      scale_fill_gradient2(breaks= c(seq(18, 27, by = 2)), limits = c(15, 30),
                           low="tan4", mid = "lemonchiffon", high="seagreen4",
                           midpoint = 22)+
      theme_bw() +
      labs(title = paste0("GDP projection in the continental US under ", ssp, " for ", year),
           fill='GDP (log)') 

    fig.name <- paste0("gdp_",ssp,"_",year,".pdf")
    #ggsave(fig.name, width = 10, height = 8)
    
    colnames(county)[colnames(county) == 'gdpsum'] <- paste0("gdp",year)
  }
  
  county_ssp <- county %>% mutate(SSP = tolower(ssp)) 
  name = paste0("county_GDP_", tolower(ssp), ".csv")
  #st_write(county_ssp, name, layer_options = "GEOMETRY=AS_XY")
  #write.csv(county_ssp,"county_GDP_ssp1.csv", row.names = TRUE)
}

setwd("data/socioecon/GDP(SSP1-5)_grid/GDP")

gdp_data <- data.frame()

for(ssp in c("ssp1", "ssp2", "ssp3", "ssp5"))
{
read_csv(paste0("county_GDP_", ssp, ".csv")) %>%
  gather(temp, GDP,10:18) %>%
  separate(temp, into = c("gdp", "Year"), sep= seq(3,7,by=4)) %>%
  dplyr::select(c("GEOID", "Year", "GDP")) %>%
  mutate(SSP = ssp) -> gdp_data1
  gdp_data = rbind(gdp_data, gdp_data1) %>%
    mutate(Year = as.numeric(Year))
}

gdp_data %>% 
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp119"),
         SSP = replace(SSP, SSP == "ssp2", "ssp245"),
         SSP = replace(SSP, SSP == "ssp3", "ssp370"),
         SSP = replace(SSP, SSP == "ssp5", "ssp585")) %>%
  filter(SSP %!in% c("ssp4")) -> gdp1
gdp_data %>%
  mutate(SSP = replace(SSP, SSP == "ssp1", "ssp126")) %>%
  filter(SSP == "ssp126") -> gdp2
gdp_data2 = rbind(gdp1, gdp2) 

# compile socioeconomic data -----
socioecon <- pop_data2 %>%
  left_join(gender_data2, by = c("Year", "GEOID", "SSP")) %>%
  left_join(white_data2, by = c("Year", "GEOID", "SSP")) %>%
  left_join(black_data2, by = c("Year", "GEOID", "SSP")) %>%
  left_join(hispanic_data2, by = c("Year", "GEOID", "SSP")) %>%
  left_join(other_data2, by = c("Year", "GEOID", "SSP")) %>%
  left_join(age_data3, by = c("Year", "GEOID", "SSP")) %>%
  left_join(gdp_data2, by = c("Year", "GEOID", "SSP")) %>%
  mutate(SSP = case_when(SSP == "ssp119" ~ "SSP1-RCP1.9",
                         SSP == "ssp126" ~ "SSP1-RCP2.6",
                         SSP == "ssp245" ~ "SSP2-RCP4.5",
                         SSP == "ssp370" ~ "SSP3-RCP7.0",
                         SSP == "ssp585" ~ "SSP5-RCP8.5"))
#write.csv(socioecon,"socioecon.csv", row.names = FALSE)

# 2. Heat data ----
HI_data_main_result <- read_csv("HI_data_main_result.csv") 

# 3. merge -----

merged <- HI_data_main_result %>%
  left_join(socioecon, by = c("Year", "GEOID", "SSP")) 

#write.csv(merged,"data/merged.csv", row.names = FALSE)

