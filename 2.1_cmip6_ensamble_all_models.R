# ------------------------------------------------------------------------------
# Program Name: 2_cmip6_ensemble_all_models.R
# Date Last Modified: June, 2025
# Program Purpose: Getting data from all models from CMIP6 for studied scenarios 
# Input Files: CMIP6 tas and hurs
# Output Files: HI_all_models_ssp119.csv
#               HI_all_models_ssp126.csv
#               HI_all_models_ssp245.csv
#               HI_all_models_ssp370.csv
#               HI_all_models_ssp585.csv
# Author: Kaihui Song
# Contact: kaihuis@berkeley.edu
# Affiliation: Energy and Resources Group, University of California, Berkeley
# ------------------------------------------------------------------------------

# 0. Global setting ----

# library
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
}

# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 
`%!in%` <- compose(`!`, `%in%`)

# 1. Read spatial data ----

# county center point data 

us_county_boundaries <- read.csv("data/us-county-boundaries.csv", sep = ";")
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

# county polygon data

GEOID.file <- read_csv("GEOID.csv")
us_counties <- map_data("county") %>%
  filter(region %!in% c("alaska", "hawaii")) %>%
  left_join(GEOID.file, by = c("region", "subregion")) %>%
  mutate(GEOID =  sprintf("%05s", GEOID))

us_counties %>% ggplot(aes(x = long, y = lat, group = group))+geom_polygon()

# county shp data

county1 <- st_read("data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp") %>%
  filter(STATEFP %!in% c("02", "15","60","66","69", "72","77")) # contiguous US

# 2. CMIP6 ensemble (tas, hurs, HI calculation) -----

climate.scenarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")

var = c("tas", "hurs")

unlink(tempdir(), recursive = TRUE)

for(ssp_index in climate.scenarios)
  {
    print(ssp_index)
    
  ##### tas
  
    var = "tas"
    
    # set directory
    dir <- 'data/variant'
    my.file.path <- paste0(dir, "/", ssp_index,"/", var)
    setwd(my.file.path)
  
    # read IAM models                                                      
    model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0", "CESM2", "CESM2-WACCM",
             "CIESM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "CanESM5-CanOE",
             "E3SM-1-0", "E3SM-1-1","E3SM-1-1-ECA", 
             "EC-Earth3", "EC-Earth3-AerChem", "EC-Earth3-CC", "FGOALS-f3-L",
             "FGOALS-g3", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", 
             "GISS-E2-1-G-CC", "GISS-E2-1-H", "GISS-E2-2-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", 
            "INM-CM4-8", "INM-CM5-0", "IPSL-CM5A2-INCA", "KACE-1-0-G", "KIOST-ESM", "MCM-UA-1-0", "NESM3",
            "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MPI-ESM-1-2-HAM",
            "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL", "UKESM1-1-LL")
  
    year <- c(2015, 2016, 2017, 2018, 2019, 2020, 
              2030, 2040, 
              2045, 2046, 2047, 2048, 2049, 2050, 
              2060, 2070, 2080, 2090, 
              2095, 2096, 2097, 2098, 2099, 2100)

    variant_fix <- c("r1i1p1f1")
    
    idx <- init_cmip6_index(
        activity = "ScenarioMIP", # only consider ScenarioMIP activity
        variable = var,   # specify dry-bulb temperature and relative humidity
        frequency = "mon",    # specify report frequent
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
    time.series.tas <-  data.frame(stringsAsFactors=FALSE)

    for(j in 1:max(idx_subset$n_group))
    {
        print(paste0("n_group = ", j))
        idx_subset_temp <- idx_subset %>% filter(n_group==j)
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
                                         filter(Year %in% c(seq(2015, 2020, by = 1), 2030, 2040, 
                                                            seq(2045, 2050, by = 1), 
                                                            2060, 2070, 2080, 2090,
                                                            seq(2095, 2100, by = 1))) # | (model == "CAMS-CSM1-0" & Year = 2099))
                      time.series.tas = rbind(time.series.tas, points_data)
                    }
           }
    }
    time.series.tas %>%
      gather(ID, Value, 4:3111) %>%
      left_join(county, by = "ID") %>%
      mutate(GEOID =  sprintf("%05s", GEOID)) %>%
      rename(tas= Value) ->l.tas
    
    
  ##### hurs
    
    var = "hurs"
    
    # set directory
  
    dir <- 'data/variant'
    my.file.path <- paste0(dir, "/", ssp_index,"/", var)
    setwd(my.file.path)
    
    # read IAM models                                                      
    model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0", "CESM2", "CESM2-WACCM",
               "CIESM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "CanESM5-CanOE",
               "E3SM-1-0", "E3SM-1-1", "E3SM-1-1-ECA", 
               "EC-Earth3", "EC-Earth3-AerChem", "EC-Earth3-CC", "FGOALS-f3-L",
               "FGOALS-g3", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", 
               "GISS-E2-1-G-CC", "GISS-E2-1-H", "GISS-E2-2-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", 
               "INM-CM4-8", "INM-CM5-0", "IPSL-CM5A2-INCA", "KACE-1-0-G", "KIOST-ESM", "MCM-UA-1-0", "NESM3",
               "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MPI-ESM-1-2-HAM",
               "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL", "UKESM1-1-LL")
    
    year <- c(2015, 2016, 2017, 2018, 2019, 2020, 
              2030, 2040, 
              2045, 2046, 2047, 2048, 2049, 2050, 
              2060, 2070, 2080, 2090, 
              2095, 2096, 2097, 2098, 2099, 2100)
    
    variant_fix <- c("r1i1p1f1")
    
    idx <- init_cmip6_index(
      activity = "ScenarioMIP", # only consider ScenarioMIP activity
      variable = var,   # specify dry-bulb temperature and relative humidity
      frequency = "mon",    # specify report frequent
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
    time.series.hurs <-  data.frame(stringsAsFactors=FALSE)
    
    for(j in 1:max(idx_subset$n_group))
    {
        print(paste0("n_group = ", j))
        idx_subset_temp <- idx_subset %>% filter(n_group==j)
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
                    spread(ID, value) %>%   # Can be skipped if you want a "long" table
                    mutate(model = url_name$model[1],
                           member_id = url_name$member_id[1],
                           date = ymd(str_sub(names(brick.temp),2)),
                           SSP = ssp_index) %>% 
                  as_tibble() %>%
                  separate(date, into = c('Year',"Mon","Day"), sep = "-") %>%
                  filter(Year %in% c(seq(2015, 2020, by = 1), 2030, 2040, 
                               seq(2045, 2050, by = 1), 
                               2060, 2070, 2080, 2090,
                               seq(2095, 2100, by = 1))) 
                time.series.hurs = rbind(time.series.hurs, points_data)
             }
         }
    }
    
    time.series.hurs %>%
      gather(ID, Value, 4:3111) %>%
      left_join(county, by = "ID") %>%
      mutate(GEOID =  sprintf("%05s", GEOID)) %>%
      rename(hurs = Value) %>%  
      dplyr::select(-Day, -ID, -member_id) %>%
      mutate(hurs = replace(hurs,
                            hurs>100,
                            100)) ->l.hurs
    
    p.data.final <-  data.frame(stringsAsFactors=FALSE)
  
    for(i in year)
      {
        print(i)
        l.tas %>% filter(Year == i) -> l.tas.temp
        l.hurs %>% filter(Year == i) %>% filter(GEOID != "000NA") -> l.hurs.temp
        l.tas.temp %>% 
            inner_join(l.hurs.temp, by = c("Year", "Mon", "model", "SSP", "GEOID", "STATE_NAME", "NAME", "lat", "lon")) %>%
            mutate(tas.F = kelvin.to.fahrenheit(tas, round = 3),
             HeatIndex = heat.index(t = tas.F, 
                                    rh = hurs, 
                                    temperature.metric = "fahrenheit", output.metric = NULL, round = 3))-> p.temp
    
        p.data.final = rbind(p.data.final, p.temp)
        }
    
    name = paste0("HI_all_models_", ssp_index, ".csv")
    #write.csv(p.data.final,"HI_all_models_ssp245.csv", row.names = FALSE) # v4 for sensitivity analysis
}

# 3. Data merge -----
# (note: this data is very large, data merge is optional; each scenario can be processed seperately)


setwd("data/all_models/")

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

HI_all_models_ssp119 %>%
    rbind(HI_all_models_ssp126) %>%
    rbind(HI_all_models_ssp245) %>%
    rbind(HI_all_models_ssp370) %>%
    rbind(HI_all_models_ssp585) -> HI_all_models_5scenarios

write.csv(HI_all_models_5scenarios,"data/HI_all_models_5scenarios.csv", row.names = FALSE)
 