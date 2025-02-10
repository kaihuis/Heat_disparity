# ------------------------------------------------------------------------------
# Program Name: cmip6_datadownload_final.R
# Date Last Modified: Jan, 2025
# Program Purpose: Download CMIP6 data
# Author: Kaihui Song
# Contact: kaihuis@berkeley.edu
# Affiliation: Energy and Resources Group, University of California, Berkeley
# ------------------------------------------------------------------------------

################################################################################
## Note: 1. Data being downloaded: 1) average surface temperature (tas)       ##
##                                 2) relative humidity (hurs)                ##
##       2. Data downloaded are decadal data from years 2020 to 2100          ##
##       3. Do not run after data being downloaded                            ##
################################################################################

# 0. Global setting ----

# library
rm(list=ls())
{
library(tidyverse)
library(openxlsx)
library(ncdf4)
library(raster)
library(rgdal)
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
library(geodata)
library(weathermetrics)
}

# functions
split_path <- function(path) {
  rev(setdiff(strsplit(path,"/|\\\\")[[1]], ""))
} 

# directory is set in the loop


# 1. Download near-surface temperature data from CMIP6 (tas)  ----

# Note: tas -- Near-surface (usually, 2 meter) air temperature, units: K)

# set directory to store files

climate.senarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
#climate.senarios <- "ssp585"
for(ssp_index in climate.senarios)
{
    ssp_index = "ssp245"
    dir <- '~/Documents/CMIP6/data/variant'
    my.file.path <- paste0(dir, "/", ssp_index, "/tas")
    setwd(my.file.path)

    model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0", "CESM2", "CESM2-WACCM",
              "CIESM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "CanESM5-CanOE",
               "E3SM-1-0", "E3SM-1-1", "E3SM-1-1-ECA", "EC-Earth3", "EC-Earth3-AerChem", "EC-Earth3-CC", "FGOALS-f3-L",
               "FGOALS-g3", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", 
              "GISS-E2-1-G-CC", "GISS-E2-1-H", "GISS-E2-2-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", 
               "INM-CM4-8", "INM-CM5-0", "IPSL-CM5A2-INCA", "KACE-1-0-G", "KIOST-ESM", "MCM-UA-1-0", "NESM3",
               "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MPI-ESM-1-2-HAM",
               "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL", "UKESM1-1-LL")
  
    year <- c(2015, 2016, 2017, 2018, 2019, 2020,
              2045, 2046, 2047, 2048, 2049, 2050,
              2095, 2096, 2097, 2098, 2099, 2100)

    variant_fix <- c("r1i1p1f1")
    idx <- init_cmip6_index(
        activity = "ScenarioMIP", # only consider ScenarioMIP activity
        variable = c("tas"),   # specify dry-bulb temperature and relative humidity
        frequency = "mon",    # specify report frequent
        experiment = ssp_index,   # specify experiment name
        source = model,   # specify GCM name
        variant = variant_fix,    # specify variant,
        resolution = c("100 km"),
        years = year,    # specify years of interest
        save = TRUE    # save to data dictionary
    )

  # download files

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

    for(i in 1:nrow(url_name)) 
    {
      url = url_name[i,2] 
      name <- split_path(url)[1]
      destfile = paste0(my.file.path,"/", name)
        if(!file.exists(destfile))
        {
          print(url_name[i,])
          GET(url, write_disk(destfile, overwrite=TRUE))
        }
    }

    sm <- summary_database(my.file.path, by = c("source", "variable"), mult = "latest", update = TRUE)
    str(sm)
    knitr::kable(sm)
    output.file.name <- paste0("metadata_",ssp_index,".csv")
    #write.csv(sm, output.file.name, row.names = FALSE)
}

# 2. Download near-surface relative humidity data from CMIP6 (hurs) ---- 

# set directory to store files

#Note: hurs -- near-surface relative humidity, units: %)

climate.senarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")

for(ssp_index in climate.senarios)
{
  ssp_index <- "ssp585"
  dir <- '~/Documents/CMIP6/data/variant'
  my.file.path <- paste0(dir, "/", ssp_index, "/hurs")
  setwd(my.file.path)
  
  model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0", "CESM2", "CESM2-WACCM",
             "CIESM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "CanESM5-CanOE",
             "E3SM-1-0", "E3SM-1-1", "E3SM-1-1-ECA", "EC-Earth3", "EC-Earth3-AerChem", "EC-Earth3-CC", "FGOALS-f3-L",
             "FGOALS-g3", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", 
             "GISS-E2-1-G-CC", "GISS-E2-1-H", "GISS-E2-2-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", 
             "INM-CM4-8", "INM-CM5-0", "IPSL-CM5A2-INCA", "KACE-1-0-G", "KIOST-ESM", "MCM-UA-1-0", "NESM3",
             "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MPI-ESM-1-2-HAM",
             "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL", "UKESM1-1-LL")
  
  year <- c(2015, 2016, 2017, 2018, 2019, 2020,
            2045, 2046, 2047, 2048, 2049, 2050,
            2095, 2096, 2097, 2098, 2099, 2100)
  
  variant_fix <- c("r1i1p1f1")
  idx <- init_cmip6_index(
    activity = "ScenarioMIP", # only consider ScenarioMIP activity
    variable = c("hurs"),   # specify dry-bulb temperature and relative humidity
    frequency = "mon",    # specify report frequent
    experiment = ssp_index,   # specify experiment name
    source = model,   # specify GCM name
    variant = variant_fix,    # specify variant,
    resolution = c("100 km"),
    years = year,    # specify years of interest
    save = TRUE    # save to data dictionary
  )
  sum(idx$file_size)
  
  # download files
  
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
  
  for(i in 1:nrow(url_name)) 
  {
    url = url_name[i,2] 
    name <- split_path(url)[1]
    destfile = paste0(my.file.path,"/", name)
    if(!file.exists(destfile))
    {
      print(url_name[i,])
      GET(url, write_disk(destfile, overwrite=TRUE))
    }
  }
  
  sm <- summary_database(my.file.path, by = c("source", "variable"), mult = "latest", update = TRUE)
  str(sm)
  knitr::kable(sm)
  output.file.name <- paste0("metadata_",ssp_index,".csv")
  #write.csv(sm, output.file.name, row.names = FALSE)
}

