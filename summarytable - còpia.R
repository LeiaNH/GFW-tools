# ------------ #
# Requirements #
# ------------ #

# install tidyverse 
#install.packages("tidyverse")

#load tidyverse
library(tidyverse)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:

key <- readr::read_csv(paste0("C:/Users/lnh88/Dropbox/GitData/GFW-tools/key.csv")) %>%
  pull(key)


# ---------- #
# Study area #
# ---------- #

# Draw your study area here
# https://geojson.io/#map=0.82/24.5/-7.7

# save it as json and copy within next string only the coordinate values
input = '{"geojson":{"type":"Polygon","coordinates":[[[2.551451497146445,1.4528680673808765],[-5.643774654900682,35.87688458359678],[0.9278306800408984,45.454433023296076],[-29.333738292271107,44.969637625092076],[-27.346998262725265,-1.7005498784700137],[2.551451497146445,1.4528680673808765]]]}}'

# -------------- #
# Fishing effort #
# -------------- #

years = c(2015:2022)

for(i in seq_along(years)){

  #y = 1
  y <- years[i]  
  
  # get information
  raw <- get_raster(spatial_resolution = 'low',
           temporal_resolution = 'yearly',
           group_by = 'flagAndGearType',
           date_range = paste0(y, '-01-01',',', y, '-12-31'),
           region = input,
           region_source = 'user_json',
           key = key)
  
  # save data
  write.csv(raw, paste0("C:/Users/lnh88/Dropbox/GitData/GFW-tools/",y,"_allflags.csv"), row.names = F)
  
  # filter european flags
  class(raw)
  
  # summarize total fishing effort per flag and geartype
  summary <- raw %>%
    dplyr::rename('FishingHours' = 'Apparent Fishing hours') %>%
    dplyr::group_by(Flag, Geartype) %>%
    summarize(
      Fishing_Hours = sum(FishingHours, na.rm = T)
    )
  # save data
  write.csv(summary, paste0("C:/Users/lnh88/Dropbox/GitData/GFW-tools/",y,"_allflags.csv"), row.names = F)
  
  }

