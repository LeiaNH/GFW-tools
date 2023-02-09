# fishing_effort_area.R
# aim: Access data from Global Fishing Watch APIs to calculate fishing effort within a customized area

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Get the fishing effort summaries within the area
# Step 5. Save info

# ------------------- #
# Step 1. Set your WD #
# ------------------- #

WD <- "D:/Dropbox/" #minipc
#WD <- "C:/Users/lnh88/Dropbox/" #laptop

# -------------------- #
# Step 2. Requirements #
# -------------------- #

# install tidyverse, qdapRegex and readxl to basic data manipulation
#install.packages("tidyverse")
#install.packages("qdapRegex")
#install.packages(readxl)

# load tidyverse, qdapRegex and readxl libraries
library(tidyverse)
library(qdapRegex)
library(readxl)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 


# ------------------ #
# Step 3. Study area #
# ------------------ #

# One of the options that rGFW package provides is to get summaries of fishing effort of an specific area such as EEZ or MPA. Nevertheless there is an option to include your customed area in json format. For that, a quick way to do it is to draw your own study area as a polygon in the next link: 
# https://geojson.io/#map=0.82/24.5/-7.7

# After that, save that polygon as json format. Now you will have a text with the info of your polygon
(area <- read_lines(paste0(WD, "GitData/GFW-tools/input/studyarea.geojson")))
# You need to copy only the coordinates
(coords <- qdapRegex::rm_between(area, "[[[", "]]]", extract=TRUE))
# And paste those coordinates in the proper space for that in next string (just replace the numbers with your own coordinates)
area = '{"geojson":{"type":"Polygon","coordinates": [[[2.551451497146445,1.4528680673808765],[-5.643774654900682,35.87688458359678],[0.9278306800408984,45.454433023296076],[-29.333738292271107,44.969637625092076],[-27.346998262725265,-1.7005498784700137],[2.551451497146445,1.4528680673808765]]]}}'

# ------------------------------ #
# Step 4. Fishing effort summary #
# ------------------------------ #

# In this specific case we are only interested in EU flagged vessels. So we upload a list of those flags
# Country list: https://european-union.europa.eu/principles-countries-history/country-profiles_en
# ISO codes: https://www.iso.org/obp/ui/#search
ISO_EU_Countries <- readxl::read_excel("D:/Dropbox/GitData/GFW-tools/input/ISO_EU_Countries.xlsx")

# vector of that countries
EU <- ISO_EU_Countries %>% pull(ISOcode)

# set the years that you want to process
years = c(2015:2022)

# empty list to store the summaries
l <- list()

# let's loop it across years
for(i in seq_along(years)){

  #i = 1
  y <- years[i]  
  
  # get information: GFW base function to get raster from API and convert response to data frame
  raw <- get_raster(spatial_resolution = 'low', # Can be "low" = 0.1 degree or "high" = 0.01 degree
           temporal_resolution = 'yearly', # Can be 'daily','monthly','yearly'
           group_by = 'flagAndGearType', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
           date_range = paste0(y, '-01-01',',', y, '-12-31'),
           region = input, # geojson or GFW region code, shape to filter raster
           region_source = 'user_json', #source of the region ('eez','mpa', 'trfmo' or 'user_json')
           key = key) #Authorization token. Can be obtained with gfw_auth function
  
  # save data if you need to plot it 
  # write.csv(raw, paste0("C:/Users/lnh88/Dropbox/GitData/GFW-tools/",y,"_allflags.csv"), row.names = F)
  
  # filter european flags
  raw <- raw %>%
    dplyr::filter(Flag %in% EU)
  
  # summarize total fishing effort per flag and geartype
  summary <- raw %>%
    dplyr::rename('FishingHours' = 'Apparent Fishing hours',
                  'Year' = 'Time Range') %>%
    dplyr::group_by(Year, Flag, Geartype) %>%
    summarize(
      Fishing_Hours = sum(FishingHours, na.rm = T)
    )
  # save data per year
  # write.csv(summary, paste0("C:/Users/lnh88/Dropbox/GitData/GFW-tools/",y,"_allflags.csv"), row.names = F)
  
  l[i] <- list(summary)
}

# unlist data stored 
output <- do.call(rbind, l)

# ----------------- #
# Step 5. Save data #
# ----------------- #

write.csv(output, paste0(WD,"GitData/GFW-tools/output/fishingeffort_geartypeEU.csv"), row.names = F)

