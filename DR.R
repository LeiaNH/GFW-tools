# ------------------------------------------------------------------------------

# Title:


# ------------------- #
# Step 1. Set your WD #
# ------------------- #

WD <- "D:/Dropbox/" #minipc
#WD <- "C:/Users/lnh88/Dropbox/" #laptop

# -------------------- #
# Step 2. Requirements #
# -------------------- #

# install tidyverse, qdapRegex and readxl for data manipulation and visualization
#install.packages("tidyverse")
#install.packages("qdapRegex")
#install.packages(readxl)
#install.packages(rnaturalearth)

# load tidyverse, qdapRegex and readxl libraries
library(tidyverse)
library(qdapRegex)
library(readxl)
library(rnaturalearth)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 


# 1. Set study area and period--------------------------------------------------

# Option 1:
# One of the options that rGFW package provides is to get summaries of fishing effort of an specific area such as EEZ or MPA. 
# Nevertheless there is an option to include your customed area in json format. 
# For that, a quick way to do it is to draw your own study area as a polygon in the next link: 
# https://geojson.io/#map=0.82/24.5/-7.7
# Set the areas (the name of the geojson files) that you want to process
# geojson_file <- paste0("input/gfwr/map.geojson")
# Read the geojson file as an sf object
# area <- st_read(geojson_file)

#Option 2:
# The list of available COUNTRIES:
# eez_regions <- get_regions(region_source = 'EEZ', key = key)
# The list of available RFMOs:
# rfmo_regions <- get_regions(region_source = 'RFMO', key = key)
# The list of available RFMOs:
# mpa_regions <- get_regions(region_source = 'MPA', key = key)

# select your own:
#area <- get_region_id(region_name = 'GFCM', region_source = 'RFMO', key = key)

# 1.2. Set study years:
years <- c(2014:2022)

#2. Download fishing effort summary data-----------------------------------------

# empty list to store the raw data if you wish:
# lraw <- list()
# empty list to store the summaries
lsz <- list()

# let's loop it across years
for(i in seq_along(years)){
  #i = 1
  y <- years[i]  
  print(y)
  start_date <- as.character(paste0(y, '-01-01'))
  end_date <- as.character(paste0(y, '-12-31'))
  
  # get information: GFW base function to get raster from API and convert response to data frame
  raw <- get_raster(spatial_resolution = 'HIGH', # Can be "low" = 0.1 degree or "high" = 0.01 degree
                    temporal_resolution = 'YEARLY', # Can be 'daily','monthly','yearly'
                    group_by = 'GEARTYPE', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
                    start_date = start_date, # DR UPDATE
                    end_date = end_date, # DR UPDATE
                    region = 'GFCM', # geojson or GFW region code (i.e. option 2) or sf object (i.e. option 1
                    region_source = 'RFMO', #source of the region ('EEZ','MPA', 'RFMO' or 'USER_JSON')
                    key = key) #Authorization token. Can be obtained with gfw_auth function
  
  # save data per year if you wish:
  dir_output <- paste0("D:/Dropbox/GitData/GFW-tools/output/DR")
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
  file_name <- paste0(dir_output, "/",y,".csv")
  write.csv(raw, file_name, row.names = F)
  
  # if you want to save raw data:
  #lraw[i] <- list(raw)

  Sys.sleep(5)  # Pause for 5 seconds between requests to avoid rate limits
}

# unlist data stored 
#raw data:
#outputraw <- do.call(rbind, lraw)

#filtered data:
outputsz <- do.call(rbind, lsz)

#3. Save fishing effort summary data--------------------------------------------

# Create directory for raw data (across years, all gears) if you wish:

#dir_output <- paste0(input_data, "/gfwr/summarydata")
#if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
#file_name <- paste0(dir_output, "/RawAllGearTypes.csv")
write.csv(outputraw, filename, row.names = F)

# Create directory for filtered data (across years, bottom trawling):

dir_output <- paste0(input_data, "/gfwr/summarydata")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
file_name <- paste0(dir_output, "/summaryTrawlEffort.csv")
write.csv(outputsz, filename, row.names = F)

#3. Quick checking map-----------------------------------------------------------
# First read the file with gridded data already saved
fishingeffort <- read.csv("input/gfwr/summarydata/summaryTrawlEffort.csv")

# Make zoomed in map 
#Mask
mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# Bounding box if you rather setting it to the raster limits:
#extent <- coord_sf(xlim = c(min(fishingeffort$Lon), max(fishingeffort$Lon)), 
#                   ylim = c(min(fishingeffort$Lat), max(fishingeffort$Lat)))

# Create a ggplot object
p <- ggplot() +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_tile(data = fishingeffort, aes(x = Lon, y = Lat, fill = ApparentFishingHours))+
  scale_fill_viridis_c(trans="log10")+ 
  #Set spatial bounds
  coord_sf(xlim = c(-6, 40), ylim = c(30, 46), expand = TRUE) + #change by extent if you wish to fit it to data
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  # Remove grids
  theme(panel.grid = element_blank())

plot(p)

# export plot
outdir <- paste0(output_data, "/fig/Map/FishingEffort")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/FishingEffort.png")
ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)