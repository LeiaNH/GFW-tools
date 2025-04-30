#--------------------------------------------------------------------------------
# radar_AISdisablings.R   
#--------------------------------------------------------------------------------
# Script used to match radar data locations with AIS disablings (Welch et al., 2022)


# Workflow
# Step 1: setwd
# Step 2: load libraries
# Step 3: Locate study area
# Step 4: load radar locs
# Step 5: load AIS-disablings data (Welch et al., 2022)
# Step 6: Match radar and AIS-disabling data
# Step 7: APIs tool

#---------------------------------------------------------------
# 1. Set working directory and Sys.Time
#---------------------------------------------------------------
#place <- "miniPC"
place<- "laptop"
if (place == "miniPC") WD <- "D:/Dropbox/GitData/GFW-tools/"
if (place == "laptop") WD <- "C:/Users/lnh88/Dropbox/GitData/GFW-tools/" 

Sys.setenv(TZ="GMT") ### !!! IMPORTANT: IF NOT SET LIKE THIS, MAKE PROBLEMS TO CREATE DATE_TIME FROM PASTING DATE plus TIME

#---------------------------------------------------------------
# 2. Libraries
#---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(tibble)
library(sf)
library(move)
library(gfwr)

#---------------------------------------------------------
# 3. Study area
#---------------------------------------------------------

# World polygons from rnaturalearthdata
world <- ne_countries(scale = "medium", returnclass = "sf") 

# Define extent limits
lon_min <- -26
lon_max <- -11
lat_min <- 13
lat_max <- 22

# Cape Verde shearwater foraging area
extent <- coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))

# quick view of our study area
(p <- 
    # open ggplot
    ggplot() +
    # plot and color world land mask  
    geom_sf(data = world, fill= "antiquewhite") +
    geom_sf_label(data = world,aes(label = name))+
    # limit your study area
    extent +
    # theme 
    theme(
      # color water
      panel.background = element_rect(fill="aliceblue"),
      # plot grid lines
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5)))

#---------------------------------------------------------------
# 4. Load radar locs
#---------------------------------------------------------------

# Upload the location where the radar detector, deployed on the bird, identified the presence of a ship. This location does not correspond to any fishing vessel detected in the GFW datasets, suggesting the vessel could be non-fishing or potentially engaged in IUU (Illegal, Unreported, and Unregulated) activities

# This bird data point is entirely made up for this exercise
radar <- tibble(
  latitude = 15.120,
  longitude = -17.521,
  time = "30/7/2019 06:01:10"
)

glimpse(radar)

# Correct time format
radar$time = lubridate::dmy_hms(radar$time)

glimpse(radar)

(p <- p +
  # bird-radar location
  geom_point(
    data = radar,
    aes(x = longitude, y = latitude), 
    fill= "red",
    colour = "black",
    alpha = 1,
    shape = 21,
    size = 4))

#---------------------------------------------------------------
# 5. Load AIS-disablings data
#---------------------------------------------------------------

data <- read.csv(paste0(WD, "/input/AIS_Disabling/ais_disabling_events.csv")) %>%
  # parse to date
  dplyr::mutate(gap_start_timestamp = lubridate::ymd_hms(substr(gap_start_timestamp,1,19)),
                gap_end_timestamp = lubridate::ymd_hms(substr(gap_end_timestamp,1,19))) %>%
  # filter gaps below 2 weeks 
  dplyr::filter(gap_hours <= 336) %>%
  # filter those disablings coinciding with radar location
  # temporally
  dplyr::filter(gap_start_timestamp <= radar$time & gap_end_timestamp >= radar$time) %>%
  # where both start and end coordinates fall within the extent
  dplyr::filter(
    dplyr::between(gap_start_lon, lon_min, lon_max),
    dplyr::between(gap_start_lat, lat_min, lat_max),
    dplyr::between(gap_end_lon, lon_min, lon_max),
    dplyr::between(gap_end_lat, lat_min, lat_max)
  )

glimpse(data)

# ---------------------------------------
# interpolate locations within AIS gaps
# ---------------------------------------

# Following Welch et al. (2022), we linearly interpolated vessel positions every five minutes during AIS disabling events. While vessels may not follow straight paths, this provides a first approximation of potential overlap with radar locations.

l <- list()

for(i in 1:nrow(data)){
  
  print(nrow(data)-i)
  #i=1
  
  ss <- data %>% dplyr::slice(i) 
  
  # id gap
  label_vessel <- ss$vessel_class
  label_mmsi <- ss$mmsi
  
  # subset
  ss <- tibble(
    x = c(ss$gap_start_lon, ss$gap_end_lon),
    y = c(ss$gap_start_lat, ss$gap_end_lat),
    time = c(ss$gap_start_timestamp, ss$gap_end_timestamp)
  )
  
  # Convert to a move object
  ssMove <- moveVis::df2move(ss,
                             proj = "+proj=longlat +datum=WGS84", 
                             x = "x", 
                             y = "y", 
                             time = "time")
  
  plot(ssMove)
  
  # interpolate locations providing a time interval
  ssMove <- move::interpolateTime(ssMove, 
                                  time = as.difftime(5, units="mins"),
                                  spaceMethod='greatcircle')
  
  plot(ssMove)
  
  # parse to dataframe
  AISgaplocs <- as.data.frame(ssMove@coords)
  AISgaplocs$time<- ssMove@timestamps
  
  # reshape dataset
  AISgaplocs <- AISgaplocs %>% 
    dplyr::select(coords.x1,coords.x2,time)%>%
    dplyr::rename(lon = coords.x1,
                  lat = coords.x2) 
  
  AISgaplocs$vessel_class <- label_vessel
  AISgaplocs$vessel_mmsi <- label_mmsi
  
  # save it
  l[i] <- list(AISgaplocs)
  }

dataInt <- data.table::rbindlist(l)

# plot those disabling events

dataInt$vessel_mmsi <- as.factor(dataInt$vessel_mmsi)

(p +
    geom_point(
      data = dataInt,
      aes(x = lon, y = lat, colour=vessel_class), 
      alpha = 0.5) +
    facet_wrap(~vessel_mmsi))

# -------------------------------------------
# 6: Match radar and AIS-disabling data
# Calculating distance to the closest vessel
# -------------------------------------------

# Convert dataInt to sf and transform to Mollweide
dataInt_sf <- st_as_sf(dataInt, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = sf::st_crs('+proj=moll'))

# Create and transform radar point
radar_sf <- st_sfc(st_point(c(radar$longitude, radar$latitude)), crs = 4326) %>%
  st_transform(crs = sf::st_crs('+proj=moll'))

# Compute distances in meters, then convert to km
dataInt_sf <- dataInt_sf %>%
  mutate(distance_km = as.numeric(st_distance(geometry, radar_sf)) / 1000)

# Find closest vessel
closest <- dataInt_sf %>%
  arrange(distance_km) %>%
  slice(1)

# Output the closest vessel_mmsi and distance
closest %>%
  dplyr::select(vessel_mmsi, distance_km)

# ------------------- #
# API - Vessel finder #
# ------------------- #

GFW_TOKEN <- readr::read_csv(paste0(WD, "/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 

# Individual vessel information

vessel_info <- gfwr::get_vessel_info(query = 224019630,
                      key = GFW_TOKEN)

vessel_info$selfReportedInfo$shipname[1]
vessel_info$selfReportedInfo$flag[1]

# Other AIS disabling events identified

id <- vessel_info$selfReportedInfo$vesselId
get_event(event_type = "GAP",
          vessels = id[1],
          key = GFW_TOKEN)

# Last activity from that specific vessel in Senegal EEZ

gap_start_timestamp <- data %>%
  dplyr::filter(mmsi == 224019630) %>%
  pull(gap_start_timestamp)

# Fishing activity

get_event(event_type = "FISHING",
          vessels = id[1],
          start_date = lubridate::as_date(gap_start_timestamp) - 30, # 1 month
          end_date = lubridate::as_date(gap_start_timestamp),
          region = 8371,
          region_source = 'EEZ',
          key = GFW_TOKEN)

# AIS disablings

get_event(event_type = "GAP",
          vessels = id[1],
          start_date = lubridate::as_date(gap_start_timestamp) - 30, # 1 month
          end_date = lubridate::as_date(gap_start_timestamp),
          region = 8371, 
          region_source = 'EEZ',
          key = GFW_TOKEN)

get_event(event_type = "GAP",
          vessels = id[1],
          start_date = lubridate::as_date(gap_start_timestamp) - 30, # 1 month
          end_date = lubridate::as_date(gap_start_timestamp),
          key = GFW_TOKEN)

(p <- p +
    # bird-radar location
    geom_point(
      data = radar,
      aes(x = -17.5, y = 16.7), 
      fill= "blue",
      colour = "black",
      alpha = 1,
      shape = 21,
      size = 4))
