#--------------------------------------------------------------------------------
# radar_SAR.R   
#--------------------------------------------------------------------------------
# Script used to match radar data locations with SAR (Paolo et al., 2024)


# Workflow
# Step 1: setwd
# Step 2: load libraries
# Step 3: Set parameters for search match radar-sar
# Step 4: load radar locs
# Step 5: load SAR data (industrial_vessels_v20240102.csv subset from Paolo et al., 2024)
# Step 6: Match radar and SAR data

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
library(tibble)
library(rnaturalearth)
library(sf)

#---------------------------------------------------------
# 3. Set parameters for search match radar-sar
#---------------------------------------------------------

# Buffer area
spatial_extent <- 0.01 
temporal_window <- 120  # 2 hours (time before and after the detection, in minutes)

#---------------------------------------------------------------
# 4. Load radar locs
#---------------------------------------------------------------

# Upload the location where the radar detector, deployed on the bird, identified the presence of a ship. This location does not correspond to any fishing vessel detected in the GFW datasets, suggesting the vessel could be non-fishing or potentially engaged in IUU (Illegal, Unreported, and Unregulated) activities

# This bird data point is entirely made up for this exercise
radar <- tibble(
  latitude = 14.551,
  longitude = -17.319,
  time = "9/7/2021 19:10"
)

glimpse(radar)

# Correct time format
radar$time = lubridate::dmy_hm(radar$time)

glimpse(radar)

# plot radar points
#par(bg = "lightblue")
plot(radar$longitude, radar$latitude, 
     xlab = "Longitude", ylab = "Latitude", 
     xlim = c(radar$longitude - 0.02, radar$longitude + 0.02),
     ylim = c(radar$latitude  - 0.02, radar$latitude  + 0.02),
     pch = 19, col = "blue")
#text(radar$longitude+ 0.002, radar$latitude , labels = "bird", col = "black")


# World polygons from rnaturalearthdata
world <- ne_countries(scale = "medium", returnclass = "sf")

# Cape Verde shearwater foraging area
extent <- coord_sf(xlim = c(-26, -11), ylim = c(13, 22))

# This is just to remove warnings from ggplot
options(warn=-1)

# quick view of our study area
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
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
      # remove axes title
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    #radar loc
    geom_point(
      data = radar,
      aes(x = longitude, y = latitude), 
      colour= "red",
      alpha = 0.5)

# Zoom in!
extent_loc <- coord_sf(xlim = c(radar$longitude-spatial_extent, radar$longitude+spatial_extent), 
                       ylim = c(radar$latitude-spatial_extent, radar$latitude+spatial_extent))

(p <- 
    # open ggplot
    ggplot() +
    # plot and color world land mask  
    geom_sf(data = world, fill= "antiquewhite") +
    geom_sf_label(data = world,aes(label = name))+
    # limit your study area
    extent_loc +
    # theme 
    theme(
      # color water
      panel.background = element_rect(fill="aliceblue"),
      # plot grid lines
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
      # remove axes title
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    #radar loc
    geom_point(
      data = radar,
      aes(x = longitude, y = latitude), 
      colour= "red",
      alpha = 0.5))

#---------------------------------------------------------------
# 5. Load SAR data
#---------------------------------------------------------------

# Here's a subset of SAR dataset. Complete dataset can be downloaded from  GFW website
SAR <- readr::read_csv(paste0(WD, "/input/SAR/industrial_vessels_v20240102_subset2021.csv"))
glimpse(SAR)
# view(SAR)

#---------------------------------------------------------------
# 6. Match radar and SAR data
#---------------------------------------------------------------

# -------------------------------------------------------------------
# bounding box from radar detection to search for a satellite image
# -------------------------------------------------------------------
  
bbox <- raster::extent(
    radar$longitude - spatial_extent,  # xmin
    radar$longitude + spatial_extent,  # xmax
    radar$latitude  - spatial_extent,  # ymin
    radar$latitude  + spatial_extent   # ymax
  )

# plot bbox
#plot(as(bbox, "SpatialPolygons"), 
 #    add = TRUE, border = "black", lwd = 3)

# Convert to sf polygon
bbox_sf <- sf::st_as_sfc(as(bbox, "SpatialPolygons"))
bbox_sf <- sf::st_set_crs(bbox_sf, 4326)  # assuming WGS84

# Add to your ggplot
(p <- p + 
  geom_sf(data = bbox_sf, fill = NA, color = "black", linewidth = 3) +
  # limit your study area
  extent_loc)
x11();p
# -------------------------------------------------------------------
# Filter SAR data overlapping spatio-temporally with the radar event
# -------------------------------------------------------------------
  
# TEMPORAL MATCH 
match <- SAR %>% 
    dplyr::filter(
      timestamp > (radar$time - lubridate::minutes(temporal_window)) &
        timestamp < (radar$time + lubridate::minutes(temporal_window)))
  
# SPATIAL MATCH
match <- match %>%
      filter(lon >= bbox@xmin & lon <= bbox@xmax & lat >= bbox@ymin & lat <= bbox@ymax) %>%
  dplyr::rename(longitude = lon, latitude = lat)

# plot ship
#points(match$longitude, match$latitude, 
 #    pch = 19, col = "red", cex= 3, add= TRUE)    
#text(match$longitude+0.002, match$latitude, labels = "ship", col = "black")

p + 
    #radar loc
    geom_point(
      data = match,
      aes(x = longitude, y = latitude), 
      colour= "blue",
      alpha = 0.5,
      size = 3) +
    # limit your study area
    extent_loc 

# Take a look
match
# It is not a fishing vessel



