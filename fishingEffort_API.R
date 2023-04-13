# fishingEffort_API.R
# aim: Access data from Global Fishing Watch APIs to calculate fishing effort within a customized area

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Get the fishing effort summaries within the area
# Step 5. Save info
# Step 6. Quick map

# ------------------- #
# Step 1. Set your WD #
# ------------------- #

#WD <- "D:/Dropbox/" #minipc
WD <- "C:/Users/lnh88/Dropbox/" #laptop

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


# ------------------ #
# Step 3. Study area #
# ------------------ #

# One of the options that rGFW package provides is to get summaries of fishing effort of an specific area such as EEZ or MPA. Nevertheless there is an option to include your customed area in json format. For that, a quick way to do it is to draw your own study area as a polygon in the next link: 
# https://geojson.io/#map=0.82/24.5/-7.7

# set the areas (the name of the geojson files) that you want to process
areas <- c("fao34","fao27partial")

# let's keep just one area
areas <- c("fao34")

# set the years that you want to process
years <- c(2015:2022)

# let's keep just one year
years <- 2015

for (a in seq_along(areas)) {
  
  #a=1
  
  areaname = areas[a]

  # After that, save that polygon as json format. Now you will have a text with the info of your polygon
  (area <- read_lines(paste0(WD, "GitData/GFW-tools/input/",areaname,".geojson")))
  # Select the coordinates
  (coords <- qdapRegex::rm_between(area, "[[[", "]]]", extract=TRUE))
  # And paste those coordinates in a geojson string 
  area <- '{"geojson":{"type":"Polygon","coordinates": [[[yourcoords]]]}}'
  area <- gsub("yourcoords", coords[[1]],  area)
  
  # ------------------------------ #
  # Step 4. Fishing effort summary #
  # ------------------------------ #

  # In this specific case we are only interested in EU flagged vessels. So we upload a list of those flags
  # Country list: https://european-union.europa.eu/principles-countries-history/country-profiles_en
  # ISO codes: https://www.iso.org/obp/ui/#search
  ISO_EU_Countries <- readxl::read_excel(paste0(WD, "GitData/GFW-tools/input/ISO_EU_Countries.xlsx"))
  
  # however explore some R packages that already parse that iso codes

  # vector of that countries
  EU <- ISO_EU_Countries %>% pull(ISOcode)
  
  # empty list to store the raw data
  lraw <- list()
  # empty list to store the summaries
  lsz <- list()
  
  # let's loop it across years
  for(i in seq_along(years)){
    #i = 1
    y <- years[i]  
    
    # get information: GFW base function to get raster from API and convert response to data frame
    raw <- get_raster(spatial_resolution = 'low', # Can be "low" = 0.1 degree or "high" = 0.01 degree
           temporal_resolution = 'yearly', # Can be 'daily','monthly','yearly'
           group_by = 'flagAndGearType', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
           date_range = paste0(y, '-01-01',',', y, '-12-31'),
           region = area, # geojson or GFW region code, shape to filter raster
           region_source = 'user_json', #source of the region ('eez','mpa', 'trfmo' or 'user_json')
           key = key) #Authorization token. Can be obtained with gfw_auth function
    
    lraw[i] <- list(raw)
  
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
  
  lsz[i] <- list(summary)
}

# unlist data stored 
outputraw <- do.call(rbind, lraw)
outputsz <- do.call(rbind, lsz)

# ----------------- #
# Step 5. Save data #
# ----------------- #

write.csv(outputraw, paste0(WD,"GitData/GFW-tools/output/", areaname, "raw_fishingeffort.csv"), row.names = F)

write.csv(outputsz, paste0(WD,"GitData/GFW-tools/output/", areaname, "fishingeffort_geartypeEU.csv"), row.names = F)

}

# ----------------- #
# Step 6. Quick map #
# ----------------- #

# Let's see that we want to plot Lithuanian fleet fishing effort within FAO37

# First read the file with gridded data already saved
fishingeffort <- read.csv(paste0(WD, "GitData/GFW-tools/output/fao34raw_fishingeffort.csv")) %>%
  # filter the fleet that you are interested in
  dplyr::filter(Flag == "LTU") %>%
  rename('FishingHours'='Apparent.Fishing.hours',
         'Year'='Time.Range')

# If we want to plot year by year we need to parse the var to factor
fishingeffort$Year = as.factor(fishingeffort$Year)

# World polygons from rnaturalearthdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Bounding box
extent <- coord_sf(xlim = c(min(fishingeffort$Lon), max(fishingeffort$Lon)), 
                   ylim = c(min(fishingeffort$Lat), max(fishingeffort$Lat)))

# plot it

(p <- 
    # open ggplot
    ggplot() +
    # plot and color world land mask  
    geom_sf(data = world, fill= "antiquewhite") +
    # geom_sf_label(data = world,aes(label = name))+
    # theme 
    theme(
      # color water
      panel.background = element_rect(fill="white"),
      # plot grid lines
      #panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
      # remove axes title
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position="bottom")+
    
    #plot navigation hours data
    geom_tile(data = fishingeffort, aes(x = Lon, y = Lat, fill = FishingHours))+
    scale_fill_viridis_c(trans="log10")+
  # limit your study area
  extent +
  # draw a panel per year
  facet_wrap(~Year))
