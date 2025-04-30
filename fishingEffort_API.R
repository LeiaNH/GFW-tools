# fishingEffort_API.R
# aim: Access data from Global Fishing Watch APIs to calculate fishing effort within a customized area

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Fishing effort within the area
# Step 5. Quick plot

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
library(sf)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 

# ---------------------------- #
# Step 3. Study area and years #
# ---------------------------- #

(area <- read_sf(paste0(WD, "GitData/REDUCE_study_area/REDUCE_NEW_study_area.shp")))
plot(area)

# set the years that you want to process
#years <- c(2023:2024)

# let's keep just one year
years <- 2024

# ------------------------------ #
# Step 4. Fishing effort summary #
# ------------------------------ #

?get_raster

# empty list to store the raw data
lraw <- list()

# let's loop it across years
for(i in seq_along(years)){
    #i = 1
    y <- years[i]  
    
    # get information: GFW base function to get raster from API and convert response to data frame
    raw <- get_raster(
      spatial_resolution = 'LOW', # Can be "low" = 0.1 degree or "high" = 0.01 degree
      temporal_resolution = 'YEARLY', # Can be 'daily','monthly','yearly'
      group_by = 'FLAGANDGEARTYPE', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
      start_date = paste0(y, '-01-01'),
      end_date = paste0(y, '-12-31'),
      region = area, # geojson or GFW region code, shape to filter raster
      region_source = 'USER_SHAPEFILE', #source of the region ('eez','mpa', 'trfmo' or 'user_json')
      key = key) #Authorization token. Can be obtained with gfw_auth function
    
    lraw[i] <- list(raw)
    
    }

# unlist data stored 
raw <- do.call(rbind, lraw)

# ----------------- #
# Step 6. Quick map #
# ----------------- #

# Let's see that we want to plot Spanish fleet fishing effort within the study area

glimpse(raw)

raw$Geartype = as.factor(raw$Geartype)

# First read the file with gridded data already saved
fishingeffort <- raw %>%
  # filter the fleet that you are interested in
  dplyr::filter(Flag == "ESP") %>%
  group_by(Geartype, Lat, Lon) %>%
  dplyr::summarize(`Fishing effort` = sum(`Apparent Fishing Hours`))

# Summarize total fishing effort by Geartype
fishingeffort_summary <- fishingeffort %>%
  dplyr::group_by(Geartype) %>%
  dplyr::summarize(total_fishing_effort = sum(`Fishing effort`, na.rm = TRUE))

# Get the top 3 gear types with the most fishing effort
top_3_geartypes <- fishingeffort_summary %>%
  dplyr::arrange(desc(total_fishing_effort)) %>%
  dplyr::slice_head(n = 3)

# Filter the fishingeffort data for the top 3 gear types
fishingeffort_top_3 <- fishingeffort %>%
  dplyr::filter(Geartype %in% top_3_geartypes$Geartype)

# View the filtered data
glimpse(fishingeffort_top_3)

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
    geom_tile(data = fishingeffort_top_3, aes(x = Lon, y = Lat, fill = `Fishing effort`))+
    scale_fill_viridis_c(trans="log10")+
  # limit your study area
  extent +
  # draw a panel per year
  facet_wrap(~Geartype))

