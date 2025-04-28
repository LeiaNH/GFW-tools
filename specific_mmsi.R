# specific_mmsi.R
# aim: Access data from Global Fishing Watch APIs to calculate fishing effort from a specific mmsi within a customized area

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Get the fishing effort summaries within the area
# Step 5. Save info
# Step 6. Quick map

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
library(geojsonsf)
library(sf)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 

# vessel
mmsiCode <- "225987981"

# ------------------ #
# Step 3. Study area #
# ------------------ #

(area <- read_sf(paste0(WD, "GitData/REDUCE_study_area/REDUCE_NEW_study_area.shp")))
plot(area)

# ------------------------------ #
# Step 4. Fishing effort summary #
# ------------------------------ #

#07/05 al 14/07/2018
#04/05 al 17/06/2019

# get information: GFW base function to get raster from API and convert response to data frame
raw <- get_raster(
      spatial_resolution = 'LOW', # Can be "low" = 0.1 degree or "high" = 0.01 degree
      temporal_resolution = 'YEARLY', # Can be 'daily','monthly','yearly'
      group_by = 'VESSEL_ID', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
      start_date = paste0('2019-05-04'),
      end_date = paste0('2019-06-17'),
      region = area, # geojson or GFW region code, shape to filter raster
      region_source = 'USER_SHAPEFILE', #source of the region ('eez','mpa', 'trfmo' or 'user_json')
      key = key) #Authorization token. Can be obtained with gfw_auth function
    
# specific vessel 
raw <- raw %>%
      dplyr::filter(MMSI == mmsiCode)
    
# summarize total fishing effort per flag and geartype
summary <- raw %>%
      dplyr::rename('FishingHours' = 'Apparent Fishing Hours') %>%
      dplyr::group_by(MMSI) %>%
      summarize(
        Fishing_Hours = sum(FishingHours, na.rm = T)
      )

# ----------------- #
# Step 5. Save data #
# ----------------- #
  
write.csv(raw, paste0(WD,"GitData/GFW-tools/output/mareas_specificMMSI/marea2019_raw.csv"), row.names = F)

write.csv(summary, paste0(WD,"GitData/GFW-tools/output/mareas_specificMMSI/marea2019_sz.csv"), row.names = F)
  
# ----------------- #
# Step 6. Quick map #
# ----------------- #

# World polygons from rnaturalearthdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

inset <- ggplot() +
  # plot and color world land mask  
  geom_sf(data = world, fill= "antiquewhite") +
  # geom_sf_label(data = world,aes(label = name))+
  # Add the areas from GeoJSON
  geom_sf(data = area, color = "red", fill = "red", alpha = 0.5, size = 0.8) +  
  theme_bw()

png(paste0(WD,"GitData/GFW-tools/output/mareas_specificMMSI/inset.png"), 
    width=1500, height=2000, res=150)
print(inset)
dev.off()

# fishing effort
fishingeffort <- raw %>%
  rename('FishingHours'='Apparent Fishing Hours')  %>%
  group_by(Lat, Lon) %>%
  summarize(
    Fishing_Hours = sum(FishingHours, na.rm = T)
  )
  
# Bounding box
extent <- coord_sf(xlim = c(min(fishingeffort$Lon)-1, max(fishingeffort$Lon)+1), 
                   ylim = c(min(fishingeffort$Lat)-1, max(fishingeffort$Lat)+1))

# plot it

(p <- 
    # open ggplot
    ggplot() +
    # plot and color world land mask  
    geom_sf(data = world, fill= "antiquewhite") +
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
    geom_tile(data = fishingeffort, aes(x = Lon, y = Lat, fill = Fishing_Hours))+
    scale_fill_viridis_c(trans="log10")+
    # Change legend name
    labs(fill = "Fishing effort (hours)") +
    # limit your study area
    extent)

png(paste0(WD,"GitData/GFW-tools/output/mareas_specificMMSI/marea2019.png"), 
    width=900, height=1800, res=150)
print(p)
dev.off()

