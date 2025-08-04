# EEZ_dailyCSV.R
# aim: Access data from monthly csv to calculate fishing effort and vessels within an EEZ

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Load EEZ
# Step 5. Total fleet hours

# ------------------- #
# Step 1. Set your WD #
# ------------------- #

#WD <- "D:/Dropbox/" #minipc
WD <- "C:/Users/lnh88/Dropbox/" #laptop

# -------------------- #
# Step 2. Requirements #
# -------------------- #

# load libraries
library(tidyverse) 
library(lubridate) 
library(readxl)
library(rnaturalearth)
library(maps) 
library(raster) 
library(sf)

# ------------------ #
# Step 3. Study area #
# ------------------ #

# World polygons from rnaturalearthdata
world <- ne_countries(scale = "medium", returnclass = "sf")

# Cape Verde shearwater foraging area
extent <- coord_sf(xlim = c(-26, -11), ylim = c(13, 22))

# This is just to remove warnings from ggplot
options(warn=-1)

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
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
      # remove axes title
      axis.title.x = element_blank(),
      axis.title.y = element_blank()))


# ---------------- #
# Step 4. Load EEZ #
# ---------------- #

#https://www.marineregions.org/gazetteer.php?p=details&id=8369

# Load EEZ polygons: EEZ shapefile from MarineRegions.org
eez <- sf::read_sf(paste0(WD,"GitData/GFW-tools/input/World_EEZ_v10_20180221"), layer = 'eez_v10') %>% 
  filter(
    # select the 200 nautical mile polygon layer
    Pol_type == '200NM',
    # select Mauritania
    MRGID_EEZ == 8369) %>% 
  dplyr::select(ISO_Ter1, MRGID_EEZ, geometry)

# plot eez
p +
  geom_sf(data = eez,
          color = "red",
          fill=NA,
          alpha = 0.8,
          linetype = 2)+
  # limit your study area
  extent

# ------------------------- #
# Step 5. Total fleet hours #
# ------------------------- #

# Create dataframe of filenames dates
effort_files <- tibble(
  file = list.files(paste0(WD, 'GitData/GFW-tools/input/fleet-daily-csvs-100-v3-2024'), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

# Read in data
fleet <- map_dfr(effort_files$file, .f = read_csv) 

# We're interested in making global maps and 0.1 degrees is a much finer resolution than necessary
# Specify new (lower) resolution in degrees for aggregating data.
res <- 0.25

# Transform data across all fleets and geartypes
effort_fleet <- fleet %>% 
  mutate(
    # calculate new lat lon bins with desired resolution
    lat_bin = floor(cell_ll_lat/res) * res + 0.5 * res, 
    lon_bin = floor(cell_ll_lon/res) * res + 0.5 * res)

# Re-aggregate the data to 0.25 degrees
effort_fleet <- effort_fleet %>% 
  group_by(lon_bin, lat_bin, mmsi) %>% 
  summarize(hours = sum(hours, na.rm = T)) 

# Convert effort data frame to sf object
effort_all_sf <- sf::st_as_sf(effort_fleet,
                              coords = c("lon_bin", "lat_bin"),
                              crs = sf::st_crs(eez))

# Filter effort data to within the MRT EEZ
eez_effort <- effort_all_sf %>% 
  sf::st_join(eez, join = sf::st_intersects) %>% # use a spatial join
  filter(MRGID_EEZ == 8369) %>% # filter only datapoints within MRT EEZ
  bind_cols(sf::st_coordinates(.) %>% as.data.frame()) %>%
  rename(lat_bin = Y,
         lon_bin = X) %>%
  sf::st_set_geometry(NULL)

# Now upload vessel info file
fishing_vessels <- utils::read.csv(file=paste0(WD,"GitData/GFW-tools/input/fishing-vessels-v3.csv")) %>%
  dplyr::filter(year == 2024,
                flag_gfw == "FRA") 

# Select french vessels
dataset <- eez_effort %>%
  dplyr::filter(mmsi %in% unique(fishing_vessels$mmsi)) %>%
  dplyr::group_by(mmsi) %>%
  dplyr::summarize(fishing_hours_MauritaniaEEZ = sum(hours, na.rm = TRUE))

# merge info
dataset <- left_join(dataset, fishing_vessels, by="mmsi")
