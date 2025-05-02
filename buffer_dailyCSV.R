# buffer_dailyCSV.R
# aim: Access data from daily csv to calculate fishing effort and vessels within the surroundings of a animal location

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Upload animal locations
# Step 5: Buffer
# Step 6: mmsi info extraction
# Step 7: fleet info extraction

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

# ----------------------------- #
# Step 4. Load animal locations #
# ----------------------------- #

# Upload file with the locations of radar detections
locs <- utils::read.csv(file = paste0(WD,"GitData/GFW-tools/input/birdLocs.csv")) 

# Brief sight of radar data
tibble::glimpse(locs)

# Parse time variable to a date format
locs <- locs %>% 
  dplyr::mutate(
    time = lubridate::ymd_hms(time))

# Check if time variable have been transform to a date format (POSIXct)
tibble::glimpse(locs$time)

# quick view of our bird radar data
(p +
    geom_point(
      data = locs,
      aes(x = longitude, y = latitude), 
      colour= "red",
      alpha = 0.5))

# Let's work with one radar detection as an example
loc <- locs[1,]
loc # glimpse

# ------------------ #
# Step 5: buffer loc #
# ------------------ #

# Buffer distance
distance <- 30*1000  # in meters

# create buffer
buf <- sf::st_as_sf(loc, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% # radar location point
  sf::st_transform(crs = sf::st_crs('+proj=moll')) %>%  # transform to projected
  sf::st_buffer(dist = distance) %>%  # buffer, distance in m
  sf::st_transform(4326) # backtransform to geographic projection

# Zoom in!
extent_loc <- coord_sf(xlim = c(loc$longitude-1, loc$longitude+1), 
                       ylim = c(loc$latitude-1, loc$latitude+1))

p1 <- p +
    geom_sf(data = buf, color="purple", alpha = 0.5, linetype = 2) +
    geom_point(data = loc,
               aes(x = longitude, y = latitude), 
               colour= "red",
               alpha = 0.5) +
    extent_loc

p1

# let's check vessel activity on that specific day
date <- as.Date(loc$time)
date

# ----------------------- #
# Step 6: mmsi extraction #
# ----------------------- #

mmsi <- read.csv(paste0(WD,"GitData/GFW-tools/input/mmsi-daily-csvs-10-v3-2019/mmsi-daily-csvs-10-v3-", date, ".csv"))

# Brief sight of radar data
glimpse(mmsi)
#date: Date in YYYY-MM-DD format
#cell_ll_lat: The latitude of the lower left (ll) corner of the grid cell, in decimal degrees
#cell_ll_lon: The longitude of the lower left (ll) corner of the grid cell, in decimal degrees
#mmsi: Maritime Mobile Service Identity, the identifier for AIS
#hours: Hours that the MMSI was broadcasting on AIS while present in the grid cell on this day
#fishing_hours: Hours that the MMSI was broadcasting on AIS in this grid cell on this day and detected as fishing by the GFW fishing detection model

# Crop data from our study area
mmsi <- mmsi %>%
  dplyr::filter(cell_ll_lon > -25 & cell_ll_lon < -14 &
                  cell_ll_lat > 13 & cell_ll_lat < 19 )

# Plot fleet available data
(p1 + 
  geom_raster(data = mmsi, aes(x = cell_ll_lon, y = cell_ll_lat), 
              fill = "grey20", colour= "black", alpha = 0.5) +
  extent_loc)

# Create an ID row per cell
mmsi <- mmsi %>% 
  # each cell is defined by longitude and latitude
  group_by(cell_ll_lon, cell_ll_lat) %>%
  dplyr::mutate(
    # that defines an identificator per group_by level
    cellID = cur_group_id()
  ) %>%
  ungroup()

# Select those vars identifying each cell location and id
raster <- mmsi %>% dplyr::select(cell_ll_lon,cell_ll_lat, cellID) 

# Rasterize that info
raster <- raster::rasterFromXYZ(raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Crop raster with polygon
raster_extract <- raster::extract(raster, buf)

# Substract ID cells intersecting the buffer
values <- raster_extract[[1]]
values <- values[!is.na(values)]

# Filter that cells intersecting the buffer
mmsi <- mmsi %>%
  dplyr::mutate(
    intersect = if_else(cellID %in% values, T, F))

# Plot fleet available data inside the buffer
p1 + 
  geom_raster(data= mmsi, aes(x = cell_ll_lon, y = cell_ll_lat, fill= intersect), alpha = 0.5) +
  extent_loc

# Summarize that data
(sz_mmsi <- mmsi %>%
    dplyr::filter(
      intersect == T
    ) %>%
    group_by(mmsi) %>%
    summarize(
      navigating_hours = sum(hours),
      fishing_hours = sum(fishing_hours)
    ))

# mmsi info
sz_mmsi_max_fishing <- sz_mmsi %>%
  dplyr::filter(fishing_hours == max(fishing_hours)) %>%
  pull(mmsi)

mmsi_info <- read.csv(paste0(WD,"GitData/GFW-tools/input/fishing-vessels-v3.csv"))

glimpse(mmsi_info)

#mmsi: Maritime Mobile Service Identity, the identifier for AIS
#year: Year 
#flag_ais: Flag state (ISO3 value) for the vessel as determined by the first three digits (MID) of the MMSI number
#flag_registry: Flag state (ISO3 value) for the vessel as listed on vessel registries (when applicable)
#flag_gfw: Flag state (ISO3 value) assigned to the vessel by GFW after considering all available information
#vessel_class_inferred: Vessel class (gear type) inferred by the GFW vessel classification neural net model
#vessel_class_inferred_score: Neural net score (0-1) for the top scoring vessel class (gear type) inferred by the GFW vessel classification neural net model. Values closer to 1 indicate higher confidence by the neural net
#vessel_class_registry: Vessel class (gear type) for the vessel as listed on vessel registries (if applicable)
#vessel_class_gfw: Vessel class (gear type) assigned to the vessel by GFW after considering all available information
#self_reported_fishing_vessel: Whether the vessel broadcasts the 'Fishing' ship type in > 98% of AIS identity messages
#length_m_inferred: Vessel length (meters) inferred by the GFW vessel classification neural net model
#length_m_registry: Vessel length (meters) for the vessel as listed on vessel registries (if applicable)
#length_m_gfw: Vessel length (meters) assigned to the vessel by GFW after considering all available information
#engine_power_kw_inferred: Engine power (kilowatts) inferred by the GFW vessel classification neural net model
#engine_power_kw_registry: Engine power (kilowatts) for the vessel as listed on vessel registries (if applicable)
#engine_power_kw_gfw: Engine power (kilowatts) assigned to the vessel by GFW after considering all available information
#tonnage_gt_inferred: Tonnage (gross tons) inferred by the GFW vessel classification neural net model
#tonnage_gt_registry: Tonnage (gross tons) for the vessel as listed on vessel registries
#tonnage_gt_gfw: Tonnage (gross tons) assigned to the vessel by GFW after considering all available information
#registries_listed: Registries where the vessel is listed and used to inform the _registry fields (if applicable)
#active_hours: Hours the vessel was broadcasting AIS and moving faster than 0.1 knots
#fishing_hours: Hours the vessel was broadcasting AIS and detected as fishing by the GFW fishing detection neural net model 

(mmsi_info <- mmsi_info %>% dplyr::filter(mmsi == sz_mmsi_max_fishing))

# ------------------------ #
# Step 7: fleet extraction #
# ------------------------ #

fleet <- read.csv(paste0(WD,"GitData/GFW-tools/input/fleet-daily-csvs-100-v3-2019/fleet-daily-csvs-100-v3-", date, ".csv"))

# Brief sight 
glimpse(fleet)

#date: Date in YYYY-MM-DD format. For the fleet-monthly-10-v3 data, the date corresponds to the first date of the month
#cell_ll_lat: The latitude of the lower left (ll) corner of the grid cell, in decimal degrees (WGS84)
#cell_ll_lon: The longitude of the lower left (ll) corner of the grid cell, in decimal degrees (WGS84)
#flag: Flag state (ISO3 value), based on flag_gfw in fishing-vessels-v3.csv
#geartype: Gear type, based on vessel_class_gfw in fishing-vessels-v3.csv 
#hours: Hours that MMSI of this geartype and flag were broadcasting on AIS while present in the grid cell on this day
#fishing_hours: Hours that MMSI of this geartype and flag were broadcasting on AIS in this grid cell on this day and detected as fishing by the GFW fishing detection model
#mmsi_present: Number of MMSI of this flag state and geartype that broadcasted on AIS while present in the grid cell on this day

# Crop data from our study area

fleet <- fleet %>%
  dplyr::filter(cell_ll_lon > -25 & cell_ll_lon < -14 &
                  cell_ll_lat > 13 & cell_ll_lat < 19)

# Plot fleet available data
p1 + 
  geom_raster(data= fleet, aes(x = cell_ll_lon, y = cell_ll_lat), fill = "grey20", alpha = 0.5) +
  extent_loc

# Create an ID row per cell
fleet <- fleet %>% 
  # each cell is defined by longitude and latitude
  group_by(cell_ll_lon, cell_ll_lat) %>%
  dplyr::mutate(
    # that defines an identificator per group_by level
    cellID = cur_group_id()
  ) %>%
  ungroup()

# Select those vars identifying each cell location and id
raster <- fleet %>% dplyr::select(cell_ll_lon,cell_ll_lat, cellID) 

# Rasterize that info
raster <- raster::rasterFromXYZ(raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Crop raster with polygon
raster_extract <- raster::extract(raster, buf)

# Substract ID cells intersecting the buffer
values <- raster_extract[[1]]
values <- values[!is.na(values)]

# Filter that cells intersecting the buffer
fleet <- fleet %>%
  dplyr::mutate(
    intersect = if_else(cellID %in% values, T, F))

# Plot fleet available data inside the buffer
p1 + 
  geom_raster(data= fleet, aes(x = cell_ll_lon, y = cell_ll_lat, fill= intersect), alpha = 0.5) +
  extent_loc

# Summarize that data
(sz_fleet <- fleet %>%
    dplyr::filter(
      intersect == T) %>%
    group_by(flag, geartype) %>%
    summarize(
      navigating_hours = sum(hours),
      fishing_hours = sum(fishing_hours)
    ))

