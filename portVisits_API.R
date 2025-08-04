# fishingEffort_API.R
# aim: Access data from Global Fishing Watch APIs to calculate fishing effort within a customized area

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Port visits for a single vessel
# Step 5. Quick plot

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
library(rnaturalearth)
library(sf)
library(dplyr)
library(purrr)
library(lubridate)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 

# ------------------- #
# Step 3. Study area  #
# ------------------- #

(area <- read_sf(paste0(WD, "GitData/REDUCE_study_area/REDUCE_NEW_study_area.shp")))
plot(area)

# ------------------------------ #
# Step 4. Port visits            #
# ------------------------------ #

?get_event

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXAMPLE: FISHING VESSEL ON 2024
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Individual vessel information
?get_vessel_info

vessel_info <- gfwr::get_vessel_info(query = 'CIUDAD DE HUELVA',key = key) 

view(vessel_info$selfReportedInfo)

vessel_info$selfReportedInfo

# Port visits

port_Visits <- get_event(event_type = "PORT_VISIT",
                           vessels = vessel_info$selfReportedInfo$vesselId[3],
                           confidences = 4, # only for port visits
#High confidence (level = 4) indicates that the vessel was identified using AIS with an entry, stop or gap, and exit within port. A port visit with a lower confidence may sometimes be a false port visit caused by noisy AIS transmission and requires a further inspection of the vessel tracks.
                           start_date = "2024-01-01", 
                           end_date = "2024-12-31",
                           region = area, 
                           region_source = 'USER_SHAPEFILE',
                           key = key)

port_Visits

# Extract key info from each visit
port_summary <- port_Visits %>%
  mutate(
    port_name = map_chr(event_info, ~ .x$endAnchorage$name),
    port_country = map_chr(event_info, ~ .x$endAnchorage$flag),
    port_lat = map_dbl(event_info, ~ .x$endAnchorage$lat),
    port_lon = map_dbl(event_info, ~ .x$endAnchorage$lon),
    visit_date = as.Date(start)
  ) %>%
  select(visit_date, port_name, port_country, port_lat, port_lon)


# ----------------- #
# Step 6. Quick map #
# ----------------- #

port_summary

# all year 
Sys.setlocale("LC_TIME", "C")

ggplot(port_summary, aes(x = visit_date, y = port_name)) +
  geom_point(aes(color = port_name), size = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Ports visited in 2024",
    x = "Month",
    y = "Port",
    color = "Port"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# map

# Modify port labels to include country in legend
port_summary <- port_summary %>%
  mutate(port_label = paste0(port_name, " (", port_country, ")"))

# Load world polygons
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Define bounding box
extent <- coord_sf(
  xlim = c(min(port_summary$port_lon) - 2, max(port_summary$port_lon) + 2),
  ylim = c(min(port_summary$port_lat) - 2, max(port_summary$port_lat) + 2)
)

# Plot
ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_point(
    data = port_summary,
    aes(x = port_lon, y = port_lat, color = port_label),
    size = 4
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  extent +
  labs(color = "Port (Country)")

 