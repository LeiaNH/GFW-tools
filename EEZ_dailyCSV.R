# ~~~~~~~~~~~~~~ #
# Step 5: EZZ sz #
# ~~~~~~~~~~~~~~ #

p

#https://www.marineregions.org/gazetteer.php?p=details&id=8369


# Load EEZ polygons: EEZ shapefile from MarineRegions.org
eez <- sf::read_sf(paste0(WD,"input/World_EEZ_v10_20180221"), layer = 'eez_v10') %>% 
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

# Create dataframe of filenames dates
effort_files <- tibble(
  file = list.files(paste0(WD, 'input/fleet-daily-csvs-100-v2-2019'), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

# Read in data
fleet <- map_dfr(effort_files$file, .f = read_csv) 

# The native data are at a resolution of 0.01 degrees
summary(fleet$cell_ll_lat)

# We're interested in making global maps and 0.01 degrees is a much finer resolution than necessary
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
  group_by(lon_bin, lat_bin, flag, geartype) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>%
  # we select only trawlers
  dplyr::filter(
    geartype == "trawlers")

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


# Top 3 
(eez_effort %>% 
    group_by(flag) %>%
    summarize(
      total_hours = sum(hours, na.rm = TRUE)) %>%
    arrange(desc(total_hours)) %>%
    slice(1:3)) # georgia, spain, china

# Select those top 3 vessel flags
eez_effort <- eez_effort %>%
  dplyr::filter(flag == "GEO" | flag == "ESP" | flag == "CHN" )

# Last plot!
p +
  # plot navigation hours data
  geom_tile(data = eez_effort, aes(x = lon_bin, y = lat_bin, fill = hours))+
  scale_fill_viridis_c(trans="log10")+
  # plot MRT eez
  geom_sf(data = eez,
          color = "red",
          fill=NA,
          alpha = 0.8,
          linetype = 2) +
  #plot radar detections over the raster
  geom_point(
    data = radar,
    aes(x = longitude, y = latitude), 
    colour= "red") +
  # limit your study area
  extent +
  # facet by vessel flag
  facet_wrap(~flag) 
