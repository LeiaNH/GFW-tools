# vessel_ID.R
# aim: Different ways to find out the vessel identity of a mmsi

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define an mmsi
# Step 4. Use GFW files
# Step 5. Use GFW API
# Step 6. Use Web Scrapping

# ------------------- #
# Step 1. Set your WD #
# ------------------- #

WD <- "D:/Dropbox/" #minipc
#WD <- "C:/Users/lnh88/Dropbox/" #laptop

# In R, scipen is an option that controls the level of scientific notation used when printing numbers. Specifically, it determines the threshold at which scientific notation is used to represent numeric values.
options(scipen=999)

# -------------------- #
# Step 2. Requirements #
# -------------------- #

library(tidyverse)

# install rvest for web scrapping
#install.packages("rvest")

# load rvest
library(rvest)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")

# load rgfw
library(gfwr)


# ------------ #
# Step 3. mmsi #
# ------------ #

mmsiCode <- 225987981

# ----------------- #
# Step 4. GFW files #
# ----------------- #


fishing_vessels <- utils::read.csv(file=paste0(WD,"GitData/GFW-tools/input/fishing-vessels-v2.csv")) 

glimpse(fishing_vessels)

# mmsi: Maritime Mobile Service Identity, the identifier for AIS
# flag_ais: Flag state (ISO3 value) for the vessel as determined by the first three digits (MID) of the MMSI number
# flag_registry: Flag state (ISO3 value) for the vessel as listed on vessel registries (when applicable)
# flag_gfw: Flag state (ISO3 value) assigned to the vessel by GFW after considering all available information
# vessel_class_inferred: Vessel class (geartype) inferred by the GFW vessel classification neural net model
# vessel_class_inferred_score: Neural net score (0-1) for the top scoring vessel class inferred by the GFW vessel classification neural net model. Values closer to 1 indicate higher confidence by the neural net
# vessel_class_registry: Vessel class (geartype) for the vessel as listed on vessel registries (if applicable)
# vessel_class_gfw: Vessel class (geartype) assigned to the vessel by GFW after considering all available information
# self_reported_fishing_vessel: Whether the vessel broadcasts the 'Fishing' ship type in > 98% of AIS identity messages
# length_m_inferred: Vessel length (meters) inferred by the GFW vessel classification neural net model
# length_m_registry: Vessel length (meters) for the vessel as listed on vessel registries (if applicable)
# length_m_gfw: Vessel length (meters) assigned to the vessel by GFW after considering all available information
# engine_power_kw_inferred: Engine power (kilowatts) inferred by the GFW vessel classification neural net model
# engine_power_kw_registry: Engine power (kilowatts) for the vessel as listed on vessel registries (if applicable)"
# engine_power_kw_gfw: Engine power (kilowatts) assigned to the vessel by GFW after considering all available information
# tonnage_gt_inferred: Tonnage (gross tons) inferred by the GFW vessel classification neural net model
# tonnage_gt_registry: Tonnage (gross tons) for the vessel as listed on vessel registries
# tonnage_gt_gfw: Tonnage (gross tons) assigned to the vessel by GFW after considering all available information
# registries_listed: Registries where the vessel is listed and used to inform the _registry fields (if applicable)
# fishing_hours_2012: Fishing hours for the vessel in 2012
# fishing_hours_2013: Fishing hours for the vessel in 2013
# fishing_hours_2014: Fishing hours for the vessel in 2014
# fishing_hours_2015: Fishing hours for the vessel in 2015
# fishing_hours_2016: Fishing hours for the vessel in 2016
# fishing_hours_2017: Fishing hours for the vessel in 2017
# fishing_hours_2018: Fishing hours for the vessel in 2018
# fishing_hours_2019: Fishing hours for the vessel in 2019
# fishing_hours_2020: Fishing hours for the vessel in 2020

fishing_vessels <- fishing_vessels %>%
  dplyr::filter(mmsi == mmsiCode)

# --------------- #
# Step 5. GFW API #
# --------------- #

# The use of gfwr requires a GFW API token, which users can request from the GFW API Portal (https://globalfishingwatch.org/our-apis/tokens). Mine is next:
key <- readr::read_csv(paste0(WD, "GitData/GFW-tools/key.csv")) %>% # here I load a csv file where I stored my API token
  pull(key) 

(gfw <- gfwr::get_vessel_info(query = mmsiCode, search_type = "search", 
                              dataset = "all", key = key))

# --------------------- #
# Step 6. Web Scrapping #
# --------------------- #

help(rvest)

rvest::read_html(paste0("https://www.myshiptracking.com/vessels/mmsi-", mmsiCode))  %>%
  rvest::html_nodes("strong") %>%
  rvest::html_text()

rvest::read_html(paste0("https://www.marinetraffic.com/en/ais/details/ships/mmsi:", mmsiCode)) %>%
  rvest::html_nodes("head") %>%
  rvest::html_text() 

