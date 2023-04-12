#################
# WEB SCRAPPING #
#################

help(rvest)

mmsi <- unique(sz_mmsi$mmsi)[1]
read_html(paste0("https://www.myshiptracking.com/vessels/mmsi-",mmsi))  %>%
  html_nodes("strong") %>%
  html_text()

# mmsi <- "240337300"
read_html(paste0("https://www.marinetraffic.com/en/ais/details/ships/mmsi:",mmsi)) %>%
  html_nodes("head") %>%
  html_text() 

############
# GFW DATA #
############

fishing_vessels <- utils::read.csv(file=paste0(WD,"input/fishing-vessels-v2.csv")) 

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

unique(sz_mmsi$mmsi) %in% unique(fishing_vessels$mmsi)