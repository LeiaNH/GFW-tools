# GFW-tools

Welcome to my repository! Here, you'll find a collection of R scripts that I've developed to work with GFW datasets, along with the gfwr package. Each of the scripts available were coded for specific purposes, so you may not need to run all of them depending on your needs. I created this repository mainly to share some exercises with people who are interested in GFW datasets, as well as vessel tracking data in general.

If you're interested in using GFW-tools scripts, you can download the input data from the following link. I hope you find these resources useful!

https://www.dropbox.com/sh/wzjquh63ifolwpl/AADOAQPtE_yKmTVIHjjxyhcka?dl=1

Codes:

* fishingEffort_API.R: This code demonstrates how to use the gfwr::get_raster() function to obtain a summary of fishing effort within an area delimited by a polygon in geojson format.

* buffer_dailyCSV.R: This code shows how to use the daily CSV files available in Global Fishing Watch to obtain a summary of fishing vessel traffic within an area delimited by a buffer around an animal location.

* EEZ_dailyCSV.R: This code demonstrates how to use the daily CSV files available in Global Fishing Watch to obtain a summary of fishing vessel traffic within an area delimited by an Exclusive Economic Zone (EEZ).

* vessel_ID.R: This code provides various methods for obtaining information about a specific MMSI code (vessel identity).


