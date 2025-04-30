# GFW-tools

Welcome! This repository contains a set of R scripts I’ve developed to work with Global Fishing Watch (GFW) datasets, using the `gfwr` package. Each script was built for a specific task, so feel free to adapt them to your needs.

This project is mainly intended to share practical examples with anyone interested in GFW data and vessel tracking.

Getting Started
---------------

To run these scripts, you’ll need:

1. Input data  
   Download and save the folder from this link:  
   https://www.dropbox.com/scl/fo/69p3ghsqf5w3ovrqpiqe3/AIw89KGO2-Y7TA9lMbvW9VY?rlkey=t3q0gnnqbfbnwy7073nb9ovjs&dl=1  
   Then, set your working directory to this folder in each script.

2. GFW API Token  
   Some scripts use the GFW API via the `gfwr` package. To access it:  
   - Request a token from the GFW API Portal:  
     https://globalfishingwatch.org/our-apis/tokens/signup  
   - Save your token as a CSV file named `key.csv`  
   - Ensure the script can read this file properly.

3. R Packages  
   Required packages are listed at the top of each script. Install them before running the code.

Script Overview
---------------

- fishingEffort_API.R  
  Summarizes fishing effort within a custom polygon using GPS APIs.

- buffer_dailyCSV.R  
  Summarizes vessel activity within a buffer around animal locations using static GFW daily CSV data.

- EEZ_monthlyCSV.R  
  Summarizes vessel activity inside an EEZ using static GFW monthly CSV data.

- vessel_ID.R  
  Explores different ways to get information on a vessel using its MMSI.

- radar_AISdisablings.R  
  Explores a way to identify AIS disabling dataset (Welch et al., 2022) around animal locations.

- radar_SAR.R  
  Explores a way to identify vessels from satellite SAR imagery (Paolo et al., 2024) around animal locations.

---

I hope you find these tools useful!
