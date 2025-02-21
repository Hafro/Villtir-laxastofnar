# Wild salmon stocks
Catch data and counters

# Files
* stofnstaerdar.Rmd
  * Look at counter and catch data to determine which probability distribution to use and estimate the standard deviation
  * Input: teljaragogn.csv and MFRI catch database
* teljargogn.csv columns
  * ar: year
  * fjoldi: Number of salmon counted
  * vatnsfall: River / counter
  * afli: Catch retained
  * sleppt: Catch released
* veidihlutfall.R
  * Estimate exploitation rate by year and region from catch and counter data
  * Input: teljargogn.csv
  * Output: veidihlutfall.csv
* veidihlutfall.csv columns
  * ar: year
  * svaedi: region
  * mp: mean exploitation rate for region and year
  * n: number of counters used for the mean calculation
  * landsvaedi_id: region index in catch database
* veidi.R
  * Calculate stock estimates from catch and explotation rate
  * Input: veidihlutfall.csv, ar.csv (coordinates and info from the 2020 risk assessment) and MFRI catch database
  * Output: ar2025.csv (info for all the rivers), ahaetta.csv (input for the risk assessment)
* ar.csv columns
  * nafn: river name
  * V: Longitude
  * N: Latitude
  * fjarlægð: Distance in 2020 assessment (to be updated)
  * Meðalfjöldi: Stock size in 2020 assessment
  * fjoldi.flokk: Size category in 2020 assessment
  * adal_nr: River number in MFRI database
  * ATH: Comment
* ar2025.csv columns
  * adal_nr: River number in MFRI database (main river/ river that connects to the marine environment)
  * adalvatnsfall: River name in MFRI database (main river/ river that connects to the marine environment)
  * landsvaedi: Region
  * landsvaedi_id: region index
  * M: Max stock estimate (from 1974)
  * hraMedal: Arithmetic mean (from 1974)
  * logMedal: Geometric mean log scale (from 1974) 
  * expMedal: Geometric mean original scale (from 1974)
  * M10: Max stock estimate from 2014
  * hraMedal10: Arithmetic mean (from 2014)
  * logMedal10: Geometric mean log scale (from 2014)
  * expMedal10: Geometric mean original scale (from 2014)
  * N_ar: Number of years with salmon records
  * nafn, V, N, fjarlægð, Meðalfjöldi, fjoldi.flokk, ATH columns from ar.csv
  * ath: comment
  * vatnsfall_id: river index in old database (to be phased out)
  * vatnsfall: river name in old database
* ahaetta.csv columns
  * adal_nr, nafn, V, N, fjarlægð, logMedal10, expMedal10, fjoldi.flokk, ath (see above)
* breytingar.Rmd
  * Overview of rivers with salmon catches
  * Highlighting rivers 
    * Close to 40-50 average stock size
    * Changes from the 2020 assessment 
  * Uses output from veidi.R
