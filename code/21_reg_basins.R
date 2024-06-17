# This code implements the basin RDD design regression

rm(list = ls())

# Load packages
pacman::p_load(
  tidyverse,
  rhdx,
  countrycode,
  magrittr, 
  terra, 
  countrycode, 
  haven, 
  rnaturalearth, 
  rnaturalearthdata, 
  sf, 
  readxl, 
  fixest
)

# reading in basin data
basin_evi <- read_csv("/data/jde/basins_evi/basin_evi.csv")

sample_basins <- read_sf("~/minesfood/data/sample_basins_tza.shp")

left_join(basin_evi, sample_basins)
