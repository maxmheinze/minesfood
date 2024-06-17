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
basin_evi <- read_csv("data_local/basin_evi.csv")

relevant_basins <- read_sf("data/relevant_basins.shp")

basin_evi_rel <- left_join(basin_evi, relevant_basins)




