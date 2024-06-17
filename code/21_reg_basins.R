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

downstream_upstream_distance <- read_csv("~/minesfood/data/downstream_upstream_distance.csv")

# reading in basin data
basin_evi <- read_csv("data_local/basin_evi.csv")


downstream_upstream_distance_evi <- left_join(downstream_upstream_distance, basin_evi)


