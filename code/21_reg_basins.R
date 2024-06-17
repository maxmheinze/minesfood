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

# reading in downstream upstream data
downstream_upstream_distance <- read_csv("~/minesfood/data/downstream_upstream_distance.csv")

# reading in EVI data
basin_evi <- read_csv("/data/jde/basins_evi/basin_evi.csv")


downstream_upstream_distance_evi <- left_join(downstream_upstream_distance, basin_evi)

dup <- downstream_upstream_distance_evi %>%
  group_by(HYBAS_ID) %>%
  mutate(downstream = ifelse(distance == 0, 1, downstream))

# Regression 
mod1 = feols((max_cropland_EVI) ~ distance + I(distance^2) + downstream | year + as.factor(mine_basin), data = dup)

mod2 = feols((max_EVI) ~ distance + I(distance^2) + downstream | year +  as.factor(mine_basin), data = dup)

mod3 = feols((max_cropland_EVI) ~ ( distance + I(distance^2))*downstream | year +  as.factor(mine_basin), data = dup)

mod4 = feols((max_EVI) ~ ( distance + I(distance^2))*downstream | year +  as.factor(mine_basin), data = dup)

etable(mod1, mod2, mod3, mod4, tex = TRUE)
