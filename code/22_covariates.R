# Load Packages -----------------------------------------------------------

rm(list = ls())

pacman::p_load(
  sf,
  dplyr,
  tmap,
  tidyverse,
  countrycode,
  geosphere, 
  raster
)

# basins to get data for
basins <- read_sf("/data/jde/processed/relevant_basins.gpkg")


#####
# Time-invariant stuff

# Elevation
elevation <- raster("/data/redd/grid_pnas/elevation.tif")

elevation_df <- raster::extract(x = elevation, y = basins, 
                          df = TRUE, weights = TRUE, normalizeWeights = TRUE) |> 
  transmute(ID, weighted = elevation * weight) |> group_by(ID) |> 
  summarise(elevation = sum(weighted, na.rm = T)) |> 
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |> 
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)

saveRDS(elevation_df, file = "/data/jde/processed/elevation.RDS")

rm(elevation); gc()

# Slope
slope <- raster("/data/redd/grid_pnas/slope.tif")

slope_df <- raster::extract(x = slope, y = basins[1:5, ], 
                                df = TRUE, weights = TRUE, normalizeWeights = TRUE) |> 
  transmute(ID, weighted = slope * weight) |> group_by(ID) |> 
  summarise(slope = sum(weighted, na.rm = T)) |> 
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |> 
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)

saveRDS(slope_df, file = "/data/jde/processed/slope.RDS")

rm(slope); gc()



# Soil Quality (need to check unit, is it a factor?)
soilq <- raster("/data/redd/grid_pnas/soilgrid.tif")

soilq_df <- raster::extract(x = soilq, y = basins[1:25, ], 
                                df = TRUE, weights = TRUE, normalizeWeights = TRUE) |> 
  rename(soilq = soilgrid) |> 
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |> 
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |> 
  group_by(HYBAS_ID, soilq) |> 
  summarise(weight = sum(weight, na.rm = T)) |> 
  filter(!is.na(soilq)) |> 
  group_by(HYBAS_ID) |> 
  slice_max(weight, n = 1) |> 
  dplyr::select(-weight)

saveRDS(soilq_df, file = "/data/jde/processed/soilq.RDS")

rm(soilq); gc()


# Biome (assigning eco-region with highest weight per basin)
ecoregions <- raster("/data/redd/grid_pnas/ecoregions_2017.tif")
ecoregions_concordance <- read_csv("/data/redd/grid_pnas/ecoregions_2017_concordance_tbl.csv")
names(ecoregions_concordance) <- tolower(names(ecoregions_concordance))

ecoregions_df <- raster::extract(x = ecoregions, y = basins, 
                            df = TRUE, weights = TRUE, normalizeWeights = TRUE) |> 
  rename(eco_id = ecoregions_2017) |> 
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |> 
  dplyr::select(-ID) |> relocate(HYBAS_ID, .before = eco_id) |> 
  group_by(HYBAS_ID, eco_id) |> 
  summarise(ecoregion_weight = sum(weight, na.rm = T)) |> 
  filter(!is.na(eco_id)) |> 
  group_by(HYBAS_ID) |> 
  slice_max(ecoregion_weight, n = 1) |> 
  dplyr::select(-ecoregion_weight) |> 
  left_join(ecoregions_concordance, by = "eco_id")
  
saveRDS(ecoregions_df, file = "/data/jde/processed/ecoregion.RDS")

rm(ecoregions, ecoregions_concordance); gc()


#####
# Time-varying stuff

# SPEI

# Temperature

# Precipitation

# Cloud cover

# Sunshine duration


