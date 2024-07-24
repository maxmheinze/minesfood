# Load Packages -----------------------------------------------------------

rm(list = ls())

pacman::p_load(
  sf,
  dplyr,
  tmap,
  tidyverse,
  countrycode,
  geosphere,
  terra,
  ncdf4
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# basins to get data for
basins <- read_sf(p("processed/relevant_basins_ordered.gpkg"))


#####
# Time-invariant stuff

# Dams (on hold for now)

# dl not working atm
# dams <- sf::st_read(
#   dsn = "https://data.apps.fao.org/map/gsrv/gsrv1/geonetwork/ows?service=wfs&version=2.0.0",
#     layer = "dams_africa_38077")
# dams <- read_xlsx(p("dams/Africa-dams_eng.xlsx"), sheet = "Dams", skip = 1)

# Elevation
elevation <- terra::rast(p("grid_pnas/elevation.tif", pre = DATA_ALT))

elevation_df <- terra::extract(x = elevation, y = basins,
                               weights = TRUE, exact = TRUE) |>
  filter(!is.na(elevation)) |>
  group_by(ID) |> transmute(weighted = elevation * weight / sum(weight)) |>
  summarise(elevation = sum(weighted, na.rm = T)) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)

saveRDS(elevation_df, file = p("processed/elevation.RDS"))

rm(elevation); gc()


# Slope
slope <- terra::rast(p("grid_pnas/slope.tif", pre = DATA_ALT))

slope_df <- terra::extract(x = slope, y = basins,
                           weights = TRUE, exact = TRUE) |>
  filter(!is.na(slope)) |>
  group_by(ID) |> transmute(weighted = slope * weight / sum(weight)) |>
  summarise(slope = sum(weighted, na.rm = T)) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)

saveRDS(slope_df, file = p("processed/slope.RDS"))

rm(slope); gc()


# SoilGrid (types 0-118)
# taking the value with the highest weight
soilgrid <- terra::rast(p("grid_pnas/soilgrid.tif", pre = DATA_ALT))

soil_prim_df <- terra::extract(x = soilgrid, y = basins,
                                weights = TRUE, exact = TRUE) |>
  filter(!is.na(soilgrid)) |>
  rename(soil_prim = soilgrid) |>
  group_by(ID) |> mutate(weight = weight / sum(weight)) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  group_by(HYBAS_ID, soil_prim) |>
  summarise(weight = sum(weight, na.rm = T)) |>
  filter(!is.na(soil_prim)) |>
  group_by(HYBAS_ID) |>
  slice_max(weight, n = 1) |>
  dplyr::select(-weight)

saveRDS(soil_prim_df, file = p("processed/soil_primary.RDS"))

# soil_prim_df <- readRDS(p("processed/soil_primary.RDS"))
groups <- read.csv("inst/soilgrid_grouped.csv")
soil_prim_df$soilgrid_grouped <- droplevels(as.factor(
  groups[match(soil_prim_df$soil_prim, groups$Number), "WRB_group"]))

saveRDS(soil_prim_df, file = p("processed/soil_primary-grouped.RDS"))

# soilq_avg_df <- terra::extract(x = soilgrid, y = basins,
#                                 weights = TRUE, exact = TRUE) |>
#   filter(!is.na(soilgrid)) |>
#   rename(soilq_avg = soilgrid) |>
#   group_by(ID) |> transmute(weighted = soilq_avg * weight / sum(weight)) |>
#   summarise(soilq_avg = sum(weighted, na.rm = T)) |>
#   left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
#   relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)
# saveRDS(soilq_avg_df, file = p("processed/soilq_average.RDS"))

rm(soilgrid); gc()


# Biome (assigning eco-region with highest weight per basin)
ecoregions <- terra::rast(p("grid_pnas/ecoregions_2017.tif", pre = DATA_ALT))
ecoregions_concordance <- read_csv(p("grid_pnas/ecoregions_2017_concordance_tbl.csv", pre = DATA_ALT))
names(ecoregions_concordance) <- tolower(names(ecoregions_concordance))

ecoregions_df <- terra::extract(x = ecoregions, y = basins,
                                weights = TRUE, exact = TRUE) |>
  filter(!is.na(ecoregions_2017)) |>
  rename(eco_id = ecoregions_2017) |>
  group_by(ID) |> mutate(weight = weight / sum(weight)) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  dplyr::select(-ID) |> relocate(HYBAS_ID, .before = eco_id) |>
  group_by(HYBAS_ID, eco_id) |>
  summarise(ecoregion_weight = sum(weight, na.rm = T)) |>
  filter(!is.na(eco_id)) |>
  group_by(HYBAS_ID) |>
  slice_max(ecoregion_weight, n = 1) |>
  dplyr::select(-ecoregion_weight) |>
  left_join(ecoregions_concordance, by = "eco_id")

saveRDS(ecoregions_df, file = p("processed/ecoregion.RDS"))

rm(ecoregions, ecoregions_concordance); gc()


# Accessibility to Cities 2015
accessibility_to_cities_2015 <- terra::rast(p("grid_pnas/accessibility_to_cities_2015.tif", pre = DATA_ALT))

accessibility_to_cities_2015_df <- terra::extract(x = accessibility_to_cities_2015,
                                                  y = basins,
                                                  weights = TRUE, exact = TRUE) |>
  filter(accessibility_to_cities_2015 > 0, !is.na(accessibility_to_cities_2015)) |> 
  group_by(ID) |> transmute(weighted = accessibility_to_cities_2015 * weight / sum(weight)) |>
  summarise(accessibility_to_cities_2015 = sum(weighted, na.rm = T)) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID)

saveRDS(accessibility_to_cities_2015_df, file = p("processed/accessibility_to_cities_2015.RDS"))

rm(accessibility_to_cities_2015); gc()


#####
# Merging time-invariant variables in one dataframe

elevation_df <- readRDS(p("processed/elevation.RDS"))
slope_df <- readRDS(p("processed/slope.RDS"))
soil_prim_df <- readRDS(p("processed/soil_primary-grouped.RDS"))
# soilq_avg_df <- readRDS(p("processed/soilq_average.RDS"))
ecoregions_df <- readRDS(p("processed/ecoregion.RDS"))
accessibility_to_cities_2015_df <- readRDS(p("processed/accessibility_to_cities_2015.RDS"))

geo_data_df <- left_join(ecoregions_df, elevation_df, by = "HYBAS_ID") |>
  left_join(slope_df, by = "HYBAS_ID") |>
  left_join(soil_prim_df, by = "HYBAS_ID") |>
  left_join(accessibility_to_cities_2015_df, by = "HYBAS_ID") # |> 
  # left_join(soilq_avg_df, by = "HYBAS_ID")

saveRDS(geo_data_df, file = p("processed/geo_data_agg.RDS"))


#####
# Time-varying climate stuff

######
# Weather variables from Climatic Research Unit (CRU)
# https://www.nature.com/articles/s41597-020-0453-3

dates <- paste0(rep(2001:2023, each = 12), "-", stringr::str_pad(1:12, 2, pad = "0"))

# Temperature (mean)
tmp_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.tmp.dat.nc", pre = "/data/AP_FP/"), 
                        subds = "tmp") 
pos <- which(substr(time(tmp_data), 1, 7) %in% dates)

start <- Sys.time()
tmp_df <- terra::extract(tmp_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("tmp"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("tmp"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("tmp"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("tmp")) |>
  left_join(tibble(name = names(tmp_data),
                   time = substr(time(tmp_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            tmp_mean = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(tmp_df, file = p("processed/cru_tmp.RDS"))


# Temperature (max)
tmx_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.tmx.dat.nc", pre = "/data/AP_FP/"),
  subds = "tmx")
pos <- which(substr(time(tmx_data), 1, 7) %in% dates)

start <- Sys.time()
tmx_df <- terra::extract(tmx_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("tmx"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("tmx"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("tmx"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("tmx")) |>
  left_join(tibble(name = names(tmx_data),
                   time = substr(time(tmx_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            tmp_max = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(tmx_df, file = p("processed/cru_tmx.RDS"))


# Temperature (min)
tmn_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.tmn.dat.nc", pre = "/data/AP_FP/"),
  subds = "tmn")
pos <- which(substr(time(tmn_data), 1, 7) %in% dates)

start <- Sys.time()
tmn_df <- terra::extract(tmn_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("tmn"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("tmn"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("tmn"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("tmn")) |>
  left_join(tibble(name = names(tmn_data),
                   time = substr(time(tmn_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            tmp_min = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(tmn_df, file = p("processed/cru_tmn.RDS"))


# Precipitation
pre_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.pre.dat.nc", pre = "/data/AP_FP/"),
  subds = "pre")
pos <- which(substr(time(pre_data), 1, 7) %in% dates)

start <- Sys.time()
pre_df <- terra::extract(pre_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("pre"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("pre"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("pre"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("pre")) |>
  left_join(tibble(name = names(pre_data),
                   time = substr(time(pre_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            precipitation = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(pre_df, file = p("processed/cru_pre.RDS"))


# Cloud cover
cld_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.cld.dat.nc", pre = "/data/AP_FP/"),
  subds = "cld")
pos <- which(substr(time(cld_data), 1, 7) %in% dates)

start <- Sys.time()
cld_df <- terra::extract(cld_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("cld"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("cld"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("cld"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("cld")) |>
  left_join(tibble(name = names(cld_data),
                   time = substr(time(cld_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            cloud_cover = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(cld_df, file = p("processed/cru_cld.RDS"))


# WET (Rainy Days)
wet_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.wet.dat.nc", pre = "/data/AP_FP/"),
  subds = "wet")
pos <- which(substr(time(wet_data), 1, 7) %in% dates)

start <- Sys.time()
wet_df <- terra::extract(wet_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("wet"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("wet"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("wet"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("wet")) |>
  left_join(tibble(name = names(wet_data),
                   time = substr(time(wet_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            wet_days = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(wet_df, file = p("processed/cru_wet.RDS"))


# FRS (Frost Days)
frs_data <- terra::rast(p("CRU_unzipped/cru_ts4.08.1901.2023.frs.dat.nc", pre = "/data/AP_FP/"),
  subds = "frs")
pos <- which(substr(time(frs_data), 1, 7) %in% dates)

start <- Sys.time()
frs_df <- terra::extract(frs_data[[pos]], basins, weights = T, exact = T) |>
  filter(if_all(contains("frs"), ~ !is.na(.x))) |> 
  group_by(ID) |>
  mutate(weight = weight / sum(weight),
         across(contains("frs"), function(x) x * weight)) |>
  dplyr::select(-weight) |>
  summarise(across(contains("frs"), function(x) sum(x, na.rm = T))) |>
  left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
  relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |>
  tidyr::pivot_longer(contains("frs")) |>
  left_join(tibble(name = names(frs_data),
                   time = substr(time(frs_data), 1, 7)),
            by = "name") |>
  transmute(HYBAS_ID,
            year = as.numeric(substr(time, 1, 4)),
            month = as.numeric(substr(time, 6, 7)),
            frs_days = value) |>
  arrange(year, month, HYBAS_ID)
cat("Calculation finished after", format(Sys.time() - start), "\n")

saveRDS(frs_df, file = p("processed/cru_frs.RDS"))


#####
# Aggregating monthly frequency of time-varying climate variables and merging
tmp_df <- readRDS(p("processed/cru_tmp.RDS"))
tmx_df <- readRDS(p("processed/cru_tmx.RDS"))
tmn_df <- readRDS(p("processed/cru_tmn.RDS"))
pre_df <- readRDS(p("processed/cru_pre.RDS"))
cld_df <- readRDS(p("processed/cru_cld.RDS"))
wet_df <- readRDS(p("processed/cru_wet.RDS"))
frs_df <- readRDS(p("processed/cru_frs.RDS"))

met_data_df <- left_join(tmp_df, tmx_df, by = c("HYBAS_ID", "year", "month")) |>
  left_join(tmn_df, by = c("HYBAS_ID", "year", "month")) |>
  left_join(pre_df, by = c("HYBAS_ID", "year", "month")) |>
  left_join(cld_df, by = c("HYBAS_ID", "year", "month")) |>
  left_join(wet_df, by = c("HYBAS_ID", "year", "month")) |>
  left_join(frs_df, by = c("HYBAS_ID", "year", "month")) |>
  group_by(HYBAS_ID, year) |>
  summarise(tmp_mean = mean(tmp_mean, na.rm = T),
            tmp_max = max(tmp_max, na.rm = T),
            tmp_min = min(tmp_min, na.rm = T),
            precipitation = sum(precipitation, na.rm = T),
            cloud_cover = mean(cloud_cover, na.rm = T),
            wet_days = sum(wet_days, na.rm = T),
            frs_days = sum(frs_days, na.rm = T))

saveRDS(met_data_df, file = p("processed/met_data_agg.RDS"))
