# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  dplyr,
  sf,
  readxl,
  readr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

order <- "25"
type <- "" # "" is poly


# Read in Data ------------------------------------------------------------

# treatment
dup <- st_read(p("processed/basins/basins",
                 "-order-", order,
                 ifelse(type == "", ".gpkg", paste0("-", type, ".gpkg")))) |>
  st_drop_geometry() |> as_tibble() |> # need to fix HYBAS_ID?
  transmute(iso3c, HYBAS_ID = basin_id, mine_basin, status,
            dist_n, dist_km, dist_km_centroid, dist_order,
            lake = LAKE, coast = COAST,
            basin_area_km2, mine_area_km2, mine_number, mine_avg_area_km2,
            mine_basins)
# dup_polys <- st_read(p("processed/basins/basins.gpkg")) |>
#   st_drop_geometry() |> as_tibble()  |> # need to fix HYBAS_ID?
#   transmute(iso3c, HYBAS_ID = basin_id, mine_basin, status,
#             dist_n, dist_km, dist_km_centroid, dist_order,
#             lake = LAKE, coast = COAST,
#             basin_area_km2, mine_area_km2, mine_number, mine_avg_area_km2,
#             mine_basins)

# Note: given that order and distance might also change depending on whether a
# basin is up/downstream to an IPIS mine, subsetting alone will not be enough
# dup_ipis <- dup_ipis |>
#   mutate(ipis = ifelse(HYBAS_ID %in% unique(dup_polys$HYBAS_ID), 0, 1))

# outcome EVI/NDVI
basin_evi <- read_csv(p("basins_evi/basin_evi_update_revision.csv"))
basin_ndvi <- read_csv(p("basins_evi/basin_ndvi_update_revision.csv"))

# land use
lc_cci <- readRDS(p("processed/CCI_lc_summarised.RDS"))
lc_cci <- rbind(lc_cci, lc_cci |> filter(year == 2022) |> mutate(year = 2023)) # duplicating values for 2023 as we miss data

# region concordance
regions_usda <- read_csv(p("processed/concordance_regions_USDA.csv"))

# geographical controls
geo_controls <- readRDS(p("processed/geo_data_agg.RDS"))

# meteorological controls
met_controls_cru <- readRDS(p("processed/cru_met_data_agg.RDS"))
met_controls_tc <- readRDS(p("processed/tc_met_data_agg.RDS"))
met_controls_chirps <- readRDS(p("processed/chirps_met_data_agg.RDS"))

met_controls <- left_join(met_controls_cru, met_controls_tc,
                          by = c("HYBAS_ID", "year")) |>
  left_join(met_controls_chirps, by = c("HYBAS_ID", "year"))

# population
pop <- read_csv(p("processed/basin_pop.csv"))


# Prepare Data for Regression ---------------------------------------------

df_reg <- full_join(dup, basin_evi, by = "HYBAS_ID") |>
  relocate(year, .after = iso3c) |>
  left_join(basin_ndvi, by = c("HYBAS_ID", "year")) |>
  left_join(lc_cci, by = c("HYBAS_ID", "year")) |>
  left_join(regions_usda, by = "iso3c") |>
  mutate(region_grouped = region,
         region_grouped = replace(region_grouped,
                                  grepl("North|East", region_grouped),
                                  "North & East Africa")) |>
  relocate(region, .after = iso3c) |>
  left_join(geo_controls, by = "HYBAS_ID") |>
  left_join(met_controls, by = c("HYBAS_ID", "year")) |>
  left_join(pop, by = c("HYBAS_ID", "year")) |>
  mutate(t.trend = year - min(year) + 1,
         distance_bin = cut(dist_km, breaks = c(-Inf, 10, 20, 30, 40, 50, Inf)),
         distance_centroid_bin = cut(dist_km_centroid,
                                     breaks = c(-Inf, 10, 20, 30, 40, 50, Inf)),
         downstream = ifelse(dist_order < 0, 0, 1))

# df_reg_polys <- full_join(dup_polys, basin_evi, by = "HYBAS_ID") |>
#   filter(!is.na(dist_order)) |> # exclude basins from IPIS
#   relocate(year, .after = iso3c) |>
#   left_join(basin_ndvi, by = c("HYBAS_ID", "year")) |>
#   left_join(lc_cci, by = c("HYBAS_ID", "year")) |>
#   left_join(regions_usda, by = "iso3c") |>
#   mutate(region_grouped = region,
#          region_grouped = replace(region_grouped,
#                                   grepl("North|East", region_grouped),
#                                   "North & East Africa")) |>
#   relocate(region, .after = iso3c) |>
#   left_join(geo_controls, by = "HYBAS_ID") |>
#   left_join(met_controls, by = c("HYBAS_ID", "year")) |>
#   left_join(pop, by = c("HYBAS_ID", "year")) |>
#   mutate(t.trend = year - min(year) + 1,
#          distance_bin = cut(dist_km, breaks = c(-Inf, 10, 20, 30, 40, 50, Inf)),
#          distance_centroid_bin = cut(dist_km_centroid,
#                                      breaks = c(-Inf, 10, 20, 30, 40, 50, Inf)),
#          downstream = ifelse(dist_order < 0, 0, 1))

# Save data ---------------------------------------------------------------

saveRDS(df_reg, file = p("processed/df_reg",
                         "-order-", order,
                         ifelse(type == "", ".RDS", paste0("-", type, ".RDS"))))
# saveRDS(df_reg_polys, file = p("processed/df_reg_polys.RDS"))
