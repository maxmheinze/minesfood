# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  dplyr,
  sf,
  readxl,
  readr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# Read in Data ------------------------------------------------------------

# outcome
basin_evi <- read_csv(p("basins_evi/basin_evi.csv"))

# treatment
dup <- read_csv(p("processed/downstream_upstream_distance_ordered.csv"))

# region concordance
regions_usda <- read_csv(p("processed/concordance_regions_USDA.csv"))

# geographical controls
geo_controls <- readRDS(p("processed/geo_data_agg.RDS"))

# meteorological controls
met_controls <- readRDS(p("processed/met_data_agg.RDS"))

# population 
pop <- read_csv(p("processed/basin_pop.csv"))


# Prepare Data for Regression ---------------------------------------------

df_reg <- full_join(dup, basin_evi, by = "HYBAS_ID") |> relocate(year, .after = iso3c) |> 
  left_join(regions_usda, by = "iso3c") |> relocate(region, .after = iso3c) |> 
  left_join(geo_controls, by = "HYBAS_ID") |>
  left_join(met_controls, by = c("HYBAS_ID", "year")) |>
  left_join(pop, by = c("HYBAS_ID", "year")) |> 
  mutate(t.trend = year - 2000)

# what's done here is that we assign a downstream basin to be unique for a mine? 
# Meaning that a basin can only be downstream to one mine but not more? we only 
# lose ~50 observations with that
df_reg <- df_reg %>%
  filter(!is.na(eco_id), year < 2024) |>
  mutate(downstream = ifelse(distance == 0, 1, downstream)) %>%
  group_by(HYBAS_ID, year) %>%
  arrange(distance) %>%
  slice_head(n = 1) %>%
  ungroup()

# throw out 6 basins where max_EVI is missing
basins_EVI_missing <- df_reg |> group_by(HYBAS_ID) |> count() |> filter(n < 23) |> pull(HYBAS_ID)
df_reg <- df_reg |> filter(!HYBAS_ID %in% basins_EVI_missing)

saveRDS(df_reg, file = p("processed/df_reg.RDS"))
