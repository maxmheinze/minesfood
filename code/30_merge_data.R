# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  dplyr,
  sf,
  readxl,
  readr
)


# Read in Data ------------------------------------------------------------

# outcome
basin_evi <- read_csv(p("basins_evi/basin_evi.csv"))

# treatment
dup <- read_csv(p("processed/downstream_upstream_distance_ordered.csv"))

# geographical controls
geo_controls <- readRDS(p("processed/geo_data_agg.RDS"))

# meteorological controls
met_controls <- readRDS(p("processed/met_data_agg.RDS"))


# check whether some basins are up- and downstream at the same time
dup |>
  group_by(HYBAS_ID, status) |> arrange(distance) |> slice_head(n = 1) |>
  group_by(HYBAS_ID) |> count() |> filter(n > 1)


# Prepare Data for Regression ---------------------------------------------

df_reg <- full_join(dup, basin_evi, by = "HYBAS_ID") |>
  left_join(geo_controls, by = "HYBAS_ID") |>
  left_join(met_controls, by = c("HYBAS_ID", "year")) |>
  mutate(t.trend = year - 2000)

# what's done here is that we exclude basins where some of the time-invariant stuff
# is missing and that we assign a downstream basin to be unique for a mine? Meaning
# that a basin can only be downstream to one mine but not more? we only lose ~50
# observations with that, would have expected more given that mines are clustered

# Do we properly account for the possibility that a basin that is downstream to a
# mine should not be upstream to another?
df_reg <- df_reg %>%
  filter(!is.na(eco_id), year < 2024) |>
  mutate(downstream = ifelse(distance == 0, 1, downstream),
         distance = distance / 10^3) %>%
  group_by(HYBAS_ID, year) %>%
  arrange(distance) %>%
  slice_head(n = 1) %>%
  ungroup()

df_reg |> group_by(order, status) |>
  summarise(distance = mean(distance, na.rm = T)) |>
  arrange(status, order) |> print(n = 30)
# distance is balanced by order, will take order to restrict number of basins

# throw out basins where max_EVI is missing
basins_unbalanced <- df_reg |> group_by(HYBAS_ID) |> count() |> filter(n < 23) |> pull(HYBAS_ID)

df_reg <- df_reg |> filter(!HYBAS_ID %in% basins_unbalanced)

saveRDS(df_reg, file = p("processed/df_reg.RDS"))
