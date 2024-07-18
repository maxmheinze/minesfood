
# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  tidyverse,
  rddtools,
  countrycode,
  magrittr,
  terra,
  countrycode,
  haven,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  readxl,
  fixest,
  rdrobust
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


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
  filter(!is.na(eco_id)) |>
  mutate(downstream = ifelse(distance == 0, 1, downstream),
         distance = distance / 10^3) %>%
  group_by(HYBAS_ID, year) %>%
  arrange(distance) %>%
  slice_head(n = 1) %>%
  ungroup()

# rdrobust Spatial Discontinuity  -----------------------------------------

dup_01 <- df_reg %>%
  mutate(distance = ifelse(downstream == 0, distance*-1, distance)) %>%
  mutate(distance = distance)

dup_02 <- df_reg %>%
  mutate(distance = ifelse(downstream == 0, distance*-1, distance)) %>%
  mutate(distance = distance) %>%
  filter(abs(distance) < 200)

fit = rdrobust(y = dup_01$max_EVI, x = dup_01$distance, c = 0, h=1,all=TRUE)

dup_02 <- dup_01 %>%
  filter(year == 2021)

rdplot(dup_02$max_EVI, dup_02$distance,
       x.lim = c(-50,50),
       #y.lim = c(0.1400,0.9933),
       x.lab="Distance",
       y.lab="max_EVI", p = 3)



rdplot(dup_01$max_cropland_EVI, dup_01$distance,
       x.lim = c(-50,50),
       #y.lim = c(0.02,0.96),
       x.lab="Distance",
       y.lab="max_cropland_EVI", p = 3)
