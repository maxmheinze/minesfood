
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


# Read in Data ------------------------------------------------------------

# outcome
basin_evi <- read_csv("/data/jde/basins_evi/basin_evi.csv")

# treatment
dup <- read_csv("/data/jde/processed/downstream_upstream_distance_ordered.csv")

# geographical controls
geo_controls <- readRDS("/data/jde/processed/geo_data_agg.RDS")

# meteorological controls
met_controls <- readRDS("/data/jde/processed/met_data_agg.RDS")


# check whether some basins are up- and downstream at the same time
dup |> 
  group_by(HYBAS_ID, status) |> arrange(distance) |> slice_head(n = 1) |> 
  group_by(HYBAS_ID) |> count() |> filter(n > 1)



# Prepare Data for Regression ---------------------------------------------

df_reg <- full_join(dup, basin_evi, by = "HYBAS_ID") |> 
  left_join(geo_controls, by = "HYBAS_ID") |> 
  left_join(met_controls, by = c("HYBAS_ID", "year"))

# what's done here is that we exclude basins where some of the time-invariant stuff
# is missing and that we assign a downstream basin to be unique for a mine? Meaning
# that a basin can only be downstream to one mine but not more? we only lose ~30 
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

# Downstream Regression, full sample -------------------------------------------

# no covariates, linear distance
mod1 = feols((max_cropland_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg)

mod2 = feols((max_cropland_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg)

mod3 = feols((max_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg)

mod4 = feols((max_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg)

etable(mod1, mod2, mod3, mod4)

# with covariates, linear distance
mod5 = feols((max_cropland_EVI) ~ distance + downstream + elevation + slope | 
               year + as.factor(mine_basin), 
             data = df_reg)

mod6 = feols((max_cropland_EVI) ~ distance * downstream + elevation + slope | 
               year +  as.factor(mine_basin), 
             data = df_reg)

mod7 = feols((max_EVI) ~ distance + downstream + elevation + slope | 
               year +  as.factor(mine_basin), 
             data = df_reg)

mod8 = feols((max_EVI) ~ distance * downstream + elevation + slope | 
               year +  as.factor(mine_basin), 
             data = df_reg)

etable(mod5, mod6, mod7, mod8)



# Downstream Regression, balanced sample ---------------------------------------

basins_balanced <- df_reg |> group_by(mine_basin, HYBAS_ID) |> 
  count() |> filter(n == 24) |> pull(mine_basin) |> unique()
df_reg_balanced <- df_reg |> filter(mine_basin %in% basins_balanced,
                                    order <= 5, year > 2015, year < 2024, order != 0)

# no covariates, linear distance
mod1 = feols(log(max_cropland_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced, 
             cluster = "HYBAS_ID")

mod2 = feols(log(max_cropland_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced, 
             cluster = "HYBAS_ID")

mod3 = feols(log(max_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced |> filter(max_EVI > 0.15), 
             cluster = "HYBAS_ID")

mod4 = feols(log(max_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced |> filter(max_EVI > 0.15), 
             cluster = "HYBAS_ID")

etable(mod1, mod2, mod3, mod4)

# with covariates, linear distance
mod5 = feols(log(max_cropland_EVI) ~ 
               distance + downstream + 
               elevation + slope + 
               tmp_max + precipitation | 
               year + as.factor(mine_basin), 
             data = df_reg_balanced, 
             cluster = "HYBAS_ID")

mod6 = feols(log(max_cropland_EVI) ~ 
               distance * downstream + 
               elevation + slope + 
               tmp_max + precipitation | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced, 
             cluster = "HYBAS_ID")

mod7 = feols(log(max_EVI) ~ 
               distance + downstream + 
               elevation + slope + 
               tmp_max + precipitation  | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced |> filter(max_EVI > 0.15), 
             cluster = "HYBAS_ID")

mod8 = feols(log(max_EVI) ~ 
               distance * downstream + 
               elevation + slope + 
               tmp_max + precipitation | 
               year +  as.factor(mine_basin), 
             data = df_reg_balanced |> filter(max_EVI > 0.15), 
             cluster = "HYBAS_ID")

etable(mod5, mod6, mod7, mod8)




# rdrobust Spatial Discontinuity  -----------------------------------------

dup_01 <- df_reg %>%
  mutate(distance = ifelse(downstream == 0, distance*-1, distance)) %>%
  mutate(distance = distance/1000)

dup_02 <- df_reg %>%
  mutate(distance = ifelse(downstream == 0, distance*-1, distance)) %>%
  mutate(distance = distance/1000) %>%
  filter(abs(distance) < 200)

fit = rdrobust(y = dup_01$max_EVI, x = dup_01$distance, c = 0, h=1,all=TRUE)

dup_02 <- dup_01 %>%
  filter(year == 2021)

rdplot(dup_02$max_EVI, dup_02$distance, 
       x.lim = c(-50,50),
       #y.lim = c(0.1400,0.9933),
       x.lab="Distance",
       y.lab="max_EVI", p = 2)



rdplot(dup_01$max_cropland_EVI, dup_01$distance, 
       x.lim = c(-50,50),
       #y.lim = c(0.02,0.96),
       x.lab="Distance",
       y.lab="max_cropland_EVI", p = 3)

