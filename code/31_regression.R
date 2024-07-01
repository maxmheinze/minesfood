
# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  tidyverse,
  rhdx,
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

downstream_upstream_distance <- read_csv("/data/jde/processed/downstream_upstream_distance.csv")

basin_evi <- read_csv("/data/jde/basins_evi/basin_evi.csv")


# Prepare Data for Regression ---------------------------------------------

downstream_upstream_distance_evi <- left_join(downstream_upstream_distance, basin_evi)

dup <- downstream_upstream_distance_evi %>%
  mutate(downstream = ifelse(distance == 0, 1, downstream)) %>%
  group_by(HYBAS_ID, year) %>%
  arrange(distance) %>% 
  slice_head(n = 1) %>%
  ungroup()


# Regressidownstream Regression --------------------------------------------------------------

mod1 = feols((max_cropland_EVI) ~ distance + I(distance^2) + downstream | year + as.factor(mine_basin), data = dup)

mod2 = feols((max_EVI) ~ distance + I(distance^2) + downstream | year +  as.factor(mine_basin), data = dup)

mod3 = feols((max_cropland_EVI) ~ ( distance + I(distance^2))*downstream | year +  as.factor(mine_basin), data = dup)

mod4 = feols((max_EVI) ~ ( distance + I(distance^2))*downstream | year +  as.factor(mine_basin), data = dup)

mod5 = feols((max_cropland_EVI) ~ (distance) + downstream | year +  as.factor(mine_basin), data = dup)

mod6 = feols((max_cropland_EVI) ~ (distance)*downstream | year +  as.factor(mine_basin), data = dup)

mod7 = feols((max_EVI) ~ (distance) + downstream | year +  as.factor(mine_basin), data = dup)

mod8 = feols((max_EVI) ~ (distance)*downstream | year +  as.factor(mine_basin), data = dup)

etable(mod1, mod2, mod3, mod4)
etable(mod5, mod6, mod7, mod8)



# rdrobust Spatial Discontinuity  -----------------------------------------

dup_01 <- dup %>%
  mutate(distance = ifelse(downstream == 0, distance*-1, distance)) %>%
  mutate(distance = distance/1000)

dup_02 <- dup %>%
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

