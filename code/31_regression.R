
# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  dplyr,
  rddtools,
  rdrobust,
  countrycode,
  readr,
  fixest
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# Order Regression, full sample -------------------------------------------

# years 2001-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))

# no covariates, order interaction
# mod1_order_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
#                           as.factor(order) + downstream |
#                           year +  as.factor(mine_basin),
#                         data = df_reg,
#                         cluster = "HYBAS_ID")

mod2_order_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          as.factor(order) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg,
                        cluster = "HYBAS_ID")

# with covariates, order interaction
# mod3_order_full = feols(c(max_c_EVI_af, max_c_EVI_ESA) ~
#                           as.factor(order) + downstream +
#                           elevation + slope + soilgrid_grouped +
#                           tmp_max + precipitation +
#                           accessibility_to_cities_2015 + pop_2015 |
#                           year + as.factor(mine_basin),
#                         data = df_reg,
#                         cluster = "HYBAS_ID")

mod4_order_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          as.factor(order) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg,
                        cluster = "HYBAS_ID")

etable(mod2_order_full, mod4_order_full, drop = "soilgrid")

evi_cropland_africover_avg <- df_reg |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()

# without controls
# reduction for overall EVI
coef(mod2_order_full[[1]])["downstream"] / evi_avg * 100
pvalue(mod2_order_full[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_order_full[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod2_order_full[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_order_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod2_order_full[[2]])["downstream"]

# with controls
# reduction for overall EVI
coef(mod4_order_full[[1]])["downstream"] / evi_avg * 100
pvalue(mod4_order_full[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_order_full[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod4_order_full[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_order_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod4_order_full[[3]])["downstream"]


# Order Regression, restricted sample -------------------------------------

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

# no covariates, order interaction
# mod1_order_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
#                            as.factor(order) + downstream |
#                            year +  as.factor(mine_basin),
#                          data = df_reg_restr,
#                          cluster = "HYBAS_ID")

mod2_order_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# with covariates, order interaction
# mod3_order_restr = feols(c(max_c_EVI_af, max_c_EVI_ESA) ~
#                            as.factor(order) + downstream +
#                            elevation + slope + soilgrid_grouped +
#                            tmp_max + precipitation +
#                            accessibility_to_cities_2015 + pop_2015 |
#                            year + as.factor(mine_basin),
#                          data = df_reg_restr,
#                          cluster = "HYBAS_ID")

mod4_order_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

etable(mod2_order_restr, mod4_order_restr, drop = "soilgrid")

evi_cropland_africover_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg_restr |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()

# without controls
# reduction for overall EVI
coef(mod2_order_restr[[1]])["downstream"] / evi_avg * 100
pvalue(mod2_order_restr[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_order_restr[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod2_order_restr[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_order_restr[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod2_order_restr[[3]])["downstream"]

# with controls
# reduction for overall EVI
coef(mod4_order_restr[[1]])["downstream"] / evi_avg * 100
pvalue(mod4_order_restr[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_order_restr[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod4_order_restr[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_order_restr[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod4_order_restr[[3]])["downstream"]



# Distance Regression, full sample -------------------------------------------

# years 2001-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))

# no covariates, linear distance
# mod1_dist_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
#                          (distance) + downstream |
#                          year +  as.factor(mine_basin),
#                        data = df_reg,
#                        cluster = "HYBAS_ID")

mod2_dist_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                         (distance) * downstream |
                         year +  as.factor(mine_basin),
                       data = df_reg,
                       cluster = "HYBAS_ID")

# with covariates, linear distance
# mod3_dist_full = feols(c(max_c_EVI_af, max_c_EVI_ESA) ~
#                          (distance) + downstream +
#                          elevation + slope + soilgrid_grouped +
#                          tmp_max + precipitation +
#                          accessibility_to_cities_2015 + pop_2015 |
#                          year + as.factor(mine_basin),
#                        data = df_reg,
#                        cluster = "HYBAS_ID")

mod4_dist_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                         (distance) * downstream +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg,
                       cluster = "HYBAS_ID")

etable(mod2_dist_full, mod4_dist_full, drop = "soilgrid")

evi_cropland_africover_avg <- df_reg |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()

# without controls
# reduction for overall EVI
coef(mod2_dist_full[[1]])["downstream"] / evi_avg * 100
pvalue(mod2_dist_full[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_dist_full[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod2_dist_full[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_dist_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod2_dist_full[[3]])["downstream"]

# with controls
# reduction for overall EVI
coef(mod4_dist_full[[1]])["downstream"] / evi_avg * 100
pvalue(mod4_dist_full[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_dist_full[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod4_dist_full[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_dist_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod4_dist_full[[3]])["downstream"]


# Distance Regression, restricted sample -------------------------------------

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

# no covariates, linear distance
# mod1_dist_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
#                           (distance) + downstream |
#                           year +  as.factor(mine_basin),
#                         data = df_reg_restr,
#                         cluster = "HYBAS_ID")

mod2_dist_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# with covariates, linear distance
# mod3_dist_restr = feols(c(max_c_EVI_af, max_c_EVI_ESA) ~
#                           (distance) + downstream +
#                           elevation + slope + soilgrid_grouped +
#                           tmp_max + precipitation +
#                           accessibility_to_cities_2015 + pop_2015 |
#                           year + as.factor(mine_basin),
#                         data = df_reg_restr,
#                         cluster = "HYBAS_ID")

mod4_dist_restr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

etable(mod2_dist_restr, mod4_dist_restr, drop = "soilgrid")

evi_cropland_africover_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg_restr |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()

# without controls
# reduction for overall EVI
coef(mod2_dist_restr[[1]])["downstream"] / evi_avg * 100
pvalue(mod2_dist_restr[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_dist_restr[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod2_dist_restr[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod2_dist_restr[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod2_dist_restr[[3]])["downstream"]

# with controls
# reduction for overall EVI
coef(mod4_dist_restr[[1]])["downstream"] / evi_avg * 100
pvalue(mod4_dist_restr[[1]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_dist_restr[[2]])["downstream"] / evi_cropland_africover_avg * 100
pvalue(mod4_dist_restr[[2]])["downstream"]
# reduction for Africover cropland EVI
coef(mod4_dist_restr[[3]])["downstream"] / evi_cropland_ESA_avg * 100
pvalue(mod4_dist_restr[[3]])["downstream"]



# Robustness: Controls sequentially added ---------------------------------

#####
# Order with restricted sample

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

# no covariates
mod1_order_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# with geo covariates
mod2_order_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream +
                           elevation + slope + soilgrid_grouped|
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# with geo + met covariates
mod3_order_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# with geo + met + socio covariates
mod4_order_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

etable(mod1_order_contr, mod2_order_contr, mod3_order_contr, mod4_order_contr, 
       drop = "soilgrid")

#####
# Distance with restricted sample

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

# no covariates
mod1_dist_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# with geo covariates
mod2_dist_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# with geo + met covariates
mod3_dist_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# with geo + met + socio covariates
mod4_dist_contr = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

etable(mod1_dist_contr, mod2_dist_contr, mod3_dist_contr, mod4_dist_contr, 
       drop = "soilgrid")



# Robustness: Varying FEs sequentially added ---------------------------------

#####
# Order with restricted sample

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015) |> 
  mutate(mine_pfaf_lvl4 = substr(mine_basin_pfaf_id, 1, 4),
         mine_pfaf_lvl6 = substr(mine_basin_pfaf_id, 1, 6),
         mine_pfaf_lvl8 = substr(mine_basin_pfaf_id, 1, 8))

# baseline (mine basin FEs)
mod1_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# country FEs
mod2_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(iso3c),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# USDA region FEs
mod3_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(region),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# Basin level 4
mod4_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl4),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# Basin level 6
mod5_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl6),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# Basin level 8
mod6_order_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        as.factor(order) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl8),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

etable(mod1_order_fe[[1]], mod2_order_fe[[1]], mod3_order_fe[[1]], 
       mod4_order_fe[[1]], mod5_order_fe[[1]], mod6_order_fe[[1]],
       drop = "soilgrid|order")

etable(mod1_order_fe[[2]], mod2_order_fe[[2]], mod3_order_fe[[2]], 
       mod4_order_fe[[2]], mod5_order_fe[[2]], mod6_order_fe[[2]],
       drop = "soilgrid|order")

etable(mod1_order_fe[[3]], mod2_order_fe[[3]], mod3_order_fe[[3]], 
       mod4_order_fe[[3]], mod5_order_fe[[3]], mod6_order_fe[[3]],
       drop = "soilgrid|order")

#####
# Distance with restricted sample

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015) |> 
  mutate(mine_pfaf_lvl4 = substr(mine_basin_pfaf_id, 1, 4),
         mine_pfaf_lvl6 = substr(mine_basin_pfaf_id, 1, 6),
         mine_pfaf_lvl8 = substr(mine_basin_pfaf_id, 1, 8))

# baseline (mine basin FEs)
mod1_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_basin),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# country FEs
mod2_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(iso3c),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# USDA region FEs
mod3_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(region),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# Basin level 4
mod4_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl4),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# Basin level 6
mod5_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl6),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# Basin level 8
mod6_dist_fe = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                       (distance) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl8),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

etable(mod1_dist_fe[[1]], mod2_dist_fe[[1]], mod3_dist_fe[[1]], 
       mod4_dist_fe[[1]], mod5_dist_fe[[1]], mod6_dist_fe[[1]],
       drop = "soilgrid")

etable(mod1_dist_fe[[2]], mod2_dist_fe[[2]], mod3_dist_fe[[2]], 
       mod4_dist_fe[[2]], mod5_dist_fe[[2]], mod6_dist_fe[[2]],
       drop = "soilgrid")

etable(mod1_dist_fe[[3]], mod2_dist_fe[[3]], mod3_dist_fe[[3]], 
       mod4_dist_fe[[3]], mod5_dist_fe[[3]], mod6_dist_fe[[3]],
       drop = "soilgrid")



# Heterogeneity: By biome -------------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

#####
# Order with restricted sample
mod_order_hetero_biome = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           as.factor(order) * downstream  +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 | 
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         split = "biome_name",
                         cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_order_hetero_biome)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_order_hetero_biome)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_order_hetero_biome)))
etable(mod_order_hetero_biome[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_order_hetero_biome[pos_africover], 
       drop = "soilgrid|order")
etable(mod_order_hetero_biome[pos_ESA], 
       drop = "soilgrid|order")

#####
# Distance with restricted sample
mod_dist_hetero_biome = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                           (distance) * downstream  +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation  +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         split = "biome_name",
                         cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_dist_hetero_biome)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_dist_hetero_biome)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_dist_hetero_biome)))
etable(mod_dist_hetero_biome[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_biome[pos_africover], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_biome[pos_ESA], 
       drop = "soilgrid|order")


# Heterogeneity: By country ------------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

#####
# Order with restricted sample
mod_order_hetero_country = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                                 as.factor(order) * downstream  +
                                 elevation + slope + soilgrid_grouped +
                                 tmp_max + precipitation +
                                 accessibility_to_cities_2015 + pop_2015 | 
                                 year +  as.factor(mine_basin),
                               data = df_reg_restr,
                               split = "iso3c",
                               cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_order_hetero_country)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_order_hetero_country)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_order_hetero_country)))
etable(mod_order_hetero_country[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_order_hetero_country[pos_africover], 
       drop = "soilgrid|order")
etable(mod_order_hetero_country[pos_ESA], 
       drop = "soilgrid|order")

#####
# Distance with restricted sample
mod_dist_hetero_country = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                                (distance) * downstream  +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation  +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr,
                              split = "iso3c",
                              cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_dist_hetero_country)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_dist_hetero_country)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_dist_hetero_country)))
etable(mod_dist_hetero_country[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_country[pos_africover], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_country[pos_ESA], 
       drop = "soilgrid|order")


# Heterogeneity: By region ------------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

#####
# Order with restricted sample
mod_order_hetero_region = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                                   as.factor(order) * downstream  +
                                   elevation + slope + soilgrid_grouped +
                                   tmp_max + precipitation +
                                   accessibility_to_cities_2015 + pop_2015 | 
                                   year +  as.factor(mine_basin),
                                 data = df_reg_restr,
                                 split = "region",
                                 cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_order_hetero_region)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_order_hetero_region)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_order_hetero_region)))
etable(mod_order_hetero_region[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_order_hetero_region[pos_africover], 
       drop = "soilgrid|order")
etable(mod_order_hetero_region[pos_ESA], 
       drop = "soilgrid|order")

#####
# Distance with restricted sample
mod_dist_hetero_region = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                                  (distance) * downstream  +
                                  elevation + slope + soilgrid_grouped +
                                  tmp_max + precipitation  +
                                  accessibility_to_cities_2015 + pop_2015 |
                                  year +  as.factor(mine_basin),
                                data = df_reg_restr,
                                split = "region",
                                cluster = "HYBAS_ID")
pos_EVI <- which(grepl("max_EVI", names(mod_dist_hetero_region)))
pos_africover <- which(grepl("max_c_EVI_af", names(mod_dist_hetero_region)))
pos_ESA <- which(grepl("max_c_EVI_ESA", names(mod_dist_hetero_region)))
etable(mod_dist_hetero_region[pos_EVI], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_region[pos_africover], 
       drop = "soilgrid|order")
etable(mod_dist_hetero_region[pos_ESA], 
       drop = "soilgrid|order")


# RD plot -----------------------------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS"))
# restricting distance to 50 here, maybe change?
df_reg_restr <- df_reg |> filter(distance <= 50, year > 2015)

rd_01 <- df_reg_restr %>%
  mutate(distance = ifelse(downstream == 0, distance * (-1), distance))

# always taking polynomial of order 2 for now
rdplot(rd_01$max_EVI, rd_01$distance,
       y.lim = c(0.02,0.96),
       title = "Years 2016-2023",
       x.lab="Distance",
       y.lab="max EVI", p = 2)

rdplot(rd_01$max_c_EVI_af, rd_01$distance,
       y.lim = c(0.02,0.96),
       title = "Years 2016-2023",
       x.lab="Distance",
       y.lab="max cropland EVI (Africover)", p = 2)

rdplot(rd_01$max_c_EVI_ESA, rd_01$distance,
       y.lim = c(0.02,0.96),
       title = "Years 2016-2023",
       x.lab="Distance",
       y.lab="max cropland EVI (ESA)", p = 2)

# subset to year 2019
rd_02 <- rd_01 %>%
  filter(year == 2019)
rdplot(rd_02$max_EVI, rd_02$distance,
       y.lim = c(0.02,0.96),
       title = "Year 2019",
       x.lab="Distance",
       y.lab="max EVI", p = 2)

rdplot(rd_02$max_c_EVI_af, rd_02$distance,
       y.lim = c(0.02,0.96),
       title = "Year 2019",
       x.lab="Distance",
       y.lab="max cropland EVI (Africover)", p = 2)

rdplot(rd_02$max_c_EVI_ESA, rd_02$distance,
       y.lim = c(0.02,0.96),
       title = "Year 2019",
       x.lab="Distance",
       y.lab="max cropland EVI (ESA)", p = 2)



# Plots of ordered regressions --------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS"))
df_reg_restr <- df_reg |> filter(order <= 10, year > 2015)

# no covariates
mod1_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                     year +  as.factor(mine_basin),
                   data = df_reg_restr |> filter(order <= 1),
                   cluster = "HYBAS_ID")

mod2_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 2),
                        cluster = "HYBAS_ID")

mod3_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 3),
                        cluster = "HYBAS_ID")

mod4_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 4),
                        cluster = "HYBAS_ID")

mod5_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 5),
                        cluster = "HYBAS_ID")

mod6_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 6),
                        cluster = "HYBAS_ID")

mod7_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 7),
                        cluster = "HYBAS_ID")

mod8_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 8),
                        cluster = "HYBAS_ID")

mod9_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr |> filter(order <= 9),
                        cluster = "HYBAS_ID")

mod10_order_plot = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~ 
                           downstream |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr |> filter(order <= 10),
                         cluster = "HYBAS_ID")

coefplot(list(mod1_order_plot[[1]], mod2_order_plot[[1]], 
              mod3_order_plot[[1]], mod4_order_plot[[1]], 
              mod5_order_plot[[1]], mod6_order_plot[[1]], 
              mod7_order_plot[[1]], mod8_order_plot[[1]], 
              mod9_order_plot[[1]], mod10_order_plot[[1]]))

coefplot(list(mod1_order_plot[[2]], mod2_order_plot[[2]], 
              mod3_order_plot[[2]], mod4_order_plot[[2]], 
              mod5_order_plot[[2]], mod6_order_plot[[2]], 
              mod7_order_plot[[2]], mod8_order_plot[[2]], 
              mod9_order_plot[[2]], mod10_order_plot[[2]]))

coefplot(list(mod1_order_plot[[3]], mod2_order_plot[[3]], 
              mod3_order_plot[[3]], mod4_order_plot[[3]], 
              mod5_order_plot[[3]], mod6_order_plot[[3]], 
              mod7_order_plot[[3]], mod8_order_plot[[3]], 
              mod9_order_plot[[3]], mod10_order_plot[[3]]))
