
# Header ------------------------------------------------------------------

rm(list = ls())

pacman::p_load(
  dplyr,
  rddtools, 
  countrycode,
  readr, 
  fixest
)


# Read in Data ------------------------------------------------------------
df_reg <- readRDS("/data/jde/processed/df_reg.RDS")


# Downstream Regression, full sample -------------------------------------------

# years 2001-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 10 basins "away"

# no covariates, linear distance
mod1_full = feols((max_cropland_EVI) ~ 
                    (distance) + downstream | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod2_full = feols((max_cropland_EVI) ~ 
                    (distance) * downstream | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod3_full = feols((max_EVI) ~ 
                    (distance) + downstream  | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod4_full = feols((max_EVI) ~ 
                    (distance) * downstream  | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

etable(mod1_full, mod2_full, mod3_full, mod4_full)

evi_cropland_avg <- df_reg |> 
  filter(!is.na(max_cropland_EVI)) |> 
  pull(max_cropland_EVI) |> mean()
evi_avg <- df_reg |> 
  filter(!is.na(max_EVI)) |> 
  pull(max_EVI) |> mean()

# 1.28% reduction for cropland EVI
coef(mod2_full)["downstream"] / evi_cropland_avg * 100
# 0.61% reduction for overall EVI
coef(mod4_full)["downstream"] / evi_avg * 100


# with covariates, linear distance
mod5_full = feols((max_cropland_EVI) ~ 
                    (distance) + downstream + 
                    elevation + slope + soilq_avg +
                    tmp_max + precipitation | 
                    year + as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod6_full = feols((max_cropland_EVI) ~ 
                    (distance) * downstream + 
                    elevation + slope + soilq_avg +
                    tmp_max + precipitation | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod7_full = feols((max_EVI) ~ 
                    (distance) + downstream + 
                    elevation + slope + soilq_avg +
                    tmp_max + precipitation | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

mod8_full = feols((max_EVI) ~ 
                    distance * downstream + 
                    elevation + slope + soilq_avg +
                    tmp_max + precipitation | 
                    year +  as.factor(mine_basin), 
                  data = df_reg, 
                  cluster = "HYBAS_ID")

etable(mod5_full, mod6_full, mod7_full, mod8_full)

# 1.25% reduction for cropland EVI
coef(mod6_full)["downstream"] / evi_cropland_avg * 100
# 0.31% reduction for overall EVI
coef(mod8_full)["downstream"] / evi_avg * 100



# Downstream Regression, restricted sample -------------------------------------

df_reg_restr <- df_reg |> filter(order <= 5, year > 2015)

# years 2016-2023
# all mined basins regardless of area mined
# all up/downstream basins up to 5 basins "away"

# no covariates, linear distance
mod1_restr = feols((max_cropland_EVI) ~ (distance) + downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod2_restr = feols((max_cropland_EVI) ~ (distance) * downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod3_restr = feols((max_EVI) ~ (distance) + downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod4_restr = feols((max_EVI) ~ (distance) * downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

etable(mod1_restr, mod2_restr, mod3_restr, mod4_restr)

evi_cropland_avg <- df_reg_restr |> 
  filter(!is.na(max_cropland_EVI)) |> 
  pull(max_cropland_EVI) |> mean()
evi_avg <- df_reg_restr |> 
  filter(!is.na(max_EVI)) |> 
  pull(max_EVI) |> mean()

# 2.90% reduction for cropland EVI
coef(mod2_restr)["downstream"] / evi_cropland_avg * 100
# 3.21% reduction for overall EVI
coef(mod2_restr)["downstream"] / evi_avg * 100


# with covariates, linear distance
mod5_restr = feols((max_cropland_EVI) ~ 
                     distance + downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year + as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod6_restr = feols((max_cropland_EVI) ~ 
                     distance * downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod7_restr = feols((max_EVI) ~ 
                     distance + downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

mod8_restr = feols((max_EVI) ~ 
                     distance * downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr, 
                   cluster = "HYBAS_ID")

etable(mod5_restr, mod6_restr, mod7_restr, mod8_restr)

# 2.88% reduction for cropland EVI
coef(mod6_restr)["downstream"] / evi_cropland_avg * 100
# 0.20% reduction for overall EVI
coef(mod8_restr)["downstream"] / evi_avg * 100

# Downstream Regression, large mines only --------------------------------------

basins_large_mines <- df_reg |> filter(mine_area_km2 > 1) |> pull(mine_basin) |> unique()
df_reg_large_mines <- df_reg |> filter(mine_basin %in% basins_large_mines)

# years 2001-2023
# only mined basins with mine_area > 1km^2
# all up/downstream basins up to 10 basins "away"

# no covariates, linear distance
mod1_large = feols((max_cropland_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod2_large = feols((max_cropland_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod3_large = feols((max_EVI) ~ (distance) + downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod4_large = feols((max_EVI) ~ (distance) * downstream | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

etable(mod1_large, mod2_large, mod3_large, mod4_large)

evi_cropland_avg <- df_reg_large_mines |> 
  filter(!is.na(max_cropland_EVI)) |> 
  pull(max_cropland_EVI) |> mean()
evi_avg <- df_reg_large_mines |> 
  filter(!is.na(max_EVI)) |> 
  pull(max_EVI) |> mean()

# 2.53% reduction for cropland EVI
coef(mod2_large)["downstream"] / evi_cropland_avg * 100
# 1.47% reduction for overall EVI
coef(mod4_large)["downstream"] / evi_avg * 100



# with covariates, linear distance
mod5_large = feols((max_cropland_EVI) ~ 
               distance + downstream + 
               elevation + slope + soilq_avg +
               tmp_max + precipitation | 
               year + as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod6_large = feols((max_cropland_EVI) ~ 
               distance * downstream + 
               elevation + slope + soilq_avg +
               tmp_max + precipitation | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod7_large = feols((max_EVI) ~ 
               distance + downstream + 
               elevation + slope + soilq_avg +
               tmp_max + precipitation | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

mod8_large = feols((max_EVI) ~ 
               distance * downstream + 
               elevation + slope + soilq_avg +
               tmp_max + precipitation | 
               year +  as.factor(mine_basin), 
             data = df_reg_large_mines, 
             cluster = "HYBAS_ID")

etable(mod5_large, mod6_large, mod7_large, mod8_large)

# 2.54% reduction for cropland EVI
coef(mod6_large)["downstream"] / evi_cropland_avg * 100
# 1.51% reduction for overall EVI
coef(mod8_large)["downstream"] / evi_avg * 100



# Downstream Regression, restricted sample with large mines only ---------------

df_reg_restr_large <- df_reg_large_mines |> filter(order <= 5, year > 2015)

# years 2016-2023
# only mined basins with mine_area > 1km^2
# all up/downstream basins up to 5 basins "away"

# no covariates, linear distance
mod1_restr_large = feols((max_cropland_EVI) ~ (distance) + downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod2_restr_large = feols((max_cropland_EVI) ~ (distance) * downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod3_restr_large = feols((max_EVI) ~ (distance) + downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod4_restr_large = feols((max_EVI) ~ (distance) * downstream | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

etable(mod1_restr_large, mod2_restr_large, mod3_restr_large, mod4_restr_large)

evi_cropland_avg <- df_reg_restr_large |> 
  filter(!is.na(max_cropland_EVI)) |> 
  pull(max_cropland_EVI) |> mean()
evi_avg <- df_reg_restr_large |> 
  filter(!is.na(max_EVI)) |> 
  pull(max_EVI) |> mean()

# 5.16% reduction for cropland EVI
coef(mod2_restr_large)["downstream"] / evi_cropland_avg * 100
# 2.67% reduction for overall EVI
coef(mod4_restr_large)["downstream"] / evi_avg * 100


# with covariates, linear distance
mod5_restr_large = feols((max_cropland_EVI) ~ 
                     distance + downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year + as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod6_restr_large = feols((max_cropland_EVI) ~ 
                     distance * downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod7_restr_large = feols((max_EVI) ~ 
                     distance + downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

mod8_restr_large = feols((max_EVI) ~ 
                     distance * downstream + 
                     elevation + slope + soilq_avg +
                     tmp_max + precipitation | 
                     year +  as.factor(mine_basin), 
                   data = df_reg_restr_large, 
                   cluster = "HYBAS_ID")

etable(mod5_restr_large, mod6_restr_large, mod7_restr_large, mod8_restr_large)

# 5.26% reduction for cropland EVI
coef(mod6_restr_large)["downstream"] / evi_cropland_avg * 100
# 2.73% reduction for overall EVI
coef(mod8_restr_large)["downstream"] / evi_avg * 100
