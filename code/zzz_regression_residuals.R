# Distance Regression, large mines only --------------------------------------

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
                     elevation + slope + soilgrid_grouped +
                     tmp_max + precipitation |
                     year + as.factor(mine_basin),
                   data = df_reg_large_mines,
                   cluster = "HYBAS_ID")

mod6_large = feols((max_cropland_EVI) ~
                     distance * downstream +
                     elevation + slope + soilgrid_grouped +
                     tmp_max + precipitation |
                     year +  as.factor(mine_basin),
                   data = df_reg_large_mines,
                   cluster = "HYBAS_ID")

mod7_large = feols((max_EVI) ~
                     distance + downstream +
                     elevation + slope + soilgrid_grouped +
                     tmp_max + precipitation |
                     year +  as.factor(mine_basin),
                   data = df_reg_large_mines,
                   cluster = "HYBAS_ID")

mod8_large = feols((max_EVI) ~
                     distance * downstream +
                     elevation + slope + soilgrid_grouped +
                     tmp_max + precipitation |
                     year +  as.factor(mine_basin),
                   data = df_reg_large_mines,
                   cluster = "HYBAS_ID")

etable(mod5_large, mod6_large, mod7_large, mod8_large)

# 2.54% reduction for cropland EVI
coef(mod6_large)["downstream"] / evi_cropland_avg * 100
# 1.51% reduction for overall EVI
coef(mod8_large)["downstream"] / evi_avg * 100



# Distance Regression, restricted sample with large mines only ---------------

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
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year + as.factor(mine_basin),
                         data = df_reg_restr_large,
                         cluster = "HYBAS_ID")

mod6_restr_large = feols((max_cropland_EVI) ~
                           distance * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr_large,
                         cluster = "HYBAS_ID")

mod7_restr_large = feols((max_EVI) ~
                           distance + downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr_large,
                         cluster = "HYBAS_ID")

mod8_restr_large = feols((max_EVI) ~
                           distance * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr_large,
                         cluster = "HYBAS_ID")

etable(mod5_restr_large, mod6_restr_large, mod7_restr_large, mod8_restr_large)

# 5.26% reduction for cropland EVI
coef(mod6_restr_large)["downstream"] / evi_cropland_avg * 100
# 2.73% reduction for overall EVI
coef(mod8_restr_large)["downstream"] / evi_avg * 100




# add regressions with avg mine size and number ----------------------------



mine_avgsize_restr <- df_reg |> filter(mine_avg_area_km2 > 0.5) |> pull(mine_basin) |> unique()
mod_order_avgsize0.5 = feols(c(max_EVI, max_c_EVI_af) ~
                               i(order_new, ref = -1) +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr |> filter(mine_basin %in% mine_avgsize_restr),
                             cluster = "mine_basin")
mine_avgsize_restr <- df_reg |> filter(mine_avg_area_km2 > 1) |> pull(mine_basin) |> unique()
mod_order_avgsize1 = feols(c(max_EVI, max_c_EVI_af) ~
                             i(order_new, ref = -1) +
                             elevation + slope + soilgrid_grouped +
                             tmp_max + precipitation +
                             accessibility_to_cities_2015 + pop_2015 |
                             year +  as.factor(mine_basin),
                           data = df_reg_restr |> filter(mine_basin %in% mine_avgsize_restr),
                           cluster = "mine_basin")
mine_avgsize_restr <- df_reg |> filter(mine_avg_area_km2 > 2.5) |> pull(mine_basin) |> unique()
mod_order_avgsize2.5 = feols(c(max_EVI, max_c_EVI_af) ~
                               i(order_new, ref = -1) +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr |> filter(mine_basin %in% mine_avgsize_restr),
                             cluster = "mine_basin")



med_number <- df_reg_restr |> filter(year == 2019, distance_centroid == 0) |> pull(mine_number) |> median()
mean_number <- df_reg_restr |> filter(year == 2019, distance_centroid == 0) |> pull(mine_number) |> mean()

med_number <- df_reg_restr |> 
  filter(year == 2019, distance_centroid == 0) |> 
  pull(mine_number) |> 
  quantile(p = c(0.1, 0.5, 0.9))


mine_number_restr <- df_reg |> filter(mine_number > 1) |> pull(mine_basin) |> unique()
mod_order_number1 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_number_restr),
                          cluster = "mine_basin")
mine_number_restr <- df_reg |> filter(mine_number > 2) |> pull(mine_basin) |> unique()
mod_order_number2 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_number_restr),
                          cluster = "mine_basin")
mine_number_restr <- df_reg |> filter(mine_number > 5) |> pull(mine_basin) |> unique()
mod_order_number5 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_number_restr),
                          cluster = "mine_basin")

etable(mod_order_base[1], 
       mod_order_size0.5[1], 
       mod_order_size1[1], 
       mod_order_size2.5[1], 
       mod_order_avgsize0.5[1], 
       mod_order_avgsize1[1], 
       mod_order_avgsize2.5[1],
       mod_order_number1[1], 
       mod_order_number2[1], 
       mod_order_number5[1],
       keep = "order")






# RMD code of full specifications -----------------------------------------

### Full sample

This specification includes:
  
  * years from **`r min(df_reg$year)` to `r max(df_reg$year)`**
  * basin systems with area mined of at least **0 square km**
  * up/downstream basins up to **`r max(df_reg$order)`** basins "away"

```{r reg_main_noint_full, warning=FALSE, message=FALSE}
# no covariates
# mod1_noint_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
#                           downstream |
#                           year +  as.factor(mine_basin),
#                         data = df_reg,
#                         cluster = "HYBAS_ID")

mod2_noint_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg,
                        cluster = "HYBAS_ID")

# with covariates
# mod3_noint_full = feols(c(max_c_EVI_af, max_c_EVI_ESA) ~
#                           downstream +
#                           elevation + slope + soilgrid_grouped +
#                           tmp_max + precipitation +
#                           accessibility_to_cities_2015 + pop_2015 |
#                           year + as.factor(mine_basin),
#                         data = df_reg,
#                         cluster = "HYBAS_ID")

mod4_noint_full = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                          downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg,
                        cluster = "HYBAS_ID")

etable(mod2_noint_full[1], mod4_noint_full[1], 
       mod2_noint_full[2], mod4_noint_full[2], 
       mod2_noint_full[3], mod4_noint_full[3], 
       drop = "soilgrid|order")

evi_cropland_africover_avg <- df_reg |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()
```

Compared to the sample mean, the presence of a mine upstream changes

* the overall EVI by **`r round(coef(mod2_noint_full[[1]])["downstream"] / evi_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_noint_full[[1]])["downstream"], 3)`* (`r round(coef(mod4_noint_full[[1]])["downstream"] / evi_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_noint_full[[1]])["downstream"], 3)`*)
* the croplands EVI (based on Africover mask) by **`r round(coef(mod2_noint_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_noint_full[[2]])["downstream"], 3)`* (`r round(coef(mod4_noint_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_noint_full[[2]])["downstream"], 3)`*)
* the croplands EVI (based on ESA mask) by **`r round(coef(mod2_noint_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_noint_full[[3]])["downstream"], 3)`* (`r round(coef(mod4_noint_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_noint_full[[3]])["downstream"], 3)`*)

on average **without** (with) controls.


### Full sample

This specification includes:
  
  * years from **`r min(df_reg$year)` to `r max(df_reg$year)`**
  * basin systems with area mined of at least **0 square km**
  * up/downstream basins up to **`r max(df_reg$order)`** basins "away"

```{r reg_main_order_full, warning=FALSE, message=FALSE}
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

etable(mod2_order_full[1], mod4_order_full[1], 
       mod2_order_full[2], mod4_order_full[2], 
       mod2_order_full[3], mod4_order_full[3], 
       drop = "soilgrid|order")

evi_cropland_africover_avg <- df_reg |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()
```

Compared to the sample mean, the presence of a mine upstream changes

* the overall EVI by **`r round(coef(mod2_order_full[[1]])["downstream"] / evi_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_order_full[[1]])["downstream"], 3)`* (`r round(coef(mod4_order_full[[1]])["downstream"] / evi_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_order_full[[1]])["downstream"], 3)`*)
* the croplands EVI (based on Africover mask) by **`r round(coef(mod2_order_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_order_full[[2]])["downstream"], 3)`* (`r round(coef(mod4_order_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_order_full[[2]])["downstream"], 3)`*)
* the croplands EVI (based on ESA mask) by **`r round(coef(mod2_order_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_order_full[[3]])["downstream"], 3)`* (`r round(coef(mod4_order_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_order_full[[3]])["downstream"], 3)`*)

on average **without** (with) controls.




### Full sample

This specification includes:
  
  * years from **`r min(df_reg$year)` to `r max(df_reg$year)`**
  * basin systems with area mined of at least **0 square km**
  * up/downstream basins up to **`r max(df_reg$order)`** basins "away"

```{r reg_main_dist_full, warning=FALSE, message=FALSE}
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

etable(mod2_dist_full[1], mod4_dist_full[1],
       mod2_dist_full[2], mod4_dist_full[2],
       mod2_dist_full[3], mod4_dist_full[3], 
       drop = "soilgrid|order")

evi_cropland_africover_avg <- df_reg |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
evi_avg <- df_reg |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()
```

Compared to the sample mean, the presence of a mine upstream changes

* the overall EVI by **`r round(coef(mod2_dist_full[[1]])["downstream"] / evi_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_dist_full[[1]])["downstream"], 3)`* (`r round(coef(mod4_dist_full[[1]])["downstream"] / evi_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_dist_full[[1]])["downstream"], 3)`*)
* the croplands EVI (based on Africover mask) by **`r round(coef(mod2_dist_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_dist_full[[2]])["downstream"], 3)`* (`r round(coef(mod4_dist_full[[2]])["downstream"] / evi_cropland_africover_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_dist_full[[2]])["downstream"], 3)`*)
* the croplands EVI (based on ESA mask) by **`r round(coef(mod2_dist_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%**, p-value = *`r round(pvalue(mod2_dist_full[[3]])["downstream"], 3)`* (`r round(coef(mod4_dist_full[[3]])["downstream"] / evi_cropland_ESA_avg * 100, 2)`%, p-value = *`r round(pvalue(mod4_dist_full[[3]])["downstream"], 3)`*)

on average **without** (with) controls.


