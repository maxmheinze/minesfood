library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

date <- "20240802"

t_folder <- "./output/tables/"
p_folder <- "./output/plots/"

f_name <- paste0("table_heterogeneity_maxorder", 
                 restr_order, "_minnumber", restr_number_basins, 
                 "_EXCLmine", excl_mine_basin, "_", date) 
p_name <- paste0("plot_heterogeneity_maxorder", 
                 restr_order, "_minnumber", restr_number_basins, 
                 "_EXCLmine", excl_mine_basin, "_", date) 

df_reg <- readRDS(p("processed/df_reg.RDS"))
mine_size_restr <- df_reg |> filter(mine_area_km2 > restr_area_mined) |> pull(mine_basin) |> unique()

df_reg_restr <- df_reg |> 
  filter(order <= restr_order, 
         year %in% restr_year, 
         mine_basin %in% mine_size_restr, 
         if(excl_mine_basin) order > 0 else order >= 0) 
if(!mine_downstream) {
  df_reg_restr <- df_reg_restr |> 
    mutate(downstream = replace(downstream, order == 0, 0))
}
if(restr_number_basins > 0) {
  mine_number_restr <- df_reg |> filter(year == restr_year[1], order != 0) |> 
    group_by(mine_basin, downstream, order) |> count() |> 
    group_by(mine_basin, downstream) |> count() |> 
    filter(n >= restr_number_basins) |> 
    group_by(mine_basin) |> count() |> 
    filter(n == 2) |> 
    pull(mine_basin)
  df_reg_restr <- df_reg_restr |> 
    filter(mine_basin %in% mine_number_restr)
}

df_reg_restr <- df_reg_restr |> 
  mutate(order_new = ifelse(downstream == 0, -order, order))


# Order specification -----------------------------------------------------

# baseline
mod_order_base = feols(c(max_EVI, max_c_EVI_af) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "HYBAS_ID")

# region
mod_order_region = feols(c(max_EVI, max_c_EVI_af) ~
                           i(order_new, ref = -1) +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         split = "region_grouped",
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# biome
mod_order_biome = feols(c(max_EVI, max_c_EVI_af) ~
                          i(order_new, ref = -1) +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        split = "biome_group",
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# country
mod_order_country = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          split = "iso3c",
                          data = df_reg_restr,
                          cluster = "HYBAS_ID")

# mine size
mine_size_restr <- df_reg |> filter(mine_area_km2 > 0.5) |> pull(mine_basin) |> unique()
mod_order_size0.5 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                          cluster = "HYBAS_ID")
mine_size_restr <- df_reg |> filter(mine_area_km2 > 1) |> pull(mine_basin) |> unique()
mod_order_size1 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                          cluster = "HYBAS_ID")
mine_size_restr <- df_reg |> filter(mine_area_km2 > 2.5) |> pull(mine_basin) |> unique()
mod_order_size2.5 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                          cluster = "HYBAS_ID")

# mine activity 
id_active_mines <- readRDS("/data/jde/processed/filter_active_mines.RDS")

id_active_abs_2016 <- id_active_mines |> filter(active_abs_2016 == 1) |> pull(mine_basin)
id_active_abs_2017 <- id_active_mines |> filter(active_abs_2017 == 1) |> pull(mine_basin)
id_active_rel0.05_2016 <- id_active_mines |> filter(active_rel0.05_2016 == 1) |> pull(mine_basin)
id_active_rel0.05_2017 <- id_active_mines |> filter(active_rel0.05_2017 == 1) |> pull(mine_basin)
id_active_rel0.1_2016 <- id_active_mines |> filter(active_rel0.1_2016 == 1) |> pull(mine_basin)
id_active_rel0.1_2017 <- id_active_mines |> filter(active_rel0.1_2017 == 1) |> pull(mine_basin)
id_active_rel0.25_2016 <- id_active_mines |> filter(active_rel0.25_2016 == 1) |> pull(mine_basin)
id_active_rel0.25_2017 <- id_active_mines |> filter(active_rel0.25_2017 == 1) |> pull(mine_basin)

mod_order_act_abs2016 = feols(c(max_EVI, max_c_EVI_af) ~
                            i(order_new, ref = -1) +
                            elevation + slope + soilgrid_grouped +
                            tmp_max + precipitation +
                            accessibility_to_cities_2015 + pop_2015 |
                            year +  as.factor(mine_basin),
                          data = df_reg_restr |> filter(mine_basin %in% id_active_abs_2016),
                          cluster = "HYBAS_ID")
mod_order_act_rel0.05_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                     i(order_new, ref = -1) +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.05_2016),
                                   cluster = "HYBAS_ID")
mod_order_act_rel0.1_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                     i(order_new, ref = -1) +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.1_2016),
                                   cluster = "HYBAS_ID")
mod_order_act_rel0.25_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                     i(order_new, ref = -1) +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.25_2016),
                                   cluster = "HYBAS_ID")

mod_order_act_abs2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                i(order_new, ref = -1) +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr |> filter(mine_basin %in% id_active_abs_2017),
                              cluster = "HYBAS_ID")
mod_order_act_rel0.05_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                     i(order_new, ref = -1) +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.05_2017),
                                   cluster = "HYBAS_ID")
mod_order_act_rel0.1_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                    i(order_new, ref = -1) +
                                    elevation + slope + soilgrid_grouped +
                                    tmp_max + precipitation +
                                    accessibility_to_cities_2015 + pop_2015 |
                                    year +  as.factor(mine_basin),
                                  data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.1_2017),
                                  cluster = "HYBAS_ID")
mod_order_act_rel0.25_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                     i(order_new, ref = -1) +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.25_2017),
                                   cluster = "HYBAS_ID")


# Distance specification --------------------------------------------------

# baseline
mod_dist_base = feols(c(max_EVI, max_c_EVI_af) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# region
mod_dist_region = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        split = "region_grouped",
                        data = df_reg_restr,
                        cluster = "HYBAS_ID")

# biome
mod_dist_biome = feols(c(max_EVI, max_c_EVI_af) ~
                         (distance + I(distance^2)) * downstream +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       split = "biome_group",
                       data = df_reg_restr,
                       cluster = "HYBAS_ID")

# region
mod_dist_country = feols(c(max_EVI, max_c_EVI_af) ~
                           (distance + I(distance^2)) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         split = "iso3c",
                         data = df_reg_restr,
                         cluster = "HYBAS_ID")

# mine size
mine_size_restr <- df_reg |> filter(mine_area_km2 > 0.5) |> pull(mine_basin) |> unique()
mod_dist_size0.5 = feols(c(max_EVI, max_c_EVI_af) ~
                           (distance + I(distance^2)) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin), 
                         data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                         cluster = "HYBAS_ID")
mine_size_restr <- df_reg |> filter(mine_area_km2 > 1) |> pull(mine_basin) |> unique()
mod_dist_size1 = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin), 
                        data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                        cluster = "HYBAS_ID")
mine_size_restr <- df_reg |> filter(mine_area_km2 > 2.5) |> pull(mine_basin) |> unique()
mod_dist_size2.5 = feols(c(max_EVI, max_c_EVI_af) ~
                           (distance + I(distance^2)) * downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin), 
                         data = df_reg_restr |> filter(mine_basin %in% mine_size_restr),
                         cluster = "HYBAS_ID")

# mine activity 
id_active_mines <- readRDS("/data/jde/processed/filter_active_mines.RDS")

id_active_abs_2016 <- id_active_mines |> filter(active_abs_2016 == 1) |> pull(mine_basin)
id_active_abs_2017 <- id_active_mines |> filter(active_abs_2017 == 1) |> pull(mine_basin)
id_active_rel0.05_2016 <- id_active_mines |> filter(active_rel0.05_2016 == 1) |> pull(mine_basin)
id_active_rel0.05_2017 <- id_active_mines |> filter(active_rel0.05_2017 == 1) |> pull(mine_basin)
id_active_rel0.1_2016 <- id_active_mines |> filter(active_rel0.1_2016 == 1) |> pull(mine_basin)
id_active_rel0.1_2017 <- id_active_mines |> filter(active_rel0.1_2017 == 1) |> pull(mine_basin)
id_active_rel0.25_2016 <- id_active_mines |> filter(active_rel0.25_2016 == 1) |> pull(mine_basin)
id_active_rel0.25_2017 <- id_active_mines |> filter(active_rel0.25_2017 == 1) |> pull(mine_basin)

mod_dist_act_abs2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                (distance + I(distance^2)) * downstream +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr |> filter(mine_basin %in% id_active_abs_2016),
                              cluster = "HYBAS_ID")
mod_dist_act_rel0.05_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                     (distance + I(distance^2)) * downstream +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.05_2016),
                                   cluster = "HYBAS_ID")
mod_dist_act_rel0.1_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                    (distance + I(distance^2)) * downstream +
                                    elevation + slope + soilgrid_grouped +
                                    tmp_max + precipitation +
                                    accessibility_to_cities_2015 + pop_2015 |
                                    year +  as.factor(mine_basin),
                                  data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.1_2016),
                                  cluster = "HYBAS_ID")
mod_dist_act_rel0.25_2016 = feols(c(max_EVI, max_c_EVI_af) ~
                                     (distance + I(distance^2)) * downstream +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.25_2016),
                                   cluster = "HYBAS_ID")

mod_dist_act_abs2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                (distance + I(distance^2)) * downstream +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr |> filter(mine_basin %in% id_active_abs_2017),
                              cluster = "HYBAS_ID")
mod_dist_act_rel0.05_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                     (distance + I(distance^2)) * downstream +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.05_2017),
                                   cluster = "HYBAS_ID")
mod_dist_act_rel0.1_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                    (distance + I(distance^2)) * downstream +
                                    elevation + slope + soilgrid_grouped +
                                    tmp_max + precipitation +
                                    accessibility_to_cities_2015 + pop_2015 |
                                    year +  as.factor(mine_basin),
                                  data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.1_2017),
                                  cluster = "HYBAS_ID")
mod_dist_act_rel0.25_2017 = feols(c(max_EVI, max_c_EVI_af) ~
                                     (distance + I(distance^2)) * downstream +
                                     elevation + slope + soilgrid_grouped +
                                     tmp_max + precipitation +
                                     accessibility_to_cities_2015 + pop_2015 |
                                     year +  as.factor(mine_basin),
                                   data = df_reg_restr |> filter(mine_basin %in% id_active_rel0.25_2017),
                                   cluster = "HYBAS_ID")


# Output creation ---------------------------------------------------------

setFixest_dict(dict = c(distance = "Distance",
                        "as.factor(order)" = "Order",
                        "order_new" = "Downstream x Order",
                        downstream = "Downstream",
                        elevation = "Elevation",
                        slope = "Slope",
                        tmp_max = "Yearly Max. Temperature",
                        precipitation = "Yearly Precipitation",
                        accessibility_to_cities_2015 = "Accessibility in 2015",
                        pop_2015 = "Population in 2015", 
                        "I(distance^2)" = "Distance$^2$", 
                        max_EVI = "Maximum EVI", 
                        max_c_EVI_af = "Maximum Cropland EVI"))

# Order

# overall EVI
etable(mod_order_base[1], 
       mod_order_size0.5[1], mod_order_size1[1], mod_order_size2.5[1],
       mod_order_act_abs2017[1], mod_order_act_rel0.1_2017[1], mod_order_act_rel0.25_2017[1],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$", 
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by mine area in the mined basin, and models (5)-(7) are samples with any, more than 10 percent and more than 25 percent increase in mining area between 2017 and 2023, respectively. All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = TRUE)

pos_evi_biome <- which(grepl("max_EVI", names(mod_order_biome)))
pos_evi_region <- which(grepl("max_EVI", names(mod_order_region)))
etable(mod_order_base[1], 
       mod_order_biome[pos_evi_biome],
       mod_order_region[pos_evi_region],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by biome, and models (5)-(7) are sample splits by regions (USDA crop classifications). All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


pos_cN <- which(grepl("ZAF|GHA|NAM|ZWE|TZA|COD", names(mod_order_country)))
pos_cN_evi <- which(grepl("max_EVI", names(mod_order_country)[pos_cN]))
pos_cN_evi <- pos_cN[pos_cN_evi]
etable(mod_order_base[1], 
       mod_order_country[pos_cN_evi],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(7) are countries with the most mining basins within them."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


# cropland EVI
etable(mod_order_base[2], 
       mod_order_size0.5[2], mod_order_size1[2], mod_order_size2.5[2],
       mod_order_act_abs2017[2], mod_order_act_rel0.1_2017[2], mod_order_act_rel0.25_2017[2],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$", 
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by mine area in the mined basin, and models (5)-(7) are samples with any, more than 10 percent and more than 25 percent increase in mining area between 2017 and 2023, respectively. All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

pos_evi_c_biome <- which(grepl("max_c_EVI_af", names(mod_order_biome)))
pos_evi_c_region <- which(grepl("max_c_EVI_af", names(mod_order_region)))
etable(mod_order_base[2], 
       mod_order_biome[pos_evi_c_biome],
       mod_order_region[pos_evi_c_region],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by biome, and models (5)-(7) are sample splits by regions (USDA crop classifications). All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


pos_cN_c <- which(grepl("ZAF|GHA|NAM|ZWE|TZA|COD", names(mod_order_country)))
pos_cN_evi_c <- which(grepl("max_c_EVI_af", names(mod_order_country)[pos_cN]))
pos_cN_evi_c <- pos_cN_c[pos_cN_evi_c]
etable(mod_order_base[2], 
       mod_order_country[pos_cN_evi_c],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(7) are countries with the most mining basins within them."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


# Distance

# overall EVI
etable(mod_dist_base[1], 
       mod_dist_size0.5[1], mod_dist_size1[1], mod_dist_size2.5[1],
       mod_dist_act_abs2017[1], mod_dist_act_rel0.1_2017[1], mod_dist_act_rel0.25_2017[1],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by mine area in the mined basin, and models (5)-(7) are samples with any, more than 10 percent and more than 25 percent increase in mining area between 2017 and 2023, respectively. All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

pos_evi_biome <- which(grepl("max_EVI", names(mod_dist_biome)))
pos_evi_region <- which(grepl("max_EVI", names(mod_dist_region)))
etable(mod_dist_base[1], 
       mod_dist_biome[pos_evi_biome],
       mod_dist_region[pos_evi_region],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by biome, and models (5)-(7) are sample splits by regions (USDA crop classifications). All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

pos_cN <- which(grepl("ZAF|GHA|NAM|ZWE|TZA|COD", names(mod_order_country)))
pos_cN_evi <- which(grepl("max_EVI", names(mod_order_country)[pos_cN]))
pos_cN_evi <- pos_cN[pos_cN_evi]
etable(mod_dist_base[1], 
       mod_dist_country[pos_cN_evi],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(7) are countries with the most mining basins within them."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


# Cropland EVI

etable(mod_dist_base[2], 
       mod_dist_size0.5[2], mod_dist_size1[2], mod_dist_size2.5[2],
       mod_dist_act_abs2017[2], mod_dist_act_rel0.1_2017[2], mod_dist_act_rel0.25_2017[2],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by mine area in the mined basin, and models (5)-(7) are samples with any, more than 10 percent and more than 25 percent increase in mining area between 2017 and 2023, respectively. All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

pos_evi_c_biome <- which(grepl("max_c_EVI_af", names(mod_dist_biome)))
pos_evi_c_region <- which(grepl("max_c_EVI_af", names(mod_dist_region)))
etable(mod_dist_base[2], 
       mod_dist_biome[pos_evi_c_biome],
       mod_dist_region[pos_evi_c_region],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(4) are sample splits by biome, and models (5)-(7) are sample splits by regions (USDA crop classifications). All specifications include the full set of controls."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

pos_cN_c <- which(grepl("ZAF|GHA|NAM|ZWE|TZA|COD", names(mod_order_country)))
pos_cN_evi_c <- which(grepl("max_c_EVI_af", names(mod_order_country)[pos_cN_c]))
pos_cN_evi_c <- pos_cN_c[pos_cN_evi_c]
etable(mod_dist_base[2], 
       mod_dist_country[pos_cN_evi_c],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream.", 
                      "Model (1) is the baseline specification, models (2)-(7) are countries with the most mining basins within them."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)




# Plotting heterogeneity results ------------------------------------------

library(broom)
library(ggplot2)

names_mods <- c("Baseline", 
                "Mine: Size > 0.5km^2", "Mine: Size > 1km^2", "Mine: Size > 2.5km^2", 
                "Mine: Growth > 0%", "Mine: Growth > 10%", "Mine: Growth > 25%", 
                "Biome: Deserts", "Biome: Forest", "Biome: Grasslands", 
                "Region: North & East Africa", "Region: Southern Africa", "Region: West Africa")

# Order
mod_order_evi_list <- list(mod_order_base[[1]], 
                           mod_order_size0.5[[1]],
                           mod_order_size1[[1]], 
                           mod_order_size2.5[[1]],
                           mod_order_act_abs2017[[1]], 
                           mod_order_act_rel0.1_2017[[1]],
                           mod_order_act_rel0.25_2017[[1]],
                           mod_order_biome[[pos_evi_biome[1]]], 
                           mod_order_biome[[pos_evi_biome[2]]], 
                           mod_order_biome[[pos_evi_biome[3]]],
                           mod_order_region[[pos_evi_region[1]]],
                           mod_order_region[[pos_evi_region[2]]],
                           mod_order_region[[pos_evi_region[3]]])
list_tidy_mod_order_evi <- lapply(mod_order_evi_list, tidy, conf.int = T, conf.level = 0.90)
df_tidy_mod_order_evi <- bind_rows(list_tidy_mod_order_evi) |> 
  filter(term == "order_new::0") |> 
  mutate(term = names_mods, 
         mod = "Order: EVI", 
         term = factor(term, 
                       levels = names_mods))

p_mod_order_evi <- ggplot(df_tidy_mod_order_evi, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 90% Conf. Int.", y = "", title = "Order Interaction: EVI") +
  theme_bw()

mod_order_evi_c_list <- list(mod_order_base[[2]], 
                             mod_order_size0.5[[2]],
                             mod_order_size1[[2]], 
                             mod_order_size2.5[[2]],
                             mod_order_act_abs2017[[2]], 
                             mod_order_act_rel0.1_2017[[2]],
                             mod_order_act_rel0.25_2017[[2]],
                             mod_order_biome[[pos_evi_c_biome[1]]], 
                             mod_order_biome[[pos_evi_c_biome[2]]], 
                             mod_order_biome[[pos_evi_c_biome[3]]],
                             mod_order_region[[pos_evi_c_region[1]]],
                             mod_order_region[[pos_evi_c_region[2]]],
                             mod_order_region[[pos_evi_c_region[3]]])
list_tidy_mod_order_evi_c <- lapply(mod_order_evi_c_list, tidy, conf.int = T, conf.level = 0.90)
df_tidy_mod_order_evi_c <- bind_rows(list_tidy_mod_order_evi_c) |> 
  filter(term == "order_new::0") |> 
  mutate(term = names_mods, 
         mod = "Order: EVI croplands", 
         term = factor(term, 
                       levels = names_mods))

p_mod_order_evi_c <- ggplot(df_tidy_mod_order_evi_c, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 90% Conf. Int.", y = "", title = "Order Interaction: EVI Croplands") +
  theme_bw()

# Distance
mod_dist_evi_list <- list(mod_dist_base[[1]], 
                           mod_dist_size0.5[[1]],
                           mod_dist_size1[[1]], 
                           mod_dist_size2.5[[1]],
                           mod_dist_act_abs2017[[1]], 
                           mod_dist_act_rel0.1_2017[[1]],
                           mod_dist_act_rel0.25_2017[[1]],
                           mod_dist_biome[[pos_evi_biome[1]]], 
                           mod_dist_biome[[pos_evi_biome[2]]], 
                           mod_dist_biome[[pos_evi_biome[3]]],
                           mod_dist_region[[pos_evi_region[1]]],
                           mod_dist_region[[pos_evi_region[2]]],
                           mod_dist_region[[pos_evi_region[3]]])
list_tidy_mod_dist_evi <- lapply(mod_dist_evi_list, tidy, conf.int = T, conf.level = 0.90)
df_tidy_mod_dist_evi <- bind_rows(list_tidy_mod_dist_evi) |> 
  filter(term == "downstream") |> 
  mutate(term = names_mods, 
         mod = "Distance: EVI", 
         term = factor(term, 
                       levels = names_mods))

p_mod_dist_evi <- ggplot(df_tidy_mod_dist_evi, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 90% Conf. Int.", y = "", title = "Distance Interaction: EVI") +
  theme_bw()

mod_dist_evi_c_list <- list(mod_dist_base[[2]], 
                             mod_dist_size0.5[[2]],
                             mod_dist_size1[[2]], 
                             mod_dist_size2.5[[2]],
                             mod_dist_act_abs2017[[2]], 
                             mod_dist_act_rel0.1_2017[[2]],
                             mod_dist_act_rel0.25_2017[[2]],
                             mod_dist_biome[[pos_evi_c_biome[1]]], 
                             mod_dist_biome[[pos_evi_c_biome[2]]], 
                             mod_dist_biome[[pos_evi_c_biome[3]]],
                             mod_dist_region[[pos_evi_c_region[1]]],
                             mod_dist_region[[pos_evi_c_region[2]]],
                             mod_dist_region[[pos_evi_c_region[3]]])
list_tidy_mod_dist_evi_c <- lapply(mod_dist_evi_c_list, tidy, conf.int = T, conf.level = 0.90)
df_tidy_mod_dist_evi_c <- bind_rows(list_tidy_mod_dist_evi_c) |> 
  filter(term == "downstream") |> 
  mutate(mod = "Distance: EVI croplands", 
         term = names_mods, 
         term = factor(term, 
                       levels = names_mods))

p_mod_dist_evi_c <- ggplot(df_tidy_mod_dist_evi_c, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 90% Conf. Int.", y = "", title = "Order Interaction: EVI Croplands") +
  theme_bw()

pdf(paste0(p_folder, p_name, ".pdf"), width = 10, height = 12)
cowplot::plot_grid(p_mod_order_evi, p_mod_order_evi_c, 
                   p_mod_dist_evi, p_mod_dist_evi_c, ncol = 2)
dev.off()


# Combining plots in one
df_tidy_mod_comb <- rbind(df_tidy_mod_order_evi, df_tidy_mod_order_evi_c, 
                          df_tidy_mod_dist_evi, df_tidy_mod_dist_evi_c) |> 
  mutate(mod = factor(mod, levels = c("Order: EVI", 
                                      "Order: EVI croplands", 
                                      "Distance: EVI", 
                                      "Distance: EVI croplands")))

p_effects_comb <- ggplot(df_tidy_mod_comb, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(.~mod, scales = "free_x") +
  labs(x = "Estimate and 90% Conf. Int.", y = "") +
  theme_bw()

pdf(paste0(p_folder, p_name, "_comb.pdf"), width = 10, height = 12)
p_effects_comb
dev.off()

df_tidy_mod_comb_order <- rbind(df_tidy_mod_order_evi, df_tidy_mod_order_evi_c) |> 
  mutate(mod = factor(mod, levels = c("Order: EVI", 
                                      "Order: EVI croplands"), 
                      labels = c("Dependent Variable: EVI", 
                                 "Dependent Variable: EVI croplands")))

p_effects_comb_order <- ggplot(df_tidy_mod_comb_order, 
                               aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(.~mod, scales = "free_x") +
  labs(x = "Estimate and 90% Conf. Int.", y = "") +
  theme_bw()

pdf(paste0(p_folder, p_name, "_comb_order.pdf"), width = 10, height = 6)
p_effects_comb_order
dev.off()




