library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})

date <- "20240802"

t_folder <- "./output/tables/"

f_name <- paste0("robustness_table_order_subsets_", date) 

restr_year <- 2016:2023
df_reg <- readRDS(p("processed/df_reg.RDS"))
mine_size_restr <- df_reg |> filter(mine_area_km2 > restr_area_mined) |> pull(mine_basin) |> unique()
df_reg_restr <- df_reg |> 
  filter(year %in% restr_year) |> 
  mutate(order_new = ifelse(downstream == 0, -order, order))

# baseline specification
mod_order_base = feols(c(max_EVI, max_c_EVI_af) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "HYBAS_ID")

mod_dist_base = feols(c(max_EVI, max_c_EVI_af) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# restricting sample to basins with at least one up/downstream 
restr_number_basins <- 1
mine_number_restr <- df_reg |> filter(year == restr_year[1], order != 0) |> 
  group_by(mine_basin, downstream, order) |> count() |> 
  group_by(mine_basin, downstream) |> count() |> 
  filter(n >= restr_number_basins) |> 
  group_by(mine_basin) |> count() |> 
  filter(n == 2) |> 
  pull(mine_basin)
df_reg_restr_number_basins <- df_reg_restr |> 
  filter(mine_basin %in% mine_number_restr)
mod_order_restr_number_basins = feols(c(max_EVI, max_c_EVI_af) ~
                                        i(order_new, ref = -1) +
                                        elevation + slope + soilgrid_grouped +
                                        tmp_max + precipitation +
                                        accessibility_to_cities_2015 + pop_2015 |
                                        year +  as.factor(mine_basin),
                                      data = df_reg_restr_number_basins,
                                      cluster = "HYBAS_ID")

mod_dist_restr_number_basins = feols(c(max_EVI, max_c_EVI_af) ~
                                       (distance + I(distance^2)) * downstream +
                                       elevation + slope + soilgrid_grouped +
                                       tmp_max + precipitation +
                                       accessibility_to_cities_2015 + pop_2015 |
                                       year +  as.factor(mine_basin),
                                     data = df_reg_restr_number_basins,
                                     cluster = "HYBAS_ID")

# including only order 1
restr_order <- 1
df_reg_restr_order <- df_reg_restr |> 
  filter(order <= restr_order) 
mod_order_restr_order = feols(c(max_EVI, max_c_EVI_af) ~
                                i(order_new, ref = -1) +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr_order,
                              cluster = "HYBAS_ID")

mod_dist_restr_order = feols(c(max_EVI, max_c_EVI_af) ~
                               (distance + I(distance^2)) * downstream +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr_order,
                             cluster = "HYBAS_ID")

# exluding the mine basin itself
df_reg_restr_mine_basin <- df_reg_restr |> 
  filter(order != 0)
mod_order_restr_mine_basin = feols(c(max_EVI, max_c_EVI_af) ~
                                i(order_new, ref = -1) +
                                elevation + slope + soilgrid_grouped +
                                tmp_max + precipitation +
                                accessibility_to_cities_2015 + pop_2015 |
                                year +  as.factor(mine_basin),
                              data = df_reg_restr_mine_basin,
                              cluster = "HYBAS_ID")

mod_dist_restr_mine_basin = feols(c(max_EVI, max_c_EVI_af) ~
                               (distance + I(distance^2)) * downstream +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr_mine_basin,
                             cluster = "HYBAS_ID")

# including exactly order -1 and 1
df_reg_restr_comb <- df_reg_restr_number_basins |> 
  filter(order == 1)
mod_order_restr_comb = feols(c(max_EVI, max_c_EVI_af) ~
                               as.factor(order_new) +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr_comb,
                             cluster = "HYBAS_ID")

mod_dist_restr_comb = feols(c(max_EVI, max_c_EVI_af) ~
                              (distance + I(distance^2)) * downstream +
                              elevation + slope + soilgrid_grouped +
                              tmp_max + precipitation +
                              accessibility_to_cities_2015 + pop_2015 |
                              year +  as.factor(mine_basin),
                            data = df_reg_restr_comb,
                            cluster = "HYBAS_ID")

setFixest_dict(dict = c(distance = "Distance",
                        "as.factor(order)" = "Order",
                        "as.factor(order_new)1" = "Downstream x Order $=$ 1",
                        "order_new" = "Downstream x Order",
                        downstream = "Downstream",
                        elevation = "Elevation",
                        slope = "Slope",
                        tmp_max = "Yearly Max. Temperature",
                        precipitation = "Yearly Precipitation",
                        accessibility_to_cities_2015 = "Accessibility in 2015",
                        pop_2015 = "Population in 2015", 
                        "I(distance^2)" = "Distance square", 
                        max_EVI = "Maximum EVI", 
                        max_c_EVI_af = "Maximum Cropland EVI"))


etable(mod_order_base[1], 
       mod_order_restr_number_basins[1], 
       mod_order_restr_order[1], 
       mod_order_restr_mine_basin[1], 
       mod_order_restr_comb[1],
       mod_order_base[2], 
       mod_order_restr_number_basins[2], 
       mod_order_restr_order[2], 
       mod_order_restr_mine_basin[2], 
       mod_order_restr_comb[2],
       keep = "Order", drop = "-",
       notes = paste0("Model YY contains asdasda"),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = TRUE)

etable(mod_dist_base[1], 
       mod_dist_restr_number_basins[1], 
       mod_dist_restr_order[1], 
       mod_dist_restr_mine_basin[1], 
       mod_dist_restr_comb[1],
       mod_dist_base[2], 
       mod_dist_restr_number_basins[2], 
       mod_dist_restr_order[2], 
       mod_dist_restr_mine_basin[2], 
       mod_dist_restr_comb[2],
       keep = "Distance|Downstream",
       interaction.order = "Downstream", order = c("Downstream"), 
       notes = paste0("Model YY contains asdasda"),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


