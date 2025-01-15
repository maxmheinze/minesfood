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

f_name <- paste0("table_robustness_y-FEs_maxorder", 
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
  mutate(order_new = ifelse(downstream == 0, -order, order), 
         mine_pfaf_lvl4 = substr(mine_basin_pfaf_id, 1, 4),
         mine_pfaf_lvl6 = substr(mine_basin_pfaf_id, 1, 6),
         mine_pfaf_lvl8 = substr(mine_basin_pfaf_id, 1, 8))

# Order specification -----------------------------------------------------

# baseline + ESA cropland
mod_order_base = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "HYBAS_ID")

# FE - Pfaffstetter level 8
mod_order_fe8 = feols(c(max_EVI, max_c_EVI_af) ~
                        i(order_new, ref = -1) +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl8),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# FE - Pfaffstetter level 6
mod_order_fe6 = feols(c(max_EVI, max_c_EVI_af) ~
                        i(order_new, ref = -1) +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl6),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# mean EVI
mod_order_mean = feols(c(mean_EVI, mean_c_EVI_af) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "HYBAS_ID")


# Distance specification --------------------------------------------------

# baseline + ESA cropland
mod_dist_base = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "HYBAS_ID")

# FE - Pfaffstetter level 8
mod_dist_fe8 = feols(c(max_EVI, max_c_EVI_af) ~
                       (distance + I(distance^2)) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl8),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# FE - Pfaffstetter level 6
mod_dist_fe6 = feols(c(max_EVI, max_c_EVI_af) ~
                       (distance + I(distance^2)) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl6),
                     data = df_reg_restr,
                     cluster = "HYBAS_ID")

# mean EVI
mod_dist_mean = feols(c(mean_EVI, mean_c_EVI_af) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
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
                        "I(distance^2)" = "Distance square", 
                        max_EVI = "Maximum EVI", 
                        max_c_EVI_af = "Maximum Cropland EVI",
                        max_c_EVI_ESA = "ESA C. EVI",
                        mean_EVI = "Mean EVI", 
                        mean_c_EVI_af = "Mean C EVI"))

etable(mod_order_base[1], 
       mod_order_fe8[1],
       mod_order_fe6[1],
       mod_order_mean[1], 
       mod_order_base[2], 
       mod_order_fe8[2],
       mod_order_fe6[2],
       mod_order_mean[2],
       mod_order_base[3],
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = TRUE)

etable(mod_dist_base[1], 
       mod_dist_fe8[1],
       mod_dist_fe6[1],
       mod_dist_mean[1], 
       mod_dist_base[2], 
       mod_dist_fe8[2],
       mod_dist_fe6[2],
       mod_dist_mean[2],
       mod_dist_base[3],
       keep = "Distance|Downstream", 
       interaction.order = "Downstream", order = c("Downstream"),
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)

