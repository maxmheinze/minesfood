library("dplyr")
library("readr")
library("fixest")
# library("pdftools")
library("rdrobust")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

date <- "20250113"

t_folder <- "./output/tables/"
p_folder <- "./output/plots/"

f_name <- paste0("table_main_maxorder", restr_order, "_minnumber", restr_number_basins, 
                 "_EXCLmine", excl_mine_basin, "_", date) 
p_name <- paste0("plot_order_effects_maxorder", restr_order, "_minnumber", restr_number_basins, 
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

# no covariates
mod4_order_contr = feols(c(elevation, slope, tmp_max, precipitation, 
                             accessibility_to_cities_2015, pop_2015) ~
                           i(order_new, ref = -1) |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")


# Distance linear specification -------------------------------------------

mod1_dist_linear_contr = feols(c(elevation, slope, tmp_max, precipitation, 
                                 accessibility_to_cities_2015, pop_2015) ~
                                 (distance) * downstream |
                                 year +  as.factor(mine_basin),
                               data = df_reg_restr,
                               cluster = "mine_basin")



# Distance square specification -------------------------------------------

# no covariates
mod1_dist_square_contr = feols(c(elevation, slope, tmp_max, precipitation, 
                                 accessibility_to_cities_2015, pop_2015) ~
                                 (distance + I(distance^2)) * downstream |
                                 year +  as.factor(mine_basin),
                               data = df_reg_restr,
                               cluster = "mine_basin")




# Output creation ---------------------------------------------------------

setFixest_dict(dict = c(distance = "Distance",
                        "as.factor(order)" = "Order",
                        "order_new" = "Downstream x Order",
                        "as.factor(order_new)1" = "Downstream x Order $=$ 1",
                        downstream = "Downstream",
                        elevation = "Elevation",
                        slope = "Slope",
                        tmp_max = "Yearly Max. Temperature",
                        precipitation = "Yearly Precipitation",
                        accessibility_to_cities_2015 = "Accessibility in 2015",
                        pop_2015 = "Population in 2015", 
                        "I(distance^2)" = "Distance$^2$", 
                        max_EVI_16_nomask = "Maximum EVI", 
                        max_EVI_16_af_c = "Maximum Cropland EVI",
                        max_EVI_16_cci_c_broad = "ESA C EVI",
                        mean_EVI_16_nomask = "Mean EVI", 
                        mean_EVI_16_af_c = "Mean C EVI"))


etable(mod1_dist_square_contr, 
       tex = TRUE, 
       adjustbox = TRUE,
       replace = TRUE, 
       drop = "soilgrid", 
       order = c("Downstream") 
)


etable(mod1_dist_linear_contr, 
       tex = TRUE, 
       adjustbox = TRUE,
       replace = TRUE, 
       drop = "soilgrid", 
       order = c("Downstream")
)



