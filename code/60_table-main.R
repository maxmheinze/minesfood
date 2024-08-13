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

date <- "20240813"

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



# No interaction specification --------------------------------------------

# no covariates
mod1_noint_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           downstream |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo covariates
mod2_noint_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           downstream +
                           elevation + slope + soilgrid_grouped |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met covariates
mod3_noint_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met + socio covariates
mod4_noint_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")


# Order specification -----------------------------------------------------

# no covariates
mod1_order_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           i(order_new, ref = -1) |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo covariates
mod2_order_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           i(order_new, ref = -1) +
                           elevation + slope + soilgrid_grouped|
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met covariates
mod3_order_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           i(order_new, ref = -1) +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met + socio covariates
mod4_order_contr = feols(c(max_EVI, max_c_EVI_af) ~
                           i(order_new, ref = -1) +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")


# Distance linear specification -------------------------------------------

# no covariates
mod1_dist_linear_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo covariates
mod2_dist_linear_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met covariates
mod3_dist_linear_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met + socio covariates
mod4_dist_linear_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")



# Distance square specification -------------------------------------------

# no covariates
mod1_dist_square_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo covariates
mod2_dist_square_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream +
                          elevation + slope + soilgrid_grouped |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met covariates
mod3_dist_square_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met + socio covariates
mod4_dist_square_contr = feols(c(max_EVI, max_c_EVI_af) ~
                          (distance + I(distance^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
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
                        max_EVI = "Maximum EVI", 
                        max_c_EVI_af = "Maximum Cropland EVI",
                        max_c_EVI_ESA = "ESA C EVI",
                        mean_EVI = "Mean EVI", 
                        mean_c_EVI_af = "Mean C EVI"))

evi_cropland_africover_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_af)) |>
  pull(max_c_EVI_af) |> mean()
evi_cropland_ESA_avg <- df_reg_restr |>
  filter(!is.na(max_c_EVI_ESA)) |>
  pull(max_c_EVI_ESA) |> mean()
yield_cropland_avg <- df_reg_restr |>
  filter(!is.na(cropland_yield_kg_ha)) |>
  pull(cropland_yield_kg_ha) |> mean()
evi_avg <- df_reg_restr |>
  filter(!is.na(max_EVI)) |>
  pull(max_EVI) |> mean()


extra_lines <- list()
extra_lines[["^_ "]] <- rep(" ", 6)
extra_lines[["^_Sample Mean Effect"]] <- 
  c(coef(mod1_noint_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod2_noint_contr[1])[["downstream"]] / evi_avg * 100,
    # coef(mod3_noint_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod4_noint_contr[1])[["downstream"]] / evi_avg * 100, 
    coef(mod1_noint_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod2_noint_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100,
    # coef(mod3_noint_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod4_noint_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100)

etable(mod1_noint_contr[1], 
       mod2_noint_contr[1],
       # mod3_noint_contr[1], 
       mod4_noint_contr[1], 
       mod1_noint_contr[2], 
       mod2_noint_contr[2], 
       # mod3_noint_contr[2], 
       mod4_noint_contr[2], 
       drop = "soilgrid", 
       order = c("Downstream"),
       extralines = extra_lines, 
       # powerBelow = -8,
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = TRUE)




extra_lines <- list()
extra_lines[["^_ "]] <- rep(" ", 6)
extra_lines[["^_Sample Mean Effect"]] <- 
  c(coef(mod1_order_contr[1])[["order_new::0"]] / evi_avg * 100,
    coef(mod2_order_contr[1])[["order_new::0"]] / evi_avg * 100,
    # coef(mod3_order_contr[1])[["order_new::0"]] / evi_avg * 100,
    coef(mod4_order_contr[1])[["order_new::0"]] / evi_avg * 100, 
    coef(mod1_order_contr[2])[["order_new::0"]] / evi_cropland_africover_avg * 100, 
    coef(mod2_order_contr[2])[["order_new::0"]] / evi_cropland_africover_avg * 100,
    # coef(mod3_order_contr[2])[["order_new::0"]] / evi_cropland_africover_avg * 100, 
    coef(mod4_order_contr[2])[["order_new::0"]] / evi_cropland_africover_avg * 100)

etable(mod1_order_contr[1], 
       mod2_order_contr[1],
       # mod3_order_contr[1], 
       mod4_order_contr[1], 
       mod1_order_contr[2], 
       mod2_order_contr[2], 
       # mod3_order_contr[2], 
       mod4_order_contr[2], 
       drop = "soilgrid|-",
       extralines = extra_lines, 
       # powerBelow = -8,
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)



extra_lines <- list()
extra_lines[["^_ "]] <- rep(" ", 6)
extra_lines[["^_Sample Mean Effect"]] <- 
  c(coef(mod1_dist_linear_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod2_dist_linear_contr[1])[["downstream"]] / evi_avg * 100,
    # coef(mod3_dist_linear_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod4_dist_linear_contr[1])[["downstream"]] / evi_avg * 100, 
    coef(mod1_dist_linear_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod2_dist_linear_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100,
    # coef(mod3_dist_linear_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod4_dist_linear_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100)

etable(mod1_dist_linear_contr[1], 
       mod2_dist_linear_contr[1],
       # mod3_dist_linear_contr[1], 
       mod4_dist_linear_contr[1], 
       mod1_dist_linear_contr[2], 
       mod2_dist_linear_contr[2], 
       # mod3_dist_linear_contr[2],
       mod4_dist_linear_contr[2], drop = "soilgrid", 
       interaction.order = "Downstream", order = c("Downstream"), 
       extralines = extra_lines, 
       # powerBelow = -8,
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


extra_lines <- list()
extra_lines[["^_ "]] <- rep(" ", 6)
extra_lines[["^_Sample Mean Effect"]] <- 
  c(coef(mod1_dist_square_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod2_dist_square_contr[1])[["downstream"]] / evi_avg * 100,
    # coef(mod3_dist_contr[1])[["downstream"]] / evi_avg * 100,
    coef(mod4_dist_square_contr[1])[["downstream"]] / evi_avg * 100, 
    coef(mod1_dist_square_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod2_dist_square_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100,
    # coef(mod3_dist_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100, 
    coef(mod4_dist_square_contr[2])[["downstream"]] / evi_cropland_africover_avg * 100)

etable(mod1_dist_square_contr[1], 
       mod2_dist_square_contr[1],
       # mod3_dist_square_contr[1], 
       mod4_dist_square_contr[1], 
       mod1_dist_square_contr[2], 
       mod2_dist_square_contr[2], 
       # mod3_dist_square_contr[2],
       mod4_dist_square_contr[2], drop = "soilgrid", 
       interaction.order = "Downstream", order = c("Downstream"), 
       extralines = extra_lines, 
       # powerBelow = -8,
       notes = paste0("This specification includes years ", 
                      restr_year[1], " to ", restr_year[length(restr_year)], 
                      ", up to ", restr_order, " order basins away from the mined basin, ", 
                      ifelse(excl_mine_basin, "excludes", "includes"), " the mined basin, ", 
                      "and restricts the sample to include only mine basin systems with at least ", 
                      restr_number_basins, " basins up/downstream."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)



# Plots of order specification

pdf(file = paste0(p_folder, p_name, ".pdf"), 
    width = 8, height = 5,
    onefile = TRUE)
iplot(mod1_order_contr, ci_level = 0.95,
         main = "Effects by order of basins (without controls)", sub = "Black = EVI, Red = Africover EVI", 
         dict = c("order_new" = "Order5"))
iplot(mod4_order_contr, ci_level = 0.95,
         main = "Effects by order of basins (with controls)", sub = "Black = EVI, Red = Africover EVI", 
         dict = c("order_new" = "Order"))
dev.off()
