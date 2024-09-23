library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})


date <- "20240813"

t_folder <- "./output/tables/"
p_folder <- "./output/plots/"


# Varying Outcome and Fixed Effects ---------------------------------------

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

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

#####
# Order specification

# baseline + ESA cropland
mod_order_base = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "mine_basin")

# FE - Pfaffstetter level 8
mod_order_fe8 = feols(c(max_EVI, max_c_EVI_af) ~
                        i(order_new, ref = -1) +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl8),
                      data = df_reg_restr,
                      cluster = "mine_basin")

# FE - Pfaffstetter level 6
mod_order_fe6 = feols(c(max_EVI, max_c_EVI_af) ~
                        i(order_new, ref = -1) +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_pfaf_lvl6),
                      data = df_reg_restr,
                      cluster = "mine_basin")

# mean EVI
mod_order_mean = feols(c(mean_EVI, mean_c_EVI_af) ~
                         i(order_new, ref = -1) +
                         elevation + slope + soilgrid_grouped +
                         tmp_max + precipitation +
                         accessibility_to_cities_2015 + pop_2015 |
                         year +  as.factor(mine_basin),
                       data = df_reg_restr,
                       cluster = "mine_basin")


#####
# Distance specification

# baseline + ESA cropland
mod_dist_base = feols(c(max_EVI, max_c_EVI_af, max_c_EVI_ESA) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "mine_basin")

# FE - Pfaffstetter level 8
mod_dist_fe8 = feols(c(max_EVI, max_c_EVI_af) ~
                       (distance + I(distance^2)) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl8),
                     data = df_reg_restr,
                     cluster = "mine_basin")

# FE - Pfaffstetter level 6
mod_dist_fe6 = feols(c(max_EVI, max_c_EVI_af) ~
                       (distance + I(distance^2)) * downstream +
                       elevation + slope + soilgrid_grouped +
                       tmp_max + precipitation +
                       accessibility_to_cities_2015 + pop_2015 |
                       year +  as.factor(mine_pfaf_lvl6),
                     data = df_reg_restr,
                     cluster = "mine_basin")

# mean EVI
mod_dist_mean = feols(c(mean_EVI, mean_c_EVI_af) ~
                        (distance + I(distance^2)) * downstream +
                        elevation + slope + soilgrid_grouped +
                        tmp_max + precipitation +
                        accessibility_to_cities_2015 + pop_2015 |
                        year +  as.factor(mine_basin),
                      data = df_reg_restr,
                      cluster = "mine_basin")


#####
# Output creation

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
                      restr_number_basins, " basins up/downstream.  ", 
                      "Models (1) and (5) are the baseline models for overall maximum EVI and the cropland-specific maximum EVI, respectively, with mine fixed effects. Models (2) and (6) use fixed effects at Pfaffstetter level 8 basins, models (3) and (7) fixed effects at Pfaffstetter level 6 basins. Models (4) and (8) report results for the yearly mean of the overall EVI and the cropland-specific EVI instead of the maximum, respectively. Model (9) reports result for the cropland-specific EVI based on the time-varying cropland mask retreived from \\cite{cci2024} instead of the time-invariant of \\cite{DigitalEarthAfrica2022}."),
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
                      restr_number_basins, " basins up/downstream.  ", 
                      "Models (1) and (5) are the baseline models for overall maximum EVI and the cropland-specific maximum EVI, respectively, with mine fixed effects. Models (2) and (6) use fixed effects at Pfaffstetter level 8 basins, models (3) and (7) fixed effects at Pfaffstetter level 6 basins. Models (4) and (8) report results for the yearly mean of the overall EVI and the cropland-specific EVI instead of the maximum, respectively. Model (9) reports result for the cropland-specific EVI based on the time-varying cropland mask retreived from \\cite{cci2024} instead of the time-invariant of \\cite{DigitalEarthAfrica2022}."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)



# Subsetting basins -------------------------------------------------------

f_name <- paste0("table_robustness_order_subsets_", date) 

restr_year <- 2016:2023
df_reg <- readRDS(p("processed/df_reg.RDS"))
mine_size_restr <- df_reg |> filter(mine_area_km2 > restr_area_mined) |> pull(mine_basin) |> unique()
df_reg_restr <- df_reg |> 
  filter(year %in% restr_year) |> 
  mutate(order_new = ifelse(downstream == 0, -order, order))

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
                                      cluster = "mine_basin")

mod_dist_restr_number_basins = feols(c(max_EVI, max_c_EVI_af) ~
                                       (distance + I(distance^2)) * downstream +
                                       elevation + slope + soilgrid_grouped +
                                       tmp_max + precipitation +
                                       accessibility_to_cities_2015 + pop_2015 |
                                       year +  as.factor(mine_basin),
                                     data = df_reg_restr_number_basins,
                                     cluster = "mine_basin")

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
                              cluster = "mine_basin")

mod_dist_restr_order = feols(c(max_EVI, max_c_EVI_af) ~
                               (distance + I(distance^2)) * downstream +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr_order,
                             cluster = "mine_basin")

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
                              cluster = "mine_basin")

mod_dist_restr_mine_basin = feols(c(max_EVI, max_c_EVI_af) ~
                               (distance + I(distance^2)) * downstream +
                               elevation + slope + soilgrid_grouped +
                               tmp_max + precipitation +
                               accessibility_to_cities_2015 + pop_2015 |
                               year +  as.factor(mine_basin),
                             data = df_reg_restr_mine_basin,
                             cluster = "mine_basin")

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
                             cluster = "mine_basin")

mod_dist_restr_comb = feols(c(max_EVI, max_c_EVI_af) ~
                              (distance + I(distance^2)) * downstream +
                              elevation + slope + soilgrid_grouped +
                              tmp_max + precipitation +
                              accessibility_to_cities_2015 + pop_2015 |
                              year +  as.factor(mine_basin),
                            data = df_reg_restr_comb,
                            cluster = "mine_basin")

#####
# Output creation

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
       drop = "soilgrid|-", keep = " 0|1$|2$|3$",
       notes = paste0("Models (1) and (6) are the baseline models, models (2) and (7) only include basin systems with at least one up- and downstream basin, models (3) and (8) include maximum order one up- and downstream basins, models (4) and (9) exclude the mine basin itself, models (5) and (10) include only basins systems with one basin up- and downstream and excludes the mine basin."),
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
       notes = paste0("Models (1) and (6) are the baseline models, models (2) and (7) only include basin systems with at least one up- and downstream basin, models (3) and (8) include maximum order one up- and downstream basins, models (4) and (9) exclude the mine basin itself, models (5) and (10) include only basins systems with one basin up- and downstream and excludes the mine basin."),
       adjustbox = TRUE,
       file = paste0(t_folder, f_name, ".tex"), replace = FALSE)


# Plotting robustness results ---------------------------------------------

library(broom)
library(ggplot2)

p_name <- paste0("plot_robustness_", date) 

names_mods <- c("Baseline", 
                "At least one basin\n up/downstream", 
                "Maximum order of 1", 
                "Excluding mine basin", 
                "Maximum order of 1 &\n at least one up/downstream &\nexcluding mine basin", 
                "FE: Basin level 8", "FE: Basin level 6", 
                "Mean instead of Max") # add ESA for croplands EVI below

# Order
mod_order_evi_list <- list(mod_order_base[[1]], 
                           mod_order_restr_number_basins[[1]], 
                           mod_order_restr_order[[1]], 
                           mod_order_restr_mine_basin[[1]], 
                           mod_order_restr_comb[[1]],
                           mod_order_fe8[[1]],
                           mod_order_fe6[[1]],
                           mod_order_mean[[1]])

list_tidy_mod_order_evi <- lapply(mod_order_evi_list, tidy, conf.int = T, conf.level = 0.95)
list_tidy_mod_order_evi[[4]] <- list_tidy_mod_order_evi[[4]] |> 
  mutate(term = replace(term, term == "order_new::1", "order_new::0"))
list_tidy_mod_order_evi[[5]] <- list_tidy_mod_order_evi[[5]] |> 
  mutate(term = replace(term, term == "as.factor(order_new)1", "order_new::0"))
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
  labs(x = "Estimate and 95% Conf. Int.", y = "", title = "Order Interaction: EVI") +
  theme_bw()


mod_order_evi_c_list <- list(mod_order_base[[2]], 
                           mod_order_restr_number_basins[[2]], 
                           mod_order_restr_order[[2]], 
                           mod_order_restr_mine_basin[[2]], 
                           mod_order_restr_comb[[2]],
                           mod_order_fe8[[2]],
                           mod_order_fe6[[2]],
                           mod_order_mean[[2]], 
                           mod_order_base[[3]])

list_tidy_mod_order_evi_c <- lapply(mod_order_evi_c_list, tidy, conf.int = T, conf.level = 0.95)
list_tidy_mod_order_evi_c[[4]] <- list_tidy_mod_order_evi_c[[4]] |> 
  mutate(term = replace(term, term == "order_new::1", "order_new::0"))
list_tidy_mod_order_evi_c[[5]] <- list_tidy_mod_order_evi_c[[5]] |> 
  mutate(term = replace(term, term == "as.factor(order_new)1", "order_new::0"))
df_tidy_mod_order_evi_c <- bind_rows(list_tidy_mod_order_evi_c) |> 
  filter(term == "order_new::0") |> 
  mutate(term = c(names_mods, "ESA cropland mask"), 
         mod = "Order: EVI croplands", 
         term = factor(term, 
                       levels = c(names_mods, 
                                  "ESA cropland mask")))

p_mod_order_evi_c <- ggplot(df_tidy_mod_order_evi_c, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 95% Conf. Int.", y = "", title = "Order Interaction: EVI") +
  theme_bw()


# Distance
mod_dist_evi_list <- list(mod_dist_base[[1]], 
                           mod_dist_restr_number_basins[[1]], 
                           mod_dist_restr_order[[1]], 
                           mod_dist_restr_mine_basin[[1]], 
                           mod_dist_restr_comb[[1]],
                           mod_dist_fe8[[1]],
                           mod_dist_fe6[[1]],
                           mod_dist_mean[[1]])

list_tidy_mod_dist_evi <- lapply(mod_dist_evi_list, tidy, conf.int = T, conf.level = 0.95)
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
  labs(x = "Estimate and 95% Conf. Int.", y = "", title = "Distance Interaction: EVI") +
  theme_bw()

mod_dist_evi_c_list <- list(mod_dist_base[[2]], 
                             mod_dist_restr_number_basins[[2]], 
                             mod_dist_restr_order[[2]], 
                             mod_dist_restr_mine_basin[[2]], 
                             mod_dist_restr_comb[[2]],
                             mod_dist_fe8[[2]],
                             mod_dist_fe6[[2]],
                             mod_dist_mean[[2]], 
                             mod_dist_base[[3]])

list_tidy_mod_dist_evi_c <- lapply(mod_dist_evi_c_list, tidy, conf.int = T, conf.level = 0.95)
df_tidy_mod_dist_evi_c <- bind_rows(list_tidy_mod_dist_evi_c) |> 
  filter(term == "downstream") |> 
  mutate(term = c(names_mods, "ESA cropland mask"), 
         mod = "Distance: EVI croplands", 
         term = factor(term, 
                       levels = c(names_mods, 
                                  "ESA cropland mask")))

p_mod_dist_evi_c <- ggplot(df_tidy_mod_dist_evi_c, aes(estimate, term)) +
  geom_point()  +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Estimate and 95% Conf. Int.", y = "", title = "Order Interaction: EVI") +
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
  labs(x = "Estimate and 95% Conf. Int.", y = "") +
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
  labs(x = "Estimate and 95% Conf. Int.", y = "") +
  theme_bw()

pdf(paste0(p_folder, p_name, "_comb_order.pdf"), width = 10, height = 6)
p_effects_comb_order
dev.off()






