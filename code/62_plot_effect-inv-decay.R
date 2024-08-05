library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
library("ggplot2")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

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

m_evi <- feols(c(max_EVI) ~
                 elevation + slope + soilgrid_grouped +
                 tmp_max + precipitation +
                 accessibility_to_cities_2015 + pop_2015 |
                 year +  as.factor(mine_basin),
               data = df_reg_restr,
               cluster = "HYBAS_ID")

m_evi_c <- feols(c(max_c_EVI_af) ~
                   elevation + slope + soilgrid_grouped +
                   tmp_max + precipitation +
                   accessibility_to_cities_2015 + pop_2015 |
                   year +  as.factor(mine_basin),
                 data = df_reg_restr,
                 cluster = "HYBAS_ID")

df_reg_restr <- df_reg_restr |> 
  mutate(resids_evi = NA, 
         resids_evi = replace(resids_evi, 
                              !is.na(pop_2015) & !is.na(accessibility_to_cities_2015), 
                              residuals(m_evi)),
         resids_evi_c = NA, 
         resids_evi_c = replace(resids_evi_c, 
                                !is.na(pop_2015) & !is.na(accessibility_to_cities_2015) & !is.na(max_c_EVI_af),
                                residuals(m_evi_c)))

grid_inv <- seq(0.001, 1, by = 0.001)
mods <- list()
# mods[["EVI"]][["linear"]] = feols(c(resids_evi) ~
#                                       (distance) * downstream,
#                                     data = df_reg_restr,
#                                     cluster = "HYBAS_ID")
# mods[["EVI"]][["squared"]] = feols(c(resids_evi) ~
#                                        (distance + I(distance^2)) * downstream,
#                                      data = df_reg_restr,
#                                      cluster = "HYBAS_ID")
for(ee in grid_inv) {
  df_reg_restr$dist_temp <- 1 / (ee * (df_reg_restr$distance + 0.01))
  mods[["EVI"]][[paste0("inv-", ee)]] = feols(c(resids_evi) ~
                                                dist_temp : downstream,
                                              data = df_reg_restr,
                                              cluster = "HYBAS_ID")
}

evi_bic <- unlist(lapply(mods[["EVI"]], BIC))
min_evi_bic_name <- names(evi_bic[which.min(evi_bic)])

summary(mods[["EVI"]][[min_evi_bic_name]])

newdat <- data.frame(dist_temp = seq(-25, 200, by = 1), downstream = 0) |> 
  mutate(downstream = ifelse(dist_temp < 0, downstream, 1))
predictions_evi <- lapply(mods$EVI, function(x) predict(x, newdata = newdat)) |> 
  bind_cols() |> mutate(distance = -25:200) |> tidyr::pivot_longer(cols = contains("inv"))
ggplot(predictions_evi |> filter(name == min_evi_bic_name), 
       aes(x = distance, y = value)) + 
  geom_point()

evi_bic_s <- evi_bic - min(evi_bic) # Standardise for numerics
post_prob_evi <- exp(evi_bic_s / -2) / sum(exp(evi_bic_s / -2))
# plot(post_prob_evi)
# post_prob_evi[post_prob_evi > 0.01] |> plot()
lower_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .1)[1]])
median_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .5)[1]]) # Median
upper_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .9)[1]])

lower_pp_evi <- as.numeric(substr(lower_pp_evi, 5, nchar(lower_pp_evi)))
mean_pp_evi <- as.numeric(substr(min_evi_bic_name, 5, nchar(min_evi_bic_name)))
median_pp_evi <- as.numeric(substr(median_pp_evi, 5, nchar(median_pp_evi)))
upper_pp_evi <- as.numeric(substr(upper_pp_evi, 5, nchar(upper_pp_evi)))

plot(0:100, 1 / (median_pp_evi * 0:100), type = "l", xlab = "Distance", ylab = "Effect Size")
# lines(1:100, 1 / (median_pp_evi * 1:100), lty = 2)
lines(0:100, 1 / (lower_pp_evi * 0:100), lty = 3)
lines(0:100, 1 / (upper_pp_evi * 0:100), lty = 3)

# mods[["EVI_c"]][["linear"]] = feols(c(resids_evi_c) ~
#                                       (distance) * downstream,
#                                     data = df_reg_restr,
#                                     cluster = "HYBAS_ID")
# mods[["EVI_c"]][["squared"]] = feols(c(resids_evi_c) ~
#                                        (distance + I(distance^2)) * downstream,
#                                      data = df_reg_restr,
#                                      cluster = "HYBAS_ID")
for(ee in grid_inv) {
  df_reg_restr$dist_temp <- 1 / (ee * (df_reg_restr$distance + 0.01))
  mods[["EVI_c"]][[paste0("inv-", ee)]] = feols(c(resids_evi_c) ~
                                                  dist_temp : downstream,
                                                data = df_reg_restr,
                                                cluster = "HYBAS_ID")
}

evi_c_bic <- unlist(lapply(mods[["EVI_c"]], BIC))
min_evi_c_bic_name <- names(evi_c_bic[which.min(evi_c_bic)])

summary(mods[["EVI"]][[min_evi_c_bic_name]])

predictions_evi_c <- lapply(mods$EVI_c, function(x) predict(x, newdata = newdat)) |> 
  bind_cols() |> mutate(distance = -25:200) |> tidyr::pivot_longer(cols = contains("inv"))
ggplot(predictions_evi_c |> filter(name == min_evi_c_bic_name), 
       aes(x = distance, y = value)) + 
  geom_point()

evi_c_bic_s <- evi_c_bic - min(evi_c_bic) # Standardise for numerics
post_prob_evi_c <- exp(evi_c_bic_s / -2) / sum(exp(evi_c_bic_s / -2))
# plot(post_prob_evi_c)
# post_prob_evi_c[post_prob_evi_c > 0.01] |> plot()
lower_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .1)[1]])
median_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .5)[1]]) # Median
upper_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .9)[1]])

lower_pp_evi_c <- as.numeric(substr(lower_pp_evi_c, 5, nchar(lower_pp_evi_c)))
mean_pp_evi_c <- as.numeric(substr(min_evi_c_bic_name, 5, nchar(min_evi_c_bic_name)))
median_pp_evi_c <- as.numeric(substr(median_pp_evi_c, 5, nchar(median_pp_evi_c)))
upper_pp_evi_c <- as.numeric(substr(upper_pp_evi_c, 5, nchar(upper_pp_evi_c)))

plot(1:100, 1 / (median_pp_evi_c * 1:100), type = "l", xlab = "Distance", ylab = "Effect Size")
# lines(1:100, 1 / (median_pp_evi_c * 1:100), lty = 2)
lines(1:100, 1 / (lower_pp_evi_c * 1:100), lty = 3)
lines(1:100, 1 / (upper_pp_evi_c * 1:100), lty = 3)
