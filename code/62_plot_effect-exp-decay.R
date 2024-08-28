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

date <- "20240828"

p_folder <- "./output/plots/"
p_name <- "plot_effect_decay_exp"


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
           cluster = "mine_basin")

m_evi_c <- feols(c(max_c_EVI_af) ~
                 elevation + slope + soilgrid_grouped +
                 tmp_max + precipitation +
                 accessibility_to_cities_2015 + pop_2015 |
                 year +  as.factor(mine_basin),
               data = df_reg_restr,
               cluster = "mine_basin")

df_reg_restr <- df_reg_restr |> 
  mutate(resids_evi = NA, 
         resids_evi = replace(resids_evi, 
                              !is.na(pop_2015) & !is.na(accessibility_to_cities_2015), 
                              residuals(m_evi)),
         resids_evi_c = NA, 
         resids_evi_c = replace(resids_evi_c, 
                                !is.na(pop_2015) & !is.na(accessibility_to_cities_2015) & !is.na(max_c_EVI_af),
                                residuals(m_evi_c)))

grid_exp <- seq(0.001, 2, by = 0.001)
mods <- list()

for(ee in grid_exp) {
  mods[["EVI"]][[paste0("exp-", ee)]] = feols(c(resids_evi) ~
                                                exp(-ee * distance) : downstream,
                                              data = df_reg_restr,
                                              cluster = "mine_basin", 
                                              notes = FALSE)
}

evi_bic <- unlist(lapply(mods[["EVI"]], BIC))
min_evi_bic_name <- names(evi_bic[which.min(evi_bic)])

evi_bic_s <- evi_bic - min(evi_bic) # Standardise for numerics
post_prob_evi <- exp(evi_bic_s / -2) / sum(exp(evi_bic_s / -2))
plot(post_prob_evi)
post_prob_evi[post_prob_evi > 0.01] |> plot()
lower_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .05)[1]])
median_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .5)[1]]) # Median
upper_pp_evi <- names(post_prob_evi[which(cumsum(post_prob_evi) > .95)[1]])

lower_pp_evi <- as.numeric(substr(lower_pp_evi, 5, nchar(lower_pp_evi)))
mean_pp_evi <- as.numeric(substr(min_evi_bic_name, 5, nchar(min_evi_bic_name)))
median_pp_evi <- as.numeric(substr(median_pp_evi, 5, nchar(median_pp_evi)))
upper_pp_evi <- as.numeric(substr(upper_pp_evi, 5, nchar(upper_pp_evi)))

# newdat <- data.frame(distance = seq(-25, 200, by = 1), downstream = 0) |> 
#   mutate(downstream = ifelse(distance < 0, downstream, 1))
# predictions_evi <- lapply(mods$EVI, function(x) predict(x, newdata = newdat)) |> 
#   bind_cols() |> mutate(distance = -25:200) |> tidyr::pivot_longer(cols = contains("exp"))
# ggplot(predictions_evi |> filter(name == min_evi_bic_name), aes(x = distance, y = value)) + 
#   geom_point()

for(ee in grid_exp) {
  mods[["EVI_c"]][[paste0("exp-", ee)]] = feols(c(resids_evi_c) ~
                                                  exp(-ee * distance) : downstream,
                                                data = df_reg_restr,
                                                cluster = "mine_basin", 
                                                notes = FALSE)
}

evi_c_bic <- unlist(lapply(mods[["EVI_c"]], BIC))
min_evi_c_bic_name <- names(evi_c_bic[which.min(evi_c_bic)])

evi_c_bic_s <- evi_c_bic - min(evi_c_bic) # Standardise for numerics
post_prob_evi_c <- exp(evi_c_bic_s / -2) / sum(exp(evi_c_bic_s / -2))
plot(post_prob_evi_c)
post_prob_evi_c[post_prob_evi_c > 0.01] |> plot()
lower_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .05)[1]])
median_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .5)[1]]) # Median
upper_pp_evi_c <- names(post_prob_evi_c[which(cumsum(post_prob_evi_c) > .95)[1]])

lower_pp_evi_c <- as.numeric(substr(lower_pp_evi_c, 5, nchar(lower_pp_evi_c)))
mean_pp_evi_c <- as.numeric(substr(min_evi_c_bic_name, 5, nchar(min_evi_c_bic_name)))
median_pp_evi_c <- as.numeric(substr(median_pp_evi_c, 5, nchar(median_pp_evi_c)))
upper_pp_evi_c <- as.numeric(substr(upper_pp_evi_c, 5, nchar(upper_pp_evi_c)))

# predictions_evi_c <- lapply(mods$EVI_c, function(x) predict(x, newdata = newdat)) |> 
#   bind_cols() |> mutate(distance = -25:200) |> tidyr::pivot_longer(cols = contains("exp"))
# ggplot(predictions_evi_c |> filter(name == min_evi_c_bic_name), aes(x = distance, y = value)) + 
#   geom_point()


pdf(paste0(p_folder, p_name, "_", date, ".pdf"), width = 10, height = 5)
par(mfrow = c(1, 2), oma = c(0, 0, 1, 0)) 

plot(0:500, exp(-mean_pp_evi * 0:500), type = "l", xlab = "Distance", 
     ylab = "Rel. Effect Size (EVI)", ylim = c(0, 1), xlim = c(0, 250))
polygon(c(0:500, 500:0), c(exp(-lower_pp_evi * 0:500), rev(exp(-upper_pp_evi * 0:500))), 
        col = rgb(0, 0, 0, 0.1), border = NA)
lines(0:500, exp(-median_pp_evi * 0:500), lty = 2)
lines(0:500, exp(-lower_pp_evi * 0:500), lty = 3)
lines(0:500, exp(-upper_pp_evi * 0:500), lty = 3)
abline(v = which(exp(-mean_pp_evi * 0:500) < 0.5)[1], lty = 2, col = "blue")
abline(v = which(exp(-mean_pp_evi * 0:500) < 0.1)[1], lty = 2, col = "red")

plot(0:500, exp(-mean_pp_evi_c * 0:500), type = "l", xlab = "Distance", 
     ylab = "Rel. Effect Size (EVI croplands)", ylim = c(0, 1), xlim = c(0, 250))
polygon(c(0:500, 500:0), c(exp(-lower_pp_evi_c * 0:500), rev(exp(-upper_pp_evi_c * 0:500))), 
        col = rgb(0, 0, 0, 0.1), border = NA)
lines(0:500, exp(-median_pp_evi_c * 0:500), lty = 2)
lines(0:500, exp(-lower_pp_evi_c * 0:500), lty = 3)
lines(0:500, exp(-upper_pp_evi_c * 0:500), lty = 3)
abline(v = which(exp(-mean_pp_evi_c * 0:500) < 0.5)[1], lty = 2, col = "blue")
abline(v = which(exp(-mean_pp_evi_c * 0:500) < 0.1)[1], lty = 2, col = "red")

mtext("Effect decay (exponential)", outer = TRUE, cex = 1.5, line = -2, font = 2)

dev.off()



