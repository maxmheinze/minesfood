############################################################################################
######### This code implements Robus regressions using rdrobust and RDHonest       ######### 
############################################################################################ 
library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
library("rdd")
library("rddensity")
library("rddtools")
library("modelsummary")
library("foreign")
library("rdlocrand")
# remotes::install_github("kolesarm/RDHonest")
library("RDHonest")

# For summarising rdrobust models: https://stackoverflow.com/que --------

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[, 1]
  )
  row.names(ret) <- NULL
  ret
}

# # For summarising rdrobust models: https://stackoverflow.com/que --------

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Kernel = model$kernel,
    Bandwidth = model$bwselect,
    Polynomial = ifelse(model$p == 1, "Linear", ifelse(model$p == 2, "Quadratic", "OTHER")),
    Observations = sum(model$N_h)
  )
  ret
}

options("modelsummary_format_numeric_latex" = "plain")


sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 500 # maximum order of basins to include
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

# recoding variables to cateogrical variables for fixed effects
df_reg_restr <- df_reg_restr %>%
  mutate(mine_basin = as.factor(mine_basin)) %>%
  mutate(year = as.factor(year)) 

# recoding running variable to zero when basin lies upstream
df_reg_restr$order <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$order * -1, df_reg_restr$order)
df_reg_restr$distance <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$distance * -1, df_reg_restr$distance)
df_reg_restr$distance_centroid <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$distance_centroid * -1, df_reg_restr$distance_centroid)

m1 <- feols(max_EVI ~ 1 | 
              mine_basin + year, df_reg_restr)

m2 <- feols(max_EVI ~ elevation + slope + soilgrid_grouped + 
              tmp_max + precipitation + 
              accessibility_to_cities_2015 + pop_2015 | 
              mine_basin + year, df_reg_restr)

m3 <- feols(max_c_EVI_af ~ 1 | 
              mine_basin + year, df_reg_restr)

m4 <- feols(max_c_EVI_af ~ elevation + slope + soilgrid_grouped + 
              tmp_max + precipitation + 
              accessibility_to_cities_2015 + pop_2015 | 
              mine_basin + year, df_reg_restr)

df_reg_restr <- df_reg_restr |> 
  mutate(resids_evi_no_contr = residuals(m1), 
         resids_evi_with_contr = NA, 
         resids_evi_with_contr = replace(resids_evi_with_contr, 
                                         !is.na(pop_2015) & !is.na(accessibility_to_cities_2015), 
                                         residuals(m2)),
         resids_afri_evi_no_contr = NA, 
         resids_afri_evi_no_contr = replace(resids_afri_evi_no_contr, 
                                            !is.na(max_c_EVI_af), 
                                            residuals(m3)), 
         resids_afri_evi_with_contr = NA, 
         resids_afri_evi_with_contr = replace(resids_afri_evi_with_contr, 
                                            !is.na(pop_2015) & 
                                              !is.na(accessibility_to_cities_2015) & 
                                              !is.na(max_c_EVI_af), 
                                            residuals(m4)))


# Distance specification using rdrobust -----------------------------------

# specifications without controls
rdd1_dist_no_contr <- rdrobust(y = df_reg_restr$resids_evi_no_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd2_dist_no_contr <- rdrobust(y = df_reg_restr$resids_evi_no_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               covs = cov_add, 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd3_dist_no_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_no_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd4_dist_no_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_no_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

models_distance_no_contr <- list("Max EVI" = rdd1_dist_no_contr , "Max EVI" = rdd2_dist_no_contr,
                                 "Cropland EVI" = rdd3_dist_no_contr, "Cropland EVI" = rdd4_dist_no_contr)

modelsummary(models_distance_no_contr, 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 


# specifications with full controls
rdd1_dist_with_contr <- rdrobust(y = df_reg_restr$resids_evi_with_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd2_dist_with_contr <- rdrobust(y = df_reg_restr$resids_evi_with_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               covs = cov_add, 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd3_dist_with_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_with_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd4_dist_with_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_with_contr, 
                               x = df_reg_restr$distance, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

models_distance_with_contr <- list("Max EVI" = rdd1_dist_with_contr , "Max EVI" = rdd2_dist_with_contr,
                                 "Cropland EVI" = rdd3_dist_with_contr, "Cropland EVI" = rdd4_dist_with_contr)

modelsummary(models_distance_with_contr, 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 

# Order specification using rdrobust -------------------------------------

# here order is used as if it were a continuous variables, no necessarily sensible?

# specifications without controls
rdd1_order_no_contr <- rdrobust(y = df_reg_restr$resids_evi_no_contr, 
                               x = df_reg_restr$order, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd2_order_no_contr <- rdrobust(y = df_reg_restr$resids_evi_no_contr, 
                               x = df_reg_restr$order, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               covs = cov_add, 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd3_order_no_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_no_contr, 
                               x = df_reg_restr$order, 
                               c = 0, 
                               p = 1,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

rdd4_order_no_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_no_contr, 
                               x = df_reg_restr$order, 
                               c = 0, 
                               p = 2,
                               bwselect ="mserd", 
                               kernel = "triangular", 
                               cluster = df_reg_restr$mine_basin, 
                               all = TRUE)

models_orderance_no_contr <- list("Max EVI" = rdd1_order_no_contr , "Max EVI" = rdd2_order_no_contr,
                                 "Cropland EVI" = rdd3_order_no_contr, "Cropland EVI" = rdd4_order_no_contr)

modelsummary(models_orderance_no_contr, 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 


# specifications with full controls
rdd1_order_with_contr <- rdrobust(y = df_reg_restr$resids_evi_with_contr, 
                                 x = df_reg_restr$order, 
                                 c = 0, 
                                 p = 1,
                                 bwselect ="mserd", 
                                 kernel = "triangular", 
                                 cluster = df_reg_restr$mine_basin, 
                                 all = TRUE)

rdd2_order_with_contr <- rdrobust(y = df_reg_restr$resids_evi_with_contr, 
                                 x = df_reg_restr$order, 
                                 c = 0, 
                                 p = 2,
                                 bwselect ="mserd", 
                                 kernel = "triangular", 
                                 covs = cov_add, 
                                 cluster = df_reg_restr$mine_basin, 
                                 all = TRUE)

rdd3_order_with_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_with_contr, 
                                 x = df_reg_restr$order, 
                                 c = 0, 
                                 p = 1,
                                 bwselect ="mserd", 
                                 kernel = "triangular", 
                                 cluster = df_reg_restr$mine_basin, 
                                 all = TRUE)

rdd4_order_with_contr <- rdrobust(y = df_reg_restr$resids_afri_evi_with_contr, 
                                 x = df_reg_restr$order, 
                                 c = 0, 
                                 p = 2,
                                 bwselect ="mserd", 
                                 kernel = "triangular", 
                                 cluster = df_reg_restr$mine_basin, 
                                 all = TRUE)

models_orderance_with_contr <- list("Max EVI" = rdd1_order_with_contr , "Max EVI" = rdd2_order_with_contr,
                                   "Cropland EVI" = rdd3_order_with_contr, "Cropland EVI" = rdd4_order_with_contr)

modelsummary(models_orderance_with_contr, 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 



# Order specification using RDHonest --------------------------------------

# using RDHonest package that is built for discrete running variables (I think) here


# specifications without controls

# unclustered SEs
rdd1_order_no_contr_discrete <- RDHonest(resids_evi_no_contr ~ order, 
                                         data = df_reg_restr |> filter(!is.na(resids_evi_no_contr)), 
                                         cutoff = 0, 
                                         kern = "triangular", 
                                         opt.criterion = "MSE")
rdd1_order_no_contr_discrete

# clustered SEs by mine_basin
rdd2_order_no_contr_discrete <- RDHonest(resids_evi_no_contr ~ order, 
                                         data = df_reg_restr |> filter(!is.na(resids_evi_no_contr)), 
                                         cutoff = 0, 
                                         kern = "triangular", 
                                         opt.criterion = "MSE", 
                                         clusterid = df_reg_restr |> filter(!is.na(resids_evi_no_contr)) |> 
                                           mutate(mine_basin = as.numeric(mine_basin)) |> pull(mine_basin),  
                                         se.method = "EHW")
rdd2_order_no_contr_discrete

# unclustered SEs
rdd3_order_no_contr_discrete <- RDHonest(resids_afri_evi_no_contr ~ order, 
                                         data = df_reg_restr |> filter(!is.na(resids_afri_evi_no_contr)), 
                                         cutoff = 0, 
                                         kern = "triangular", 
                                         opt.criterion = "MSE")
rdd3_order_no_contr_discrete

# clustered SEs by mine_basin
rdd4_order_no_contr_discrete <- RDHonest(resids_afri_evi_no_contr ~ order, 
                                         data = df_reg_restr |> filter(!is.na(resids_afri_evi_no_contr)), 
                                         cutoff = 0, 
                                         kern = "triangular", 
                                         opt.criterion = "MSE", 
                                         clusterid = df_reg_restr |> filter(!is.na(resids_afri_evi_no_contr)) |> 
                                           mutate(mine_basin = as.numeric(mine_basin)) |> pull(mine_basin), 
                                         se.method = "EHW")
rdd4_order_no_contr_discrete


# specifications with full controls

# unclustered SEs
rdd1_order_with_contr_discrete <- RDHonest(resids_evi_with_contr ~ order, 
                                           data = df_reg_restr |> filter(!is.na(resids_evi_with_contr)), 
                                           cutoff = 0, 
                                           kern = "triangular", 
                                           opt.criterion = "MSE")
rdd1_order_with_contr_discrete

# clustered SEs by mine_basin
rdd2_order_with_contr_discrete <- RDHonest(resids_evi_with_contr ~ order, 
                                           data = df_reg_restr |> filter(!is.na(resids_evi_with_contr)), 
                                           cutoff = 0, 
                                           kern = "triangular", 
                                           opt.criterion = "MSE", 
                                           clusterid = df_reg_restr |> filter(!is.na(resids_evi_with_contr)) |> 
                                             mutate(mine_basin = as.numeric(mine_basin)) |> pull(mine_basin),  
                                           se.method = "EHW")
rdd2_order_with_contr_discrete

# unclustered SEs
rdd3_order_with_contr_discrete <- RDHonest(resids_afri_evi_with_contr ~ order, 
                                           data = df_reg_restr |> filter(!is.na(resids_afri_evi_with_contr)), 
                                           cutoff = 0, 
                                           kern = "triangular", 
                                           opt.criterion = "MSE")
rdd3_order_with_contr_discrete

# clustered SEs by mine_basin
rdd4_order_with_contr_discrete <- RDHonest(resids_afri_evi_with_contr ~ order, 
                                           data = df_reg_restr |> filter(!is.na(resids_afri_evi_with_contr)), 
                                           cutoff = 0, 
                                           kern = "triangular", 
                                           opt.criterion = "MSE", 
                                           clusterid = df_reg_restr |> filter(!is.na(resids_afri_evi_with_contr)) |> 
                                             mutate(mine_basin = as.numeric(mine_basin)) |> pull(mine_basin), 
                                           se.method = "EHW")
rdd4_order_with_contr_discrete

