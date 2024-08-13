############################################################################################
######### This code implements the Robust Regressions and the McCrary Density Test ######### 
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

# Perform the McCrary density test
mccrary_test <- rddensity(df_reg_restr$distance, c = 0)
mccrary_test <- rddensity(df_reg_restr$distance_centroid, c = 0)
mccrary_test <- rddensity(df_reg_restr$order, c = 0)

# Summarize the test results
summary(mccrary_test)

rdplotdensity <- rdplotdensity(mccrary_test, df_reg_restr$distance, 
                               plotRange = c(-10, 10)) # I have no idea how to interpret this precisely

##############################################################
#### REPLICATION using RDROBUST###############################
##############################################################

df_reg_restr_africover <- df_reg_restr %>% 
  filter(!is.na(max_c_EVI_af))

m1 <- feols(max_EVI ~ 1 | mine_basin + year + soilgrid_grouped, df_reg_restr)
m2 <- feols(max_c_EVI_af ~ 1 | mine_basin + year + soilgrid_grouped, df_reg_restr_africover)


df_reg_restr$resid_evi <- m1$residuals
df_reg_restr_africover$resids_afri_evi <- m2$residuals

cov_add <- cbind(df_reg_restr$elevation, df_reg_restr$slope, df_reg_restr$tmp_max, df_reg_restr$precipitation, df_reg_restr$accessibility_to_cities_2015, df_reg_restr$pop_2015)
cov_add_africover <- cbind(df_reg_restr_africover$elevation, df_reg_restr_africover$slope, df_reg_restr_africover$tmp_max, df_reg_restr_africover$precipitation, df_reg_restr_africover$accessibility_to_cities_2015, df_reg_restr_africover$pop_2015)

rdd1 <- rdrobust(y = df_reg_restr$resid_evi, 
                 x = df_reg_restr$distance, 
                 c = 0, 
                 p = 1,
                 bwselect ="mserd", 
                 kernel = "triangular", 
                 covs = cov_add, 
                 cluster = df_reg_restr$mine_basin, 
                 all = TRUE)

rdd2 <- rdrobust(y = df_reg_restr$resid_evi, 
                 x = df_reg_restr$distance, 
                 c = 0, 
                 p = 2,
                 bwselect ="mserd", 
                 kernel = "triangular", 
                 covs = cov_add, 
                 cluster = df_reg_restr$mine_basin, 
                 all = TRUE)

rdd3 <- rdrobust(y = df_reg_restr_africover$resids_afri_evi, 
                 x = df_reg_restr_africover$distance, 
                 c = 0, 
                 p = 1,
                 bwselect ="mserd", 
                 kernel = "triangular", 
                 covs = cov_add_africover, 
                 cluster = df_reg_restr_africover$mine_basin, 
                 all = TRUE)

rdd4 <- rdrobust(y = df_reg_restr_africover$resids_afri_evi, 
                 x = df_reg_restr_africover$distance, 
                 c = 0, 
                 p = 2,
                 bwselect ="mserd", 
                 kernel = "triangular", 
                 covs = cov_add_africover, 
                 cluster = df_reg_restr_africover$mine_basin, 
                 all = TRUE)


modelsummary(list(rdd1,rdd2,rdd3,rdd4), 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 





#### using order as a running variable

rdd1_or <- rdrobust(y = df_reg_restr$resid_evi, 
                    x = df_reg_restr$order, 
                    c = 0, 
                    p = 1,
                    bwselect ="mserd", 
                    kernel = "triangular", 
                    covs = cov_add, 
                    cluster = df_reg_restr$mine_basin, 
                    all = TRUE)

rdd2_or <- rdrobust(y = df_reg_restr$resid_evi, 
                    x = df_reg_restr$order, 
                    c = 0, 
                    p = 2,
                    bwselect ="mserd", 
                    kernel = "triangular", 
                    covs = cov_add, 
                    cluster = df_reg_restr$mine_basin, 
                    all = TRUE)

rdd3_or <- rdrobust(y = df_reg_restr$resids_afri_evi, 
                    x = df_reg_restr$order, 
                    c = 0, 
                    p = 1,
                    bwselect ="mserd", 
                    kernel = "triangular", 
                    covs = cov_add_africover, 
                    cluster = df_reg_restr$mine_basin, 
                    all = TRUE)

rdd4_or <- rdrobust(y = df_reg_restr$resids_afri_evi, 
                    x = df_reg_restr$order, 
                    c = 0, 
                    p = 2,
                    bwselect ="mserd", 
                    kernel = "triangular", 
                    covs = cov_add_africover, 
                    cluster = df_reg_restr$mine_basin, 
                    all = TRUE)


modelsummary(list(rdd1_or,rdd2_or,rdd3_or,rdd4_or), 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 





########################### Running a Placebo Test ################################################
# RECODE as factor to include fixed effects
df_reg_restr_falsification <- df_reg_restr %>%
  filter(downstream == 0)

#### Implementation of RDROBUST
m2 <- rdrobust(y=df_reg_restr_falsification$max_EVI, x=df_reg_restr_falsification$order, c= -3,
               bwselect="mserd", 
               kernel = "triangular", 
               covs=cbind(df_reg_restr_falsification$mine_basin, df_reg_restr_falsification$year), 
               cluster=df_reg_restr_falsification$mine_basin)
summary(m2)


# placebo
m2 <- rdrobust(y=df_reg_restr$tmp_mean, 
               x=df_reg_restr$order, 
               c= 0,
               bwselect="mserd", 
               kernel = "triangular", 
               covs=cbind(df_reg_restr$mine_basin,df_reg_restr$year), 
               cluster=df_reg_restr$mine_basin)

m2 <- rdrobust(y=df_reg_restr$tmp_max, 
               x=df_reg_restr$distance, 
               c= 0,
               bwselect="mserd", 
               kernel = "triangular", 
               covs=cbind(df_reg_restr$mine_basin,df_reg_restr$year), 
               cluster=df_reg_restr$mine_basin)

summary(m2)


m3 <- rdrobust(y=df_reg_restr$tmp_max, 
               x=df_reg_restr$order, 
               c= 0,
               h = c(-10,10),
               kernel = "uniform", 
               covs=cbind(df_reg_restr$mine_basin,df_reg_restr$year), 
               cluster=df_reg_restr$mine_basin)

summary(m3)

m3 <- rdrobust(y=df_reg_restr$elevation, 
               x=df_reg_restr$order, 
               c= 0,
               bwselect="mserd", 
               kernel = "triangular", 
               covs=cbind(df_reg_restr$mine_basin,df_reg_restr$year), 
               cluster=df_reg_restr$mine_basin)

summary(m3)


modelsummary(list(rdd1,rdd2,rdd3,rdd4), 
             output = "latex_tabular",
             fmt = 3, 
             statistic = c("({std.error})", "[{p.value}]")) 






########################################################################################################## 

### implementation of local randomization approach
#z <- df_reg_restr[, c("elevation", "slope", "tmp_max", "precipitation", "accessibility_to_cities_2015", "pop_2015")]
#df_reg_restr <- df_reg_restr %>% mutate(order = as.numeric(order))
#rdwinselect(df_reg_restr$order, z, seed = 50)

            