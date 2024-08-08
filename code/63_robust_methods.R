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

sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

restr_year <- 2000:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 30 # maximum order of basins to include
excl_mine_basin <- TRUE # should the mine basin itself be excluded?
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


df_reg_restr$order <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$order * -1, df_reg_restr$order)
df_reg_restr$distance <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$distance * -1, df_reg_restr$distance)

# Perform the McCrary density test
mccrary_test <- rddensity(df_reg_restr$distance, c = 0, plot = TRUE)
mccrary_test <- rddensity(df_reg_restr$order, c = 0, plot = TRUE)

# Summarize the test results
summary(mccrary_test)

rdplotdensity <- rdplotdensity(mccrary_test, df_reg_restr$distance, 
                       plotRange = c(-10, 10)) # I have no idea how to interpret this precisely


rdplot(df_reg_restr$max_c_EVI_af, df_reg_restr$distance, c = 0, p = 1, nbins = NULL, binselect = "esmvpr",
       scale = NULL, kernel = "triangular", weights = NULL, h = NULL,
       covs = NULL, covs_eval = "mean", covs_drop = TRUE, ginv.tol = 1e-20,
       support = NULL, subset = NULL, masspoints = "adjust",
       hide = FALSE, ci = NULL, shade = FALSE, title = NULL,
       x.label = NULL, y.label = NULL, x.lim = NULL, y.lim = NULL,
       col.dots = NULL, col.lines = NULL)


rdplot(df_reg_restr$max_EVI, df_reg_restr$distance, c = 0, p = 1, binselect = "esmvpr",
       scale = NULL, kernel = "triangular", weights = NULL, h = NULL,
       covs = NULL, covs_eval = "mean", covs_drop = TRUE, ginv.tol = 1e-20,
       support = NULL, subset = NULL, masspoints = "adjust",
       hide = FALSE, ci = NULL, shade = FALSE, title = NULL,
       x.label = NULL, y.label = NULL, x.lim = NULL, y.lim = NULL,
       col.dots = NULL, col.lines = NULL)


# RECODE as facto to include fixed effects
df_reg_restr <- df_reg_restr %>%
  mutate(mine_basin = as.factor(mine_basin))

#### Implementation of RDROBUST
m1 <- rdrobust(y=df_reg_restr$max_EVI, x=df_reg_restr$distance, 
               bwselect="mserd", 
               covs=cbind((df_reg_restr$mine_basin), df_reg_restr$elevation, df_reg_restr$slope, df_reg_restr$precipitation, df_reg_restr$tmp_mean), 
               cluster=df_reg_restr$mine_basin)
summary(m1)
