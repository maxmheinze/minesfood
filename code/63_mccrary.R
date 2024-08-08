###############################################################################
######### This code implements the Mccrary Test for endgenous sorting ######### 
###############################################################################
library("dplyr")
library("readr")
library("fixest")
library("pdftools")
library("rdrobust")
library("rdd")
library("rddensity")

sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

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


# Change the variable order based on the condition
df_reg_restr$order <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$order * -1, df_reg_restr$order)
df_reg_restr$distance <- ifelse(df_reg_restr$downstream == 0, df_reg_restr$distance * -1, df_reg_restr$distance)

DCdensity(df_reg_restr$order,0,plot=TRUE)

DCdensity(df_reg_restr$distance, 0, bw = 20, verbose = FALSE,
          plot = TRUE, ext.out = TRUE, htest = TRUE)

DCdensity(df_reg_restr$distance, 0, bw = 20, verbose = FALSE,
          plot = TRUE, ext.out = TRUE, htest = TRUE)
# Give it the running variable and the cutoff
# It will pick the bandwidth, and has default polynomials, kernel, and bias correction
# It doesn't have bins to pick
denstest <- rddensity(df_reg_restr$distance, c = 0)
summary(denstest)

# Not plot the density discontinuity
# It needs the density test object we just made
rdplotdensity(denstest, df_reg_restr$distance)

summary()
