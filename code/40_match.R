
library("dplyr")
library("fixest")
library("MatchIt")
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


df_reg <- readRDS(p("processed/df_reg.RDS")) |> 
  filter(!is.na(max_cropland_EVI))

match1 <- matchit(downstream ~ elevation + slope + soilq_avg + 
    tmp_min + tmp_max + precipitation,
  data = df_reg, method = "cem", distance = "glm")
# plot(match1)
# summary(match1)

match2 <- matchit(downstream ~ elevation + slope,
  data = df_reg, method = "cem", distance = "glm")
# plot(match2)
# summary(match2)

mod2_full = feols((max_cropland_EVI) ~ (distance) * downstream |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID")

mod2_full_c1 = feols((max_cropland_EVI) ~ (distance) * downstream |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match1$weights)

mod2_full_c2 = feols((max_cropland_EVI) ~ (distance) * downstream |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match2$weights)
