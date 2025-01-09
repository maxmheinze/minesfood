
library("dplyr")
library("fixest")
library("MatchIt")
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


df_reg <- readRDS(p("processed/df_reg.RDS")) |>
  filter(!is.na(downstream))
df_reg <- df_reg |> 
  mutate( # Signed shorthands
    dist = distance * ifelse(status == "upstream", -1, 1),
    ord = order * ifelse(status == "upstream", -1, 1),
  )

match_geo <- matchit(downstream ~ elevation + slope,
  data = df_reg, method = "cem", distance = "glm")
# plot(match_geo)
# summary(match_geo)

# feols(max_EVI ~ factor(ord) |
#   year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID") |> 
#   summary()

m_geo_plain = feols(max_c_EVI_af ~ factor(ord) |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match_geo$weights)
summary(m_geo_plain)

m_geo_full = feols(max_c_EVI_af ~ factor(ord) + 
    elevation + slope + tmp_max + precipitation + 
    accessibility_to_cities_2015 + pop_2015 |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match_geo$weights)
summary(m_geo_full)

n_geo_plain = feols(max_EVI ~ factor(ord) |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match_geo$weights)
summary(n_geo_plain)

n_geo_full = feols(max_EVI ~ factor(ord) + 
    elevation + slope + tmp_max + precipitation + 
    accessibility_to_cities_2015 + pop_2015 |
  year + as.factor(mine_basin), data = df_reg, cluster = "HYBAS_ID",
  weights = match_geo$weights)
summary(n_geo_full)
