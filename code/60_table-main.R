library("dplyr")
library("readr")
library("fixest")
# library("pdftools")
library("rdrobust")
sapply(list.files("./R", ".R$"), \(f) {source(paste0("./R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- TRUE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

# c("nomask", "cci_veg_broad", "cci_veg_narrow")
v_mask <- "cci_veg_broad"
# c("cci_c_broad", "cci_c_narrow", "cci_c_rainfed", "cci_c_irrigated", "af_c", "esri_c")
c_mask <- "cci_c_broad"

comp_max <- "16" # c("16", "px")

measure <- "EVI" # c("EVI", "NDVI")

spec_general_max <- paste0("max_", measure, "_", comp_max, "_", v_mask)
spec_croplands_max <- paste0("max_", measure, "_", comp_max, "_", c_mask)

spec_general_mean <- paste0("mean_", measure, "_16_", v_mask)
spec_croplands_mean <- paste0("mean_", measure, "_16_", c_mask)

date <- "20250115"

t_folder <- "./output/tables/"
p_folder <- "./output/plots/"

f_name <- paste0("table_main_maxorder", restr_order, "_minnumber", restr_number_basins,
                 "_EXCLmine", excl_mine_basin, "_", date)
p_name <- paste0("plot_order_effects_maxorder", restr_order, "_minnumber", restr_number_basins,
                 "_EXCLmine", excl_mine_basin, "_", date)

df_reg <- readRDS(p("processed/df_reg.RDS"))
mine_size_restr <- df_reg |> filter(mine_area_km2 > restr_area_mined) |> pull(mine_basin) |> unique()

df_reg_restr <- df_reg |>
  filter(dist_n <= restr_order,
         year %in% restr_year,
         mine_basin %in% mine_size_restr,
         if(excl_mine_basin) dist_n > 0 else dist_n >= 0)
if(!mine_downstream) {
  df_reg_restr <- df_reg_restr |>
    mutate(downstream = replace(downstream, dist_n == 0, 0))
}
if(restr_number_basins > 0) {
  mine_number_restr <- df_reg |> filter(year == restr_year[1], dist_n != 0) |>
    group_by(mine_basin, downstream, dist_n) |> count() |>
    group_by(mine_basin, downstream) |> count() |>
    filter(n >= restr_number_basins) |>
    group_by(mine_basin) |> count() |>
    filter(n == 2) |>
    pull(mine_basin)
  df_reg_restr <- df_reg_restr |>
    filter(mine_basin %in% mine_number_restr)
}

# No interaction specification --------------------------------------------

# no covariates
mod1_noint_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           downstream |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo covariates
mod2_noint_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           downstream +
                           elevation + slope + soilgrid_grouped |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met covariates
mod3_noint_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met + socio covariates
mod4_noint_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           downstream +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")


# Order specification -----------------------------------------------------

# no covariates
mod1_order_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           i(dist_order, ref = -1) |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo covariates
mod2_order_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           i(dist_order, ref = -1) +
                           elevation + slope + soilgrid_grouped|
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met covariates
mod3_order_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           i(dist_order, ref = -1) +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")

# with geo + met + socio covariates
mod4_order_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                           i(dist_order, ref = -1) +
                           elevation + slope + soilgrid_grouped +
                           tmp_max + precipitation +
                           accessibility_to_cities_2015 + pop_2015 |
                           year +  as.factor(mine_basin),
                         data = df_reg_restr,
                         cluster = "mine_basin")


# Distance linear specification -------------------------------------------

# no covariates
mod1_dist_linear_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo covariates
mod2_dist_linear_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km) * downstream +
                          elevation + slope + soilgrid_grouped |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met covariates
mod3_dist_linear_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met + socio covariates
mod4_dist_linear_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")



# Distance square specification -------------------------------------------

# no covariates
mod1_dist_square_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km + I(dist_km^2)) * downstream |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo covariates
mod2_dist_square_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km + I(dist_km^2)) * downstream +
                          elevation + slope + soilgrid_grouped |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met covariates
mod3_dist_square_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km + I(dist_km^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")

# with geo + met + socio covariates
mod4_dist_square_contr = feols(c(get(spec_general_max), get(spec_croplands_max)) ~
                          (dist_km + I(dist_km^2)) * downstream +
                          elevation + slope + soilgrid_grouped +
                          tmp_max + precipitation +
                          accessibility_to_cities_2015 + pop_2015 |
                          year +  as.factor(mine_basin),
                        data = df_reg_restr,
                        cluster = "mine_basin")


# Output creation ---------------------------------------------------------

setFixest_dict(dict = dict_fixest)

evi_cropland_avg <- df_reg_restr |>
  filter(!is.na(get(spec_croplands_max))) |>
  pull(get(spec_croplands_max)) |> mean()
evi_general_avg <- df_reg_restr |>
  filter(!is.na(get(spec_general_max))) |>
  pull(get(spec_general_max)) |> mean()


extra_lines <- list()
extra_lines[["^_ "]] <- rep(" ", 6)
extra_lines[["^_Sample Mean Effect"]] <-
  c(coef(mod1_noint_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod2_noint_contr[1])[["downstream"]] / evi_general_avg * 100,
    # coef(mod3_noint_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod4_noint_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod1_noint_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod2_noint_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    # coef(mod3_noint_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod4_noint_contr[2])[["downstream"]] / evi_cropland_avg * 100)

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
  c(coef(mod1_order_contr[1])[["dist_order::1"]] / evi_general_avg * 100,
    coef(mod2_order_contr[1])[["dist_order::1"]] / evi_general_avg * 100,
    # coef(mod3_order_contr[1])[["dist_order::1"]] / evi_general_avg * 100,
    coef(mod4_order_contr[1])[["dist_order::1"]] / evi_general_avg * 100,
    coef(mod1_order_contr[2])[["dist_order::1"]] / evi_cropland_avg * 100,
    coef(mod2_order_contr[2])[["dist_order::1"]] / evi_cropland_avg * 100,
    # coef(mod3_order_contr[2])[["dist_order::1"]] / evi_cropland_avg * 100,
    coef(mod4_order_contr[2])[["dist_order::1"]] / evi_cropland_avg * 100)

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
  c(coef(mod1_dist_linear_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod2_dist_linear_contr[1])[["downstream"]] / evi_general_avg * 100,
    # coef(mod3_dist_linear_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod4_dist_linear_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod1_dist_linear_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod2_dist_linear_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    # coef(mod3_dist_linear_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod4_dist_linear_contr[2])[["downstream"]] / evi_cropland_avg * 100)

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
  c(coef(mod1_dist_square_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod2_dist_square_contr[1])[["downstream"]] / evi_general_avg * 100,
    # coef(mod3_dist_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod4_dist_square_contr[1])[["downstream"]] / evi_general_avg * 100,
    coef(mod1_dist_square_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod2_dist_square_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    # coef(mod3_dist_contr[2])[["downstream"]] / evi_cropland_avg * 100,
    coef(mod4_dist_square_contr[2])[["downstream"]] / evi_cropland_avg * 100)

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
         dict = c("dist_order" = "Order5"))
iplot(mod4_order_contr, ci_level = 0.95,
         main = "Effects by order of basins (with controls)", sub = "Black = EVI, Red = Africover EVI",
         dict = c("dist_order" = "Order"))




# Create Plot for Presentation; increasing Fontsizes and making the bigfat

# Set graphical parameters to increase axis label size and overall text size
par(cex.axis = 1.2,
    cex.lab = 1.4,          # Increase axis label size
    cex.main = 1.5,           # Increase main title size
    cex.sub = 1.2,          # Increase subtitle size
    lwd = 1.5,                # Increase line widths globally
    font.axis = 2,          # Bold axis labels
    font.lab = 2,           # Bold axis titles
    font.main = 2,          # Bold main title
    font.sub = 2)           # Bold subtitle

# Plot with adjusted sizes and thicknesses
iplot(
  mod4_order_contr,
  cex = 1.5,
  ci_level = 0.95,
  pt.cex = 1.5,             # Increase point size
  pt.lwd = 1.5,               # Increase line width of points
  ci.lwd = 1.5,               # Increase confidence interval line width
  main = "Effects by Order of Basins (with Controls)",
  sub = "Black = EVI, Red = Cropland EVI",
  dict = c("dist_order" = "Basin Order"))

# Close the graphics device if needed
dev.off()

