

# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  stargazer,
  xtable
)

sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

restr_year <- 2016:2023 # years
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have

date <- "20240813"

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

df_reg_restr <- df_reg_restr |>
  mutate( # Signed shorthands
    dist_signed = dist_km * ifelse(status == "upstream", -1, 1)
  )


# Basins Summary Table ----------------------------------------------------

df_reg_restr %>%
  dplyr::filter(year == snapshot_year) %>%
  dplyr::select(dist_n, status) %>%
  table() %>%
  xtable(caption = "Number of basins by order (where 0 is the mine basin) and downstream/upstream status. Mine basins are considered downstream of a mine with order 0.",
         align = "lrr",
         label = "tab:basinsummary")


df_reg_restr %>%
  dplyr::select(max_EVI, mean_EVI, max_c_EVI_af, mean_c_EVI_af, max_c_EVI_ESA, mean_c_EVI_ESA, tmp_max, precipitation, pop_2015,
                elevation, slope, accessibility_to_cities_2015, soilgrid_grouped) %>%
  data.frame() %>%
  stargazer(label = "tab:summary")
