

# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  stargazer,
  xtable
)

sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

t_folder <- "./output/tables/"
p_folder <- "./output/plots/"

# Parameters --------------------------------------------------------------

restr_year <- 2016:2023 # years
snapshot_year <- 2023
restr_area_mined <- 0 # minimum of mined area in mine basin
restr_order <- 10 # maximum order of basins to include
excl_mine_basin <- FALSE # should the mine basin itself be excluded?
mine_downstream <- TRUE # if included, should the mine basin downstream?
restr_number_basins <- 0 # minimum number of up/downstream basins each mine basin has to have



# Data --------------------------------------------------------------------

df_reg <- readRDS(p("processed/df_reg.RDS")) |> filter(year %in% restr_year)

df_reg <- df_reg |> 
  mutate( # Signed shorthands
    dist = distance * ifelse(status == "upstream", -1, 1),
    ord = order * ifelse(status == "upstream", -1, 1),
  )


# Basins Summary Table ----------------------------------------------------

df_reg %>%
  dplyr::filter(year == snapshot_year) %>%
  dplyr::select(order, status) %>%
  table() %>%
  xtable(caption = "Number of basins by order (where 0 is the mine basin) and downstream/upstream status. Mine basins are considered downstream of a mine with order 0.",
         align = "lrr",
         label = "tab:basinsummary")


df_reg %>%
  dplyr::select(max_EVI, mean_EVI, max_c_EVI_af, mean_c_EVI_af, max_c_EVI_ESA, mean_c_EVI_ESA, tmp_max, precipitation, pop_2015,
                elevation, slope, accessibility_to_cities_2015, soilgrid_grouped) %>%
  data.frame() %>%
  stargazer(label = "tab:summary")
