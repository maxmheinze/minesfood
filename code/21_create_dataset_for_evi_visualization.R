pacman::p_load(
  sf,
  tidyverse,
  magrittr
)

basins <- st_read("./data/relevant_basins.gpkg")

evi <- read_csv("/data/jde/basins_evi/basin_evi.csv")

output_data <- basins %>%
  left_join(evi, by = "HYBAS_ID") %>%
  dplyr::filter(year == 2023)

st_write(output_data, "./data/basins_viz_evi.gpkg")