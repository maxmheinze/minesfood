# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# NDVI data for all areas of basins ---------------------------------------------






# saving the data -------------------------------------------------------------

write_csv(basin_evi_cropland, p("basins_evi/basin_ndvi.csv"))

