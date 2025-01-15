# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})



# Cropland productivity ESA -----------------------------------------------


basins_cropland_productivity_esa <- read_csv(p("basins_evi/basins_cropland_productivity_esa.csv"))

basins_cropland_productivity_esa_long <- basins_cropland_productivity_esa %>%
  pivot_longer(
    cols = starts_with("year_"),  # Selecting columns starting with 'year_'
    names_to = "year",            # New column for the year
    values_to = "cropland_yield_kg_ha",          # New column for the values
    names_prefix = "year_",       # Removing 'year_' prefix
    names_transform = list(Year = as.integer)) %>%
  mutate(year = as.integer(year), 
         cropland_yield_kg_ha = replace(cropland_yield_kg_ha, 
                                        cropland_yield_kg_ha == 0,
                                        NA))



basin_evi_cropland <- basin_evi %>% 
  left_join(basins_cropland_productivity_esa_long, by = c("HYBAS_ID", "year")) 