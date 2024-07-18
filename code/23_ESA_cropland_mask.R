# Load Packages -----------------------------------------------------------

rm(list = ls())

pacman::p_load(
  sf,
  dplyr,
  tmap,
  tidyverse,
  countrycode,
  geosphere,
  stars,
  terra,
  ncdf4
)

# basins to get data for
basins <- read_sf(p("processed/relevant_basins.gpkg"))

africa <- st_bbox(rnaturalearth::ne_countries(continent = "africa"))

files <- list.files(p(DATA_ALT, "land_use/"))
years <- 2000:2022

for(i in years) {
  ff <- files[grepl(i, files)]
  lc_data <- rast(p(DATA_ALT, "land_use/", ff), subds = "lccs_class")
  lc_data <- crop(lc_data, africa) # crop to extent of basins
  lc_data <- subst(lc_data, c(10, 20), 1, NA)
  # lc_data <- app(lc_data, function(x) {ifelse(x %in% c(10, 20, 30),
  #                                             1, NA)})
  writeRaster(lc_data,
              filename = p("processed/land_use/croplands_", i, ".tif"),
              overwrite = TRUE)
  rm(lc_data); gc()
  cat("Year ", i, " done!\n")
}

