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
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# basins to get data for
basins <- read_sf(p("processed/relevant_basins_ordered.gpkg"))

africa <- st_bbox(rnaturalearth::ne_countries(continent = "africa"))

files <- list.files(p("land_use/", pre = "/data/redd/"))

years <- 2000:2022

for(i in years) {
  ff <- files[grepl(i, files)]
  lc_data <- rast(p("land_use/", ff, pre = "/data/redd/"), subds = "lccs_class")
  lc_data <- crop(lc_data, africa)
  
  # add 30 for mosaic with >50% cropland, 40 for mosaic with <50% cropland
  lc_data <- subst(lc_data, c(10, 20), 1, NA) 
  
  writeRaster(lc_data,
              filename = p("processed/land_use/croplands_mosaic_", i, ".tif"),
              overwrite = TRUE)
  rm(lc_data); gc()
  cat("Year ", i, " done!\n")
}

