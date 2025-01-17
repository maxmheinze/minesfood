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

africa <- st_bbox(rnaturalearth::ne_countries(continent = "africa"))

files <- list.files(p("land_use/", pre = "/data/redd/"))

years <- 2000:2022

for(i in years) {
  ff <- files[grepl(i, files)]
  lc_data <- rast(p("land_use/", ff, pre = "/data/redd/"), subds = "lccs_class")
  lc_data <- crop(lc_data, africa)
  
  # LC classes see page 81-82 here: https://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf
  
  c_broad <- subst(lc_data, c(10, 11, 12, 20, 30, 40), 1, NA)

  c_narrow <- subst(lc_data, c(10, 11, 12, 20), 1, NA)

  c_rainfed <- subst(lc_data, c(10, 11, 12), 1, NA)
  
  c_irrigated <- subst(lc_data, c(20), 1, NA)
  
  veg_broad <- subst(lc_data,
                      c(10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72,
                        80, 81, 82, 90, 100, 110, 120, 121, 122, 130, 140,
                        150, 151, 152, 153, 160, 170, 180),
                      1,
                      NA)

  veg_narrow <- subst(lc_data,
                      c(10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72,
                        80, 81, 82, 90, 100, 110, 120, 121, 122, 130, 140),
                      1,
                      NA)

  flooded_sparse <- subst(lc_data,
                          c(150, 151, 152, 153, 160, 170, 180),
                          1,
                          NA)
  
  writeRaster(c_broad,
              filename = p("processed/land_use/c_broad_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(c_narrow,
              filename = p("processed/land_use/c_narrow_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(c_rainfed,
              filename = p("processed/land_use/c_rainfed_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(c_irrigated,
              filename = p("processed/land_use/c_irrigated_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(veg_broad,
              filename = p("processed/land_use/veg_broad_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(veg_narrow,
              filename = p("processed/land_use/veg_narrow_", i, ".tif"),
              overwrite = TRUE)
  writeRaster(flooded_sparse,
              filename = p("processed/land_use/flooded_sparse_", i, ".tif"),
              overwrite = TRUE)
  
  rm(lc_data, c_broad, c_narrow, c_rainfed, c_irrigated, veg_broad, veg_narrow, flooded_sparse); gc()
  cat("Year ", i, " done!\n")
}


file_delete <- list.files(p("processed/land_use/"), full.names = T)
file_delete <- file_delete[which(grepl("json", file_delete))]
file.remove(file_delete)
