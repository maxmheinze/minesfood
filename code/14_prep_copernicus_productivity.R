library(ncdf4)
library(raster)
library(sf)
library(RColorBrewer)

# Define the years
years <- 2000:2021

# Base path for the directories
base_path <- "/data/jde/copernicus_crop_productivity"

# Loop over the years from 2006 to 2023
for (year in 2000:2021) {
  # Construct the path for the current year's directory
  year_dir <- file.path(base_path, as.character(year))
  
  # Check if the directory exists
  if (dir.exists(year_dir)) {
    # List all ZIP files in the directory
    zip_files <- list.files(year_dir, pattern = "\\.zip$", full.names = TRUE)
    
    # Unzip each file in the directory
    for (zip_file in zip_files) {
      unzip(zipfile = zip_file, exdir = year_dir)
      cat("Unzipped:", zip_file, "to", year_dir, "\n")
    }
  } else {
    cat("Directory does not exist:", year_dir, "\n")
  }
}

# Step 2: Load the Basin Data
basins <- read_sf(p("processed/relevant_basins.gpkg"))




# Define the base file path and pattern
base_path <- "/data/jde/copernicus_crop_productivity/"
crop <- "Maize_TAGP_C3S-glob-agric"
date_prefix <- "_1_"
date_suffix <- "-12-31_dek_CSSF_hist_v1.nc"

# Initialize an empty list to store file paths
file_paths <- list()

# Loop through the years and construct file paths
for (year in 2000:2021) {
  # Construct the file path for each year
  file_path <- paste0(base_path, year, "/", crop, "_", year, date_prefix, year, date_suffix)
  
  # Store the file path in the list
  file_paths[[as.character(year)]] <- file_path
}

file_paths[[as.character(2022)]] <- "/data/jde/copernicus_crop_productivity/2022/Maize_TAGP_C3S-glob-agric_2023_1_2022-12-31_dek_CSSF_hist_v1.nc"
file_paths[[as.character(2023)]] <- "/data/jde/copernicus_crop_productivity/2023/Maize_TAGP_C3S-glob-agric_2023_1_2023-12-31_dek_CSSF_hist_v1.nc"

# Loop through each year, process the rasters, and store the mean values
for (year in names(file_paths)) {
  # Load the .nc file as a raster
  Maize_12_31 <- raster(file_paths[[year]])
  
  # Crop and mask the raster with the shapefile
  Maize_12_31_cropped <- crop(Maize_12_31, basins)
  Maize_12_31_masked <- mask(Maize_12_31_cropped, basins)
  
  # Extract mean values for each basin
  mean_values <- extract(Maize_12_31_masked, basins, fun = mean, na.rm = TRUE, weights = TRUE)
  
  # Add the mean values to the basins dataframe with the appropriate year column name
  basins[[paste0("year_", year)]] <- mean_values
}

basins_cropland_productivity_esa <- basins %>%
  st_drop_geometry() %>% 
  as_tibble()  %>%
  dplyr::select(HYBAS_ID, year_2000:year_2021) %>%
  rename_with(~ str_replace_all(., "\\[,.*\\]", "")) %>%
  mutate(across(where(is.list), ~ map(.x, as.numeric) %>% unlist())) %>%
  mutate(across(where(~ is.matrix(.)), ~ as.vector(.)))
  
write_csv(basins_cropland_productivity_esa, "/data/jde/basins_evi/basins_cropland_productivity_esa.csv")


