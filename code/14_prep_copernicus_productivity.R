library(ncdf4)
library(raster)
library(sf)
library(RColorBrewer)

# Define the years
years <- 2006:2023

# Base path for the directories
base_path <- "/data/jde/copernicus_crop_productivity"


# Loop over the years from 2006 to 2023
for (year in 2006:2023) {
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

# Step 3: Load the Crop Productivity Data
cp_2001 <- rast(p("copernicus_crop_productivity/2001/Maize_TAGP_C3S-glob-agric_2001_1_2001-01-10_dek_CSSF_hist_v1.nc"))

# Step 4: Crop and Mask Raster with Shapefile
cropped_raster <- crop(cp_2001, basins)
masked_raster <- mask(cropped_raster, basins)

# Step 5: Extract or Summarize Data
extracted_values <- extract(masked_raster, basins, weight= TRUE, fun = mean, na.rm = TRUE)

# Step 6: Adding Area Extension back to Basins ID
# Convert the raster to a data frame for ggplot
raster_df <- as.data.frame(extracted_values, xy = TRUE)

# Plot the raster and shapefile using ggplot
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = TAGP)) +
  #geom_sf(data = basins, fill = NA, color = "blue", alpha = 0.1) +
  scale_fill_viridis_c(name = "Crop Productivity") + # Color scale for productivity
  theme_minimal() +
  labs(title = "Crop Productivity and Regions",
       x = "Longitude",
       y = "Latitude") +
  coord_sf()
