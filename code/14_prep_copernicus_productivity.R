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

# Define a function to get the correct day based on the month
get_end_day <- function(month) {
  if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
    return(31)
  } else if (month == 2) {
    return(28) # Assuming non-leap year; change to 29 if leap year is considered
  } else {
    return(30)
  }
}

# Base file path pattern
base_path <- "/data/jde/copernicus_crop_productivity/2001/Maize_TAGP_C3S-glob-agric_2001_1_2001-"

# Initialize an empty list to store raster files
raster_list <- list()

# Loop over each month
for (month in 1:12) {
  # Format the month as two digits
  month_str <- sprintf("%02d", month)
  
  # Days for which the data is available
  days <- c(10, 20, get_end_day(month))
  
  # Loop over each day and load the file
  for (day in days) {
    # Format the day as two digits
    day_str <- sprintf("%02d", day)
    
    # Construct the file path
    file_path <- paste0(base_path, month_str, "-", day_str, "_dek_CSSF_hist_v1.nc")
    
    # Load the .nc file as a raster
    raster_data <- raster(file_path)
    
    # Store the raster object in the list
    raster_list[[paste0("Maize_", month_str, "_", day_str)]] <- raster_data
  }
}

raster_list$Maize_01_10
raster_list$Maize_12_31


# Step 4: Crop and Mask Raster with Shapefile
Maize_12_31 <- crop(raster_list$Maize_12_31, basins)

Maize_12_31 <- mask(Maize_12_31, basins)

# Step 5: Extract or Summarize Data
Maize_12_31 <- extract(Maize_12_31, basins, fun = mean, na.rm = TRUE, weights = TRUE)

# Step 6: Adding Area Extension back to Basins ID
# Convert the raster to a data frame for ggplot
basins$year_2001 <- Maize_12_31

# Plot the raster and shapefile using ggplot
ggplot() +
  geom_raster(data = raster_df, aes(x = basins, y = y, fill = TAGP)) +
  #geom_sf(data = basins, fill = NA, color = "blue", alpha = 0.1) +
  scale_fill_viridis_c(name = "Crop Productivity") + # Color scale for productivity
  theme_minimal() +
  labs(title = "Crop Productivity and Regions",
       x = "Longitude",
       y = "Latitude") +
  coord_sf()
