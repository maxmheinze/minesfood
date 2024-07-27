library(ncdf4)
library(raster)
library(sf)

# Step 1: Read the NetCDF data
# Define the directory containing the zip files
zip_directory <- p("copernicus_crop_productivity/2003") # Replace with your directory path

# Define the output directory for unzipped files
output_directory <- p("copernicus_crop_productivity/2003/")  # Replace with your output directory path

# List all the zip files in the directory
zip_files <- list.files(zip_directory, pattern = "*.zip", full.names = TRUE)

# Loop through each zip file and unzip it
for (zip_file in zip_files) {
  # Unzip the file
  unzip(zip_file, exdir = output_directory)
  
  # Print a message to indicate the progress
  cat("Unzipped:", zip_file, "\n")
}

nc_data <- nc_open(p("copernicus_crop_productivity/2004/Maize_TAGP_C3S-glob-agric_2004_1_2004-01-10_dek_CSSF_hist_v1.nc"))
variable <- ncvar_get(nc_data, "TAGP") # Replace with your variable name
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
nc_close(nc_data)

# Step 2: Load the Basin Data
basins <- read_sf(p("processed/relevant_basins.gpkg"))

# Step 3: Convert NetCDF data to Raster
r <- raster(p("copernicus_crop_productivity/2004/Maize_TAGP_C3S-glob-agric_2004_1_2004-01-10_dek_CSSF_hist_v1.nc"), varname = "TAGP")

# Step 4: Crop and Mask Raster with Shapefile
cropped_raster <- crop(r, basins)
masked_raster <- mask(cropped_raster, basins)

values <- extract(cropped_raster, basins, method = "simple", fun = mean, na.rm = TRUE)

# Step 5: Extract or Summarize Data
extracted_values <- extract(masked_raster, basins)
mean_values <- lapply(extracted_values, mean, na.rm = TRUE)
