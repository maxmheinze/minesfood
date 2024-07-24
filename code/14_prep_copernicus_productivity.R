library(ncdf4)
library(raster)
library(sf)

##############################
#Don't Run IN PROGRESS########
##############################


# Step 1: Read the NetCDF data

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
