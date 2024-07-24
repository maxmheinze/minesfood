library(ncdf4)
library(raster)
library(sf)

# Step 1: Read the NetCDF data
nc_data <- nc_open(p("jde/copernicus_crop_productivity/Maize_TAGP_C3S-glob-agric_2004_1_2004-01-10_dek_CSSF_hist_v1.nc"))
variable <- ncvar_get(nc_data, "variable_name") # Replace with your variable name
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
nc_close(nc_data)

# Step 2: Load the shapefile
shapefile_path <- "path_to_your_shapefile.shp"
shape_data <- st_read(shapefile_path)

# Step 3: Convert NetCDF data to Raster
r <- raster(nc_file, varname = "variable_name")

# Step 4: Crop and Mask Raster with Shapefile
cropped_raster <- crop(r, shape_data)
masked_raster <- mask(cropped_raster, shape_data)

# Step 5: Extract or Summarize Data
extracted_values <- extract(masked_raster, shape_data)
mean_values <- lapply(extracted_values, mean, na.rm = TRUE)
