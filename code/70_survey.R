# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr,
  DescTools
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


##############################################################################
# 1. Load Packages
##############################################################################
library(haven)
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(fixest)

##############################################################################
# 2. Load Datasets
##############################################################################
# Adjust file paths as necessary
hh      <- read_dta(p("lsms_survey_data/Household_dataset.dta"))
indivd  <- read_dta(p("lsms_survey_data/Individual_dataset.dta"))
pc      <- read_dta(p("lsms_survey_data/Plotcrop_dataset.dta"))
pd      <- read_dta(p("lsms_survey_data/Plot_dataset.dta"))

# If 'basins' is already an sf object (from GeoPackage), you can use:
df_reg <- readRDS(p("processed/df_reg.RDS"))
basins <- read_sf(p("processed/basins/basins.gpkg"))

##############################################################################
# 3. Farmers Data from Plot Dataset (pd)
##############################################################################
# Create a unique set of plots based on latitude and remove missing latitudes
unique_plots <- pd %>%
  distinct(lat_modified, .keep_all = TRUE) %>%  # Remove duplicates by lat
  drop_na(lat_modified)                         # Drop rows with missing lat

# Convert to sf
farmers_sf <- st_as_sf(
  unique_plots,
  coords = c("lon_modified", "lat_modified"),  # specify lon/lat columns
  crs    = 4326                                # WGS84
)

# Transform farmers to match the basins' CRS (if necessary)
farmers_sf <- st_transform(farmers_sf, crs = st_crs(basins))

##############################################################################
# 4. Simplify Basins Geometry and Check Intersections
##############################################################################
# Optionally simplify the basins for faster processing
basins_simplified <- st_simplify(basins, dTolerance = 10, preserveTopology = TRUE)

# Create an intersection matrix (farmers vs. basins)
intersection_matrix <- st_intersects(farmers_sf, basins_simplified, sparse = FALSE)

# Count how many basins intersect each farmer plot
farmers_sf$intersect_count <- rowSums(intersection_matrix)

# Filter farmers that intersect at least one basin
intersecting_farmers <- farmers_sf %>%
  filter(intersect_count > 0)

# Summarize results
cat("Total number of basin intersections:", sum(farmers_sf$intersect_count), "\n")

##############################################################################
# 5. Plot Map of Africa with Intersecting Farmers
##############################################################################
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

ggplot() +
  geom_sf(data = africa, fill = "gray90", color = "gray50") +
  geom_sf(data = basins_simplified, aes(fill = basin_id), color = "blue", alpha = 0.8) +
  geom_sf(data = intersecting_farmers, color = "red", size = 0.01) +
  #coord_sf(xlim = c(30, 50), ylim = c(-15, 15)) +  # Focus on Eastern Africa
  labs(
    title    = "Intersecting Farmers and Basins",
    subtitle = "Red points: Farmers' plots | Blue polygons: Basins",
    x        = "Longitude",
    y        = "Latitude"
  ) +
  theme_minimal()

##############################################################################
# 6. Convert pc (Plotcrop Dataset) to sf
##############################################################################
# Drop rows with missing lat/long if needed
unique_plots_pc <- pd %>%
  drop_na(lat_modified)

pc_sf <- st_as_sf(
  unique_plots_pc,
  coords = c("lon_modified", "lat_modified"),
  crs    = 4326  # Change if your data uses a different CRS
)

##############################################################################
# 7. Ensure Basins is in sf and Match CRS
##############################################################################
# If 'basins' is already sf, just rename or keep as is.
basins_sf <- basins  # if you prefer a shorter name

# Check CRS
st_crs(pc_sf)
st_crs(basins_sf)

# If they differ, transform one to match the other:
# basins_sf <- st_transform(basins_sf, st_crs(pc_sf))

##############################################################################
# 8. Spatial Join: Points that Fall into Each Basin
##############################################################################
# left = FALSE => only keep points that intersect a basin
# left = TRUE  => keep all points (with NA where no intersection)
pc_basins_joined <- st_join(pc_sf, basins_sf, left = FALSE)

##############################################################################
# 9. Further Data Processing (Upstream/Downstream, etc.)
##############################################################################
pc_basin <- pc_basins_joined %>%
  mutate(
    distance    = ifelse(status == "upstream", DIST_MAIN * -1, DIST_MAIN),
    downstream  = ifelse(status == "downstream", 1, 0),
    order_new   = ifelse(downstream == 0, -ORDER, ORDER)
  )


df_after_2016 <- pc_basin %>%
  filter(
    (country == "Ethiopia" & wave %in% c(3, 4, 5)) |
      (country == "Malawi"   & wave %in% c(3, 4)) |
      (country == "Mali"     & wave %in% c(2)) |
      (country == "Tanzania" & wave %in% c(5)) |
      (country == "Nigeria" & wave == 3) |
      (country == "Uganda" & wave >= 7)
  )

# Basic custom Winsorization function
winsorize_custom <- function(x, lower_prob = 0.01, upper_prob = 0.99) {
  # Calculate the cutoffs
  lower_cut <- quantile(x, lower_prob, na.rm = TRUE)
  upper_cut <- quantile(x, upper_prob, na.rm = TRUE)

  # Replace out-of-bounds values with the cutoffs
  x[x < lower_cut] <- lower_cut
  x[x > upper_cut] <- upper_cut

  return(x)
}

# Apply winsorization to the `yield_kg` column
df_winsor <- df_after_2016 %>%
  mutate(
    yield_kg_wins = winsorize_custom(yield_kg, lower_prob = 0.01, upper_prob = 0.99),
    harvest_kg_wins = winsorize_custom(harvest_kg, lower_prob = 0.01, upper_prob = 0.99),
    farm_size_wins = winsorize_custom(farm_size, lower_prob = 0.01, upper_prob = 0.99),
    yield_value_USD_wins = winsorize_custom(yield_value_USD, lower_prob = 0.01, upper_prob = 0.99)
  )

##############################################################################
# 10. Yield Regressions (fixest)
##############################################################################
m1 <- feols(
  log(yield_kg_wins) ~ i(as.factor(dist_order), ref = -1) + dist_popcenter|
    MAIN_BAS + admin_1_name + country^wave,
  data = df_winsor
  )

iplot(m1)
summary(m1)

m2 <- feols(
  log(yield_kg) ~ as.factor(dist_order) + plot_area_GPS + total_labor_days |
    MAIN_BAS + admin_1_name + country^wave + hh_id_obs,
  data = pc_basin
)

summary(m2)

m3 <- feols(
  log(yield_kg) ~ i(order_new, ref = -1):downstream + total_labor_days |
    MAIN_BAS + admin_1 + season + country^wave + hh_id_obs,
  data = pc_basin
)

iplot(m3)


m4 <- feols(
  (yield_kg) ~ downstream:(exp(-1*distance)) + total_labor_days |
    MAIN_BAS + admin_1 + country^wave,
  data = pc_basin
)

m4


##############################################################################
# 12. Summarize Upstream/Downstream Counts
##############################################################################
plot_counts <- pc_basins_joined %>%
  filter(status %in% c("upstream", "mine", "downstream")) %>%
  group_by(MAIN_BAS, status) %>%
  summarise(num_plots = n(), .groups = "drop")

# Pivot to wide format
plot_counts_wide <- plot_counts %>%
  pivot_wider(
    names_from  = status,
    values_from = num_plots,
    values_fill = 0
  )

