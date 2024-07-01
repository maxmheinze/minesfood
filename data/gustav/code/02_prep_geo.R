# Load Required Libraries -------------------------------------------------
pacman::p_load(
  sf, raster, tmap, did, countrycode, data.table, tidyverse, haven,
  readxl, rnaturalearth, rnaturalearthdata, here, fixest, zoo
)

# Reading in Data ---------------------------------------------------------
# Mining data
mines <- read_sf("~/minesfood/data/gustav/MiningConversion_2007-2017Vec/MiningConversion_2007-2017Vec.shp")
full_conversion <- read_sf("~/minesfood/data/gustav/FullConversiontoMiningExtent2019/FullConversiontoMiningExtent2019.shp")

# Coordinates data
coordinates <- read_excel("~/minesfood/data/gustav/coordinates.xls")
coordinates_sf <- coordinates %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# District shapefile for GLSS 7
Ghana_Districts_216 <- read_sf("~/minesfood/data/gustav/districts_216/Map_of_Districts_216.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(
    code_length = nchar(ID),
    region = as.numeric(ifelse(code_length == 3, substr(ID, 1, 1), substr(ID, 1, 2))),
    dist_code = as.numeric(ifelse(code_length == 3, substr(ID, 2, 3), substr(ID, 3, 4)))
  ) %>%
  st_make_valid() %>%
  rename(district = ID)

# Preparing Mine Data -----------------------------------------------------
mines_clean <- mines %>%
  mutate(year = classifica + 2000)

# Filter mining sites by year
mining_sites_sf_07 <- mines_clean %>% filter(year <= 2007)
mining_sites_sf_12 <- mines_clean %>% filter(year <= 2012)
mining_sites_sf_17 <- mines_clean %>% filter(year <= 2017)

# Function to Process GLSS Data -------------------------------------------
process_glss_data <- function(file_path, glss_num, eanum_prefix) {
  data <- read_csv(file_path) %>%
    mutate(glss = glss_num, clust = eanum + eanum_prefix) %>%
    left_join(coordinates_sf) %>%
    st_as_sf() %>%
    st_transform(st_crs(mining_sites_sf_07)) %>%
    dplyr::select(clust, geometry)
  return(data)
}

process_glss_data_4 <- function(file_path, glss_num, eanum_prefix) {
  data <- read_csv(file_path) %>%
    rename(eanum = ea) %>%
    mutate(glss = glss_num, clust = eanum + eanum_prefix) %>%
    left_join(coordinates_sf) %>%
    st_as_sf() %>%
    st_transform(st_crs(mining_sites_sf_07)) %>%
    dplyr::select(clust, geometry)
  return(data)
}

# GLSS Data Preparation ---------------------------------------------------
# GLSS 2
cluster_sf_2 <- process_glss_data(
  "~/minesfood/data/gustav/clusters_GLSS2.csv",
  glss_num = 2,
  eanum_prefix = 2000
)

# GLSS 4
cluster_sf_4 <- process_glss_data_4(
  "~/minesfood/data/gustav/eanum_district_GLSS4.csv",
  glss_num = 4,
  eanum_prefix = 4000
)

# GLSS 5
cluster_sf_5 <- process_glss_data(
  "~/minesfood/data/gustav/eanum_district_GLSS5.csv",
  glss_num = 5,
  eanum_prefix = 5000
)

# GLSS 6
glss6_cluster <- read_csv("~/minesfood/data/gustav/glss6_coordinates_complete.csv") %>%
  st_as_sf(coords = c("latitude", "longitude"), crs = "WGS84") %>%
  mutate(clust = cluster + 60000) %>%
  dplyr::select(clust, geometry)

# GLSS 7
glss7_cluster <- read_sf("~/minesfood/data/gustav/glss7.shp") %>%
  mutate(clust = as.numeric(Cluster) + 70000) %>%
  dplyr::select(clust, geometry)

cluster <- rbind(cluster_sf_2, cluster_sf_4, cluster_sf_5, glss6_cluster, glss7_cluster)

# Adding Industrial Dummy -------------------------------------------------

artisanal_mine <- read_sf("~/minesfood/data/gustav/FullConversiontoMiningExtent2019/FullConversiontoMiningExtent2019.shp") %>% 
  filter(MineType == 1) %>%
  st_make_valid() %>% st_simplify(dTolerance = 10)

industrial_mine <- read_sf("~/minesfood/data/gustav/FullConversiontoMiningExtent2019/FullConversiontoMiningExtent2019.shp") %>% 
  filter(MineType == 2) %>% 
  st_make_valid()  



# Assuming cluster, industrial_mine, and artisanal_mine are already loaded and transformed

# Define buffer distances
buffer_distances <- c(10000)

# Create buffers for each distance and store them in a list
buffers <- lapply(buffer_distances, function(dist) st_buffer(cluster, dist = dist))

buffers <- lapply(buffers, function(dist) st_simplify(dist, dTolerance = 10))

# Check if polygons are within each buffer for industrial mines
polygons_within_buffers <- lapply(buffers, function(buffer) st_intersects(buffer, industrial_mine, sparse = FALSE))

# Create new columns indicating if a cluster has polygons within each buffer distance for industrial mines
for (i in seq_along(buffer_distances)) {
  cluster[[paste0("industrial_mine_", buffer_distances[i] / 1000, "km")]] <- apply(polygons_within_buffers[[i]], 1, any) * 1
}

st_intersection_faster <- function(x,y,...){
  #faster replacement for st_intersection(x, y,...)
  
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}

# Check intersections for artisanal mines and calculate artisanal area within each buffer distance
for (i in seq_along(buffer_distances)) {
  intersection <- st_intersection_faster(buffers[[i]], artisanal_mine)
  intersection$artisanal_area <- st_area(intersection)
  
  intersection_artisanal <- intersection %>%
    group_by(clust) %>%
    summarize(artisanal_area = sum(artisanal_area)) %>%
    st_drop_geometry()
  
  # Join the artisanal area data to the main cluster data
  cluster <- left_join(cluster, intersection_artisanal, by = "clust")
  
  # Rename the column to indicate the buffer distance
  names(cluster)[names(cluster) == "artisanal_area"] <- paste0("artisanal_area_", buffer_distances[i] / 1000, "km")
}

# View the updated cluster data frame
head(cluster)


