
library(sf)
library(dplyr)
library(tmap)
library(tidyverse)
library(countrycode)
library(geosphere)

# Mining polygons
mines <- st_read("/data/jde/mines/global_mining_polygons_v2.gpkg")
# Subset to Tanzania and use centroids for faster processing
#m <- mines |> filter(ISO3_CODE == "TZA") |> st_centroid()

africa_codes <- codelist %>%
  filter(continent == "Africa") %>%
  pluck("iso3c")

m <- mines |> filter(ISO3_CODE %in% africa_codes) |> st_centroid()

# https://data.hydrosheds.org/file/hydrobasins/standard/hybas_af_lev01-12_v1c.zip
lvl <- "12"
s <- st_read(paste0("/data/jde/hybas_lakes/hybas_lake_af_lev", lvl, "_v1c.shp"))
s <- st_make_valid(s)
s <- dplyr::filter(s, LAKE == 0)
d <- s |> select(HYBAS_ID, NEXT_DOWN) |> st_drop_geometry()
# s |> select(NEXT_DOWN) |> plot()

int <- st_intersects(s, m)
# Get the IDs of the basins that contain mines
treated_id <- s[["HYBAS_ID"]][lengths(int) > 0]

stream <- \(id, n = 1L, max = 11L, down = TRUE) {
  if(n >= max) return()
  if(down) {
    id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
  } else { # Upstream
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
  }
  if(all(id_next == 0) || length(id_next) == 0) return()
  return(c(id_next, Recall(id_next, n = n + 1L)))
}
downstream_ids <- lapply(treated_id, stream, down = TRUE)

upstream_ids <- lapply(treated_id, stream, down = FALSE)
# Todo:
#   We should probably at least track the order of a basin

s <- s |> mutate(
  status = ifelse(HYBAS_ID %in% treated_id, "mine",
    ifelse(HYBAS_ID %in% unlist(downstream_ids), "downstream",
      ifelse(HYBAS_ID %in% unlist(upstream_ids), "upstream",
        NA_character_)))
  )
s |> pull(status) |> table()

# Plot areas
# t <- st_crop(s, st_bbox(s |> filter(!is.na(status)))) |>
#   tm_shape() +
#   tm_fill("status", colorNA = "transparent") +
#   tm_borders() +
#   tm_shape(m) +
#   tm_dots(col = "black", border.col = "white", size = .3, shape = 23)
# tmap_save(t, paste0("basins_mines-l", lvl, ".png"))


# Extracting the treated and untreated basins to prepare in GEE
downstream_ids_vector <- unlist(downstream_ids)
upstream_ids_vector <- unlist(upstream_ids)

# extracting treated and untreated polygons 
relevant_basins <- s %>%
  filter(HYBAS_ID %in% c(treated_id, upstream_ids_vector, downstream_ids_vector))

write_sf(relevant_basins, "~/minesfood/data/relevant_basins.gpkg")



<<<<<<< HEAD
=======
# creating distances dataframe
downstream_ids_with_name <- downstream_ids
names(downstream_ids_with_name) <- treated_id


upstream_ids_with_name <- upstream_ids
names(upstream_ids_with_name) <- treated_id


str(upstream_ids_with_name)
str(downstream_ids_with_name)



# Convert your sf dataset to a data frame for easier handling
relevant_basins <- read_sf("~/minesfood/data/relevant_basins.shp")

relevant_basins_centroid <- st_centroid(relevant_basins)

basins_df <- relevant_basins_centroid

# Function to calculate the distance between two points
calculate_distance <- function(id1, id2, basins_df) {
  basin1 <- basins_df[basins_df$HYBAS_ID == id1,]
  basin2 <- basins_df[basins_df$HYBAS_ID == id2,]
  
  if (nrow(basin1) == 0 || nrow(basin2) == 0) {
    return(NA)  # If either basin is not found, return NA
  }
  
  # Extract coordinates
  coords1 <- st_coordinates(st_centroid(basin1))
  coords2 <- st_coordinates(st_centroid(basin2))
  
  # Calculate the distance
  dist <- distHaversine(coords1, coords2)
  return(dist)
}

# Initialize an empty list to store the results
distances_list <- list()

# Loop through each basin and calculate distances to its downstream basins
for (basin_id in names(downstream_ids_with_name)) {
  downstream_ids <- downstream_ids_with_name[[basin_id]]
  
  if (!is.null(downstream_ids)) {
    for (downstream_id in downstream_ids) {
      distance <- calculate_distance(as.numeric(basin_id), as.numeric(downstream_id), basins_df)
      distances_list[[length(distances_list) + 1]] <- data.frame(
        basin_id = basin_id,
        downstream_id = downstream_id,
        distance = distance
      )
    }
  }
}

downstream_distances_df <- bind_rows(distances_list)






# Function to calculate the distance between two points
calculate_distance <- function(id1, id2, basins_df) {
  basin1 <- basins_df[basins_df$HYBAS_ID == id1,]
  basin2 <- basins_df[basins_df$HYBAS_ID == id2,]
  
  if (nrow(basin1) == 0 || nrow(basin2) == 0) {
    return(NA)  # If either basin is not found, return NA
  }
  
  # Extract coordinates
  coords1 <- st_coordinates(st_centroid(basin1))
  coords2 <- st_coordinates(st_centroid(basin2))
  
  # Calculate the distance
  dist <- distHaversine(coords1, coords2)
  return(dist)
}

# Initialize an empty list to store the results
upstream_distances_list <- list()

# Loop through each basin and calculate distances to its upstream basins
for (basin_id in names(upstream_ids_with_name)) {
  upstream_ids <- upstream_ids_with_name[[basin_id]]
  
  if (!is.null(upstream_ids)) {
    for (upstream_id in upstream_ids) {
      distance <- calculate_distance(as.numeric(basin_id), as.numeric(upstream_id), basins_df)
      upstream_distances_list[[length(upstream_distances_list) + 1]] <- data.frame(
        basin_id = basin_id,
        upstream_id = upstream_id,
        distance = distance
      )
    }
  }
}

# Combine the list into a data frame
upstream_distances_df <- bind_rows(upstream_distances_list)





downstream_distances_df <- downstream_distances_df %>%
  mutate(downstream = 1) %>%
  rename(HYBAS_ID = downstream_id)  %>%
  rename(mine_basin = basin_id)

upstream_distances_df <- upstream_distances_df %>%
  mutate(downstream = 0) %>%
  rename(HYBAS_ID = upstream_id) %>%
  rename(mine_basin = basin_id)

downstream_upstream_distance <- rbind(downstream_distances_df, upstream_distances_df)

write.csv(downstream_upstream_distance, "~/minesfood/data/downstream_upstream_distance.csv", row.names = FALSE)

