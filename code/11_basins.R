# Load Packages -----------------------------------------------------------

rm(list = ls())

pacman::p_load(
  sf,
  dplyr,
  tmap,
  tidyverse,
  countrycode,
  geosphere,
  rnaturalearth
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

CENTROID_INT <- FALSE # Whether to just match on centroids
MAX_ORDER <- 25 # Subset to the first N basins
ADD_IPIS <- TRUE# Add IPIS ASM point data

# Load and Prepare Mine Data ----------------------------------------------

mines <- st_read(p("mines/global_mining_polygons_v2.gpkg"))

africa_codes <- codelist |>
  filter(continent == "Africa") |>
  pluck("iso3c")

if(CENTROID_INT) { # Centroid-based
  m <- mines |> filter(ISO3_CODE %in% africa_codes) |> st_centroid()
} else {# Full
  m <- mines |> filter(ISO3_CODE %in% africa_codes)
}

if(ADD_IPIS) {
  m <- c("CAF", "TZA", "DRC", "ZWE") |>
    lapply(\(f) st_read(p("mines/IPIS-ASM_", f, ".gpkg")) |>
        transmute(ISO3_CODE = f, AREA = 0, source = "IPIS")
      )
  m <- mines |> transmute(ISO3_CODE, AREA, source = "POLYS") |>
    rbind(do.call(rbind, m))
}

# Load and Prepare HydroBASINS Data ---------------------------------------

# https://data.hydrosheds.org/file/hydrobasins/standard/hybas_af_lev01-12_v1c.zip
lvl <- "12"
s <- st_read(p("hybas_lakes/hybas_lake_af_lev", lvl, "_v1c.shp"))
s <- st_make_valid(s)
s <- s |> mutate(NEXT_DOWN = ifelse(DIST_SINK == 0, 0, NEXT_DOWN))

# Filter out lakes -- or not
# lake_id <- s |> st_drop_geometry() |> filter(LAKE != 0) |> pull(HYBAS_ID)
# s <- dplyr::filter(s, LAKE == 0)
lake_id <- 0
d <- s |> select(HYBAS_ID, NEXT_DOWN) |> st_drop_geometry()

int <- st_intersects(s, m)

# Get the IDs of the basins that contain mines
treated_id <- s[["HYBAS_ID"]][lengths(int) > 0]


# Get Downstream/Upstream Basins ------------------------------------------

# Function that determines up- and downstream basins from NEXT_DOWN field
stream <- \(id, n = 1L, max = MAX_ORDER + 1, down = TRUE) {
 if(n >= max) return()
 if(down) {
   id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
 } else { # Upstream
   id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
 }
 if(all(id_next == 0) || length(id_next) == 0) return()
 return(c(id_next, Recall(id_next, n = n + 1L, max = max, down = down)))
}

stream_ordered <- function(id, n = 1L, max = MAX_ORDER + 1, down = TRUE) {
  if (n >= max) return(NULL)
  if (down) {
    id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
  } else {
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
  }
  if (all(id_next == 0) || length(id_next) == 0) return()
  results <- cbind(id_next, n)
  id_next <- unique(id_next)
  recursive_results <- Recall(id_next, n = n + 1L, max = max, down = down)
  if (!is.null(recursive_results)) {
    results <- rbind(results, recursive_results)
  }
  return(results)
}

# Get basins downstream and upstream of mines
downstream_ids <- lapply(treated_id, stream, down = TRUE)
upstream_ids <- lapply(treated_id, stream, down = FALSE)

# Also track the orders of basins
downstream_ids_ordered <- lapply(treated_id, stream_ordered, down = TRUE)
upstream_ids_ordered <- lapply(treated_id, stream_ordered, down = FALSE)


# Relevant Basins Shapefile -----------------------------------------------

# Extracting the treated and untreated basins to prepare in GEE
downstream_ids_vector <- unlist(downstream_ids)
# downstream_ids_vector <- downstream_ids_vector[which(!downstream_ids_vector %in% lake_id)]
upstream_ids_vector <- unlist(upstream_ids)

su <- s |> mutate(
  status = ifelse(HYBAS_ID %in% treated_id, "mine",
    ifelse(HYBAS_ID %in% downstream_ids_vector, "downstream",
      ifelse(HYBAS_ID %in% upstream_ids_vector, "upstream", NA_character_)))
  )
su |> pull(status) |> table()

# Plot areas
# t <- st_crop(s, st_bbox(s |> filter(!is.na(status)))) |>
#   tm_shape() +
#   tm_fill("status", colorNA = "transparent") +
#   tm_borders() +
#   tm_shape(m) +
#   tm_dots(col = "black", border.col = "white", size = .3, shape = 23)
# tmap_save(t, paste0("basins_mines-l", lvl, ".png"))


# extracting treated and untreated polygons
relevant_basins <- su |> filter(!is.na(status))
relevant_basins$basin_area_km2 <- units::drop_units(st_area(relevant_basins) / 10^6)

file <- p("processed/relevant_basins",
  if(ADD_IPIS) "-ipis" else "",
  if(CENTROID_INT) "-centroid.gpkg" else ".gpkg")
write_sf(relevant_basins, file)

# Necessary for Earth Engine
write_sf(relevant_basins, gsub("gpkg$", "shp", file))


# Calculate distances ---

# # Centroid Distances
basin_centroids <- relevant_basins %>%
  st_centroid %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, DIST_SINK) %>%
  mutate(NEXT_DOWN = ifelse(DIST_SINK == 0, 0, NEXT_DOWN))

basin_distances_centroids <- basin_centroids %>%
  st_drop_geometry() %>%
  left_join(basin_centroids, by = join_by("NEXT_DOWN" == "HYBAS_ID")) %>%
  dplyr::select(-NEXT_DOWN.y) %>%
  mutate(geometry.x = basin_centroids$geometry) %>%
  rename(geometry.y = geometry) %>%
  relocate(geometry.y, .after = geometry.x) %>%
  mutate(distance = ifelse(NEXT_DOWN == 0, 0,
    as.numeric(st_distance(st_sfc(geometry.x), # distance in km
      st_sfc(geometry.y), by_element = TRUE)) / 10^3)) %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, distance)

# River Flow Distances
basin_distances_river <- relevant_basins %>%
  st_drop_geometry() %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, NEXT_SINK, DIST_SINK, DIST_MAIN) %>%
  left_join(., ., by = join_by("NEXT_DOWN" == "HYBAS_ID")) %>%
  mutate(distance = ifelse(NEXT_DOWN == 0, DIST_SINK.x, DIST_SINK.x - DIST_SINK.y)) %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, distance)

basin_area <- data.frame(HYBAS_ID = relevant_basins$HYBAS_ID,
  basin_area_km2 = units::drop_units(st_area(relevant_basins) / 10^6))


# Elaborated Distances -----------------------------------------

get_distance <- function(id1, id2, distances_df, downstream = TRUE) {
  if (is.na(id1) | is.na(id2) | id1 == id2) return(0)
  row <- if(downstream) {
    distances_df[distances_df$HYBAS_ID == id2 & distances_df$NEXT_DOWN == id1, ]
  } else {
    distances_df[distances_df$HYBAS_ID == id1 & distances_df$NEXT_DOWN == id2, ]
  }
  if (nrow(row) == 0) stop("No distance found.")
  return(row$distance)
}
get_distance_upstream <- \(...) get_distance(..., downstream = FALSE)


# Downstream ---
downstream_distances_list <- downstream_ids_ordered

for (i in seq_along(downstream_distances_list)) { # Add the mine basin as order 0
  downstream_distances_list[[i]] <- rbind(
    c(treated_id[i], 0),
    downstream_distances_list[[i]])
}

for (i in seq_along(downstream_distances_list)) {
  elem <- data.frame(
    basin_id = downstream_distances_list[[i]][, 1],
    dist_n = downstream_distances_list[[i]][, 2],
    mine_basin = treated_id[[i]],
    dist_km = 0, dist_km_centroid = 0
  )
  distances <- distances_centroid <- rep(0, NROW(elem))
  if (nrow(elem) > 1) {
    for (j in 2:nrow(elem)) { # First row is the mine
      id_prev <- elem[j - 1, 1]
      id_curr <- elem[j, 1]
      distances[j] <- get_distance(id_curr, id_prev, basin_distances_river)
      distances_centroid[j] <- get_distance(id_curr, id_prev, basin_distances_centroids)
    }
  }
  elem$dist_km <- cumsum(distances)
  elem$dist_km_centroid <- cumsum(distances_centroid)
  downstream_distances_list[[i]] <- elem
}


# Upstream ---
upstream_distances_list <- upstream_ids_ordered

for (i in seq_along(upstream_distances_list)) { # Add the mine basin as order 0
  upstream_distances_list[[i]] <- rbind(
    c(treated_id[i], 0),
    upstream_distances_list[[i]])
  colnames(upstream_distances_list[[i]]) <- c("id_next", "n")

  upstream_distances_list[[i]] <- dplyr::left_join(
    data.frame(upstream_distances_list[[i]]),
    data.frame(basin_distances_river[, 1:2]), by = join_by("id_next" == "HYBAS_ID"))
  upstream_distances_list[[i]][1, 3] <- NA # Nothing for the mine basin
}


for (i in seq_along(upstream_distances_list)) {
  elem <- upstream_distances_list[[i]]

  distances <- distances_centroid <- rep(0, NROW(elem))
  if (nrow(elem) > 1) {
    for (j in 2:nrow(elem)) {
      id1 <- elem[j, 1]
      id2 <- elem[j, 3]
      distances[j] <- get_distance_upstream(id1, id2, basin_distances_river)
      distances_centroid[j] <- get_distance_upstream(id1, id2, basin_distances_centroids)
    }
  }

  # We can no longer accumulate over rows
  accumulated_distances <- accumulated_distances_centroid <- numeric(nrow(elem))
  for (j in 1:nrow(elem)) {
    current_distance <- distances[j]
    current_distance_centroid <- distances_centroid[j]
    id2 <- elem[j, 3]
    while (!is.na(id2) && id2 != elem[1, 1]) {
      idx <- which(elem[, 1] == id2)
      if (length(idx) == 0) break # Traverse upstream until the first row
      current_distance <- current_distance + distances[idx]
      current_distance_centroid <- current_distance_centroid + distances_centroid[idx]
      id2 <- elem[idx, 3]
    }
    accumulated_distances[j] <- current_distance
    accumulated_distances_centroid[j] <- current_distance_centroid
  }

  upstream_distances_list[[i]] <- data.frame(
    basin_id = elem[, 1],
    dist_n = elem[, 2],
    mine_basin = treated_id[i],
    dist_km = accumulated_distances,
    dist_km_centroid = accumulated_distances_centroid
  )
}


# Merge upstream, downstream and mines

basins_ordered <- rbind(
  bind_rows(downstream_distances_list) |> mutate(status = "downstream"),
  bind_rows(upstream_distances_list) |> mutate(status = "upstream")
) |> # Add mine status and -/+ order
  mutate(
    status = ifelse(basin_id == mine_basin, "mine", status),
    dist_order = dist_n * ifelse(status == "upstream", -1, 1)
  )

# Basins may appear multiple times – we keep info on
#   1) mines
#   2) downstream > upstream
#   3) close > far
basins_ordered_unique <- basins_ordered |>
  group_by(basin_id) |>
  arrange(status != "mine", status != "downstream",
    dist_order, dist_km, .by_group = TRUE) |>
  slice_head(n = 1)

so <- basins_ordered_unique |>
  left_join(s, by = join_by("basin_id" == "HYBAS_ID"))

file <- p("processed/relevant_basins_ordered",
  if(ADD_IPIS) "-ipis" else "",
  if(CENTROID_INT) "-centroid.gpkg" else ".gpkg")
write_sf(so, file)
write_sf(so, gsub("gpkg$", "shp", file))


# TODO
# so <- s |>
#   left_join(basins_ordered_unique, by = join_by("HYBAS_ID" == "basin_id")) |>
#   dplyr::filter(!is.na(status)) |>
#   dplyr::select(HYBAS_ID, mine_basin, status, order) |>
#   left_join(basin_area) |>
#   left_join(basin_mines) |>
#   left_join(s |> st_drop_geometry() |> transmute(HYBAS_ID, basin_pfaf_id = PFAF_ID)) |>
#   relocate(iso3c, .after = mine_basin) |>
#   relocate(geometry, .after = mine_area_km2)


# Gather some info ---
mine_ids <- int[lengths(int) > 0]
names(mine_ids) <- s[["HYBAS_ID"]][lengths(int) > 0]

basin_mines <- data.frame(
  "mine_basin" = rep(names(mine_ids), lengths(mine_ids)),
  "mine_id" = mine_ids |> purrr::map_dfr(as_tibble) |> pull(value))
basin_mines <- basin_mines |>
  left_join(m |> st_drop_geometry() |> transmute(
    mine_id = seq_len(nrow(m)), iso3c = ISO3_CODE, mine_area_km2 = AREA),
    by = "mine_id")

# Assign country by larger area of mine and add mine areas
basin_mines_iso <- basin_mines |>
  group_by(mine_basin, iso3c) |>
  summarise(mine_area_km2 = sum(mine_area_km2)) |>
  group_by(mine_basin) |>
  slice_max(mine_area_km2, n = 1) |>
  select(-mine_area_km2)
basin_mines <- basin_mines |>
  group_by(mine_basin) |>
  summarise(mine_area_km2 = sum(mine_area_km2), mine_number = n()) |>
  mutate(mine_avg_area_km2 = mine_area_km2 / mine_number) |>
  left_join(basin_mines_iso) |>
  left_join(s |> st_drop_geometry() |>
      transmute(mine_basin = as.character(HYBAS_ID), mine_basin_pfaf_id = PFAF_ID)) |>
  mutate(mine_basin = as.double(mine_basin),
         iso3c = replace(iso3c, iso3c == "ESH", "MAR")) |>
  relocate(iso3c:mine_basin_pfaf_id, .after = mine_basin)

# Add Order to Downstream/Upstream Distance DF ----------------------------

# This is legacy code that is redundant (I think) but I'm not sure whether
# we use this some time later so I just ensured consistency and left it
# in --Max

downstream_upstream_distance_ordered <- downstream_upstream_distance |>
  transmute(HYBAS_ID = as.double(HYBAS_ID), mine_basin = as.double(mine_basin),
            downstream, distance, distance_centroid) |>
  full_join(basins_ordered_unique, by = join_by("HYBAS_ID" == "basin", "mine_basin")) |>
  drop_na(status) |>
  mutate_at(vars(downstream, distance, distance_centroid), ~replace_na(., 0)) |>
  left_join(basin_area) |>
  left_join(basin_mines) |>
  left_join(s |> st_drop_geometry() |> transmute(HYBAS_ID, basin_pfaf_id = PFAF_ID)) |>
  mutate(mine_area_km2 = replace(mine_area_km2, mine_basin != HYBAS_ID, 0)) |>
  transmute(HYBAS_ID, HYBAS_PFAF_ID = basin_pfaf_id, mine_basin, mine_basin_pfaf_id,
            iso3c, downstream, status, order, distance, distance_centroid, basin_area_km2,
            mine_area_km2, mine_number, mine_avg_area_km2)

file <- p("processed/downstream_upstream_distance_ordered",
  if(ADD_IPIS) "-ipis" else "",
  if(CENTROID_INT) "-centroid.csv" else ".csv")
write.csv(downstream_upstream_distance_ordered, file, row.names = FALSE)
