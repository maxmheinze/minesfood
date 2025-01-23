# Load Packages -----------------------------------------------------------

pacman::p_load(
  sf,
  dplyr,
  tidyverse,
  countrycode,
  geosphere,
  rnaturalearth
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

# Settings ---

# The closest MAX_ORDER basins are considered treated/controls
MAX_ORDER <- 10
# Add IPIS locations of artisanal mines (a lot and noisy)
ADD_IPIS <- FALSE
# Keep mines > downstream > distance OR mines > distance > downstream
ORDER_OVER_DOWNSTREAM <- FALSE
# Intersect using centroids (legacy, otherwise use the full polygons)
CENTROID_INT <- FALSE
# Filter out lakes (legacy, breaks basin-links)
FILTER_LAKES <- FALSE

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
s <- st_make_valid(s) |> # Overwrite artifical long-distance links
  mutate(NEXT_DOWN = ifelse(DIST_SINK == 0, 0, NEXT_DOWN))

if(FILTER_LAKES) {
  s <- s |> filter(LAKE != 1)
}

d <- s |> select(HYBAS_ID, NEXT_DOWN) |> st_drop_geometry()

# Intersect the basins and mines
int <- st_intersects(s, m)

# Get the IDs of the basins that contain mines
treated_id <- s[["HYBAS_ID"]][lengths(int) > 0]
# Mines in a given basin
mine_ids <- int[lengths(int) > 0]
names(mine_ids) <- s[["HYBAS_ID"]][lengths(int) > 0]

# Get Downstream/Upstream Basins ------------------------------------------

# Functions from R/11_stream.R -- return up-/downstream basins of an ID

# Get basins downstream and upstream of mines
downstream_ids <- lapply(treated_id, stream, down = TRUE)
upstream_ids <- lapply(treated_id, stream, down = FALSE)

# Also track the orders of basins
downstream_order <- lapply(treated_id, stream_ordered, down = TRUE)
upstream_order <- lapply(treated_id, stream_ordered, down = FALSE)


# Relevant Basins Shapefile -----------------------------------------------

# Create a shapefile for GEE
s_relevant <- s |> mutate(
  status = ifelse(HYBAS_ID %in% treated_id, "mine",
    ifelse(HYBAS_ID %in% unlist(downstream_ids), "downstream",
      ifelse(HYBAS_ID %in% unlist(upstream_ids), "upstream", NA_character_)))
  )
s_relevant |> pull(status) |> table()

s_relevant <- s_relevant |> filter(!is.na(status))
s_relevant$basin_area_km2 <- units::drop_units(st_area(s_relevant) / 1e6)

# Store the result
file <- p("processed/basins/relevant_basins-order-", MAX_ORDER,
  if(ADD_IPIS) "-ipis" else "",
  if(FILTER_LAKES) "-nolake" else "",
  if(CENTROID_INT) "-centroid.gpkg" else ".gpkg")
write_sf(s_relevant, file) # GPKG
write_sf(s_relevant, gsub("gpkg$", "shp", file)) # SHP for GEE


# Calculate distances -----------------------------------------------------

# Centroid Distances
basin_centroids <- s_relevant %>% st_centroid() %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, DIST_SINK) %>%
  mutate(NEXT_DOWN = ifelse(DIST_SINK == 0, 0, NEXT_DOWN))

distances_centroids <- basin_centroids %>%
  st_drop_geometry() %>% # Remove geom for faster join
  left_join(basin_centroids, by = join_by("NEXT_DOWN" == "HYBAS_ID")) %>%
  mutate(geometry.x = basin_centroids$geometry) %>% # Re-add geom
  rename(geometry.y = geometry) %>% relocate(geometry.y, .after = geometry.x) %>%
  mutate(distance = ifelse(NEXT_DOWN == 0, 0, # Distance in km²
    as.numeric(st_distance(st_sfc(geometry.x), st_sfc(geometry.y), by_element = TRUE)) / 1e3)) %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, distance)

# River Flow Distances
distances_river <- s_relevant %>%
  st_drop_geometry() %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, NEXT_SINK, DIST_SINK, DIST_MAIN) %>%
  left_join(., ., by = join_by("NEXT_DOWN" == "HYBAS_ID")) %>%
  mutate(distance = ifelse(NEXT_DOWN == 0, DIST_SINK.x, DIST_SINK.x - DIST_SINK.y)) %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, distance)

# Basins sizes
basin_area <- data.frame(HYBAS_ID = s_relevant$HYBAS_ID,
  basin_area_km2 = units::drop_units(st_area(s_relevant) / 1e6))


# Elaborated Distances ----------------------------------------------------

# Functions from R/11_stream.R -- get a distance from a dataframe

# Downstream ---
ds_dists <- downstream_order

for (i in seq_along(ds_dists)) {
  ds_dists[[i]] <- rbind( # Add the mine basin as order 0
    c(treated_id[i], 0), ds_dists[[i]])
}

# Build dataframes with distances
for (i in seq_along(ds_dists)) {
  elem <- data.frame(
    basin_id = ds_dists[[i]][, 1],
    mine_basin = treated_id[[i]],
    dist_n = ds_dists[[i]][, 2],
    dist_km = 0, dist_km_centroid = 0
  )
  d <- d_centroid <- rep(0, NROW(elem))
  if (nrow(elem) > 1) { for (j in 2:nrow(elem)) { # First row is the mine
    id_prev <- elem[j - 1, 1]
    id_curr <- elem[j, 1]
    d[j] <- get_distance(id_curr, id_prev, distances_river)
    d_centroid[j] <- get_distance(id_curr, id_prev, distances_centroids)
  } }
  elem$dist_km <- cumsum(d)
  elem$dist_km_centroid <- cumsum(d_centroid)
  ds_dists[[i]] <- elem
}

# Upstream ---
us_dists <- upstream_order

for (i in seq_along(us_dists)) { # Add the mine basin as order 0
  us_dists[[i]] <- rbind( c(treated_id[i], 0), us_dists[[i]])
  colnames(us_dists[[i]]) <- c("id_next", "n")
  # Add the ID of the next basin
  us_dists[[i]] <- dplyr::left_join(
    data.frame(us_dists[[i]]),
    data.frame(distances_river[, 1:2]), by = join_by("id_next" == "HYBAS_ID"))
  us_dists[[i]][1, 3] <- NA # Nothing for the mine basin
}

# Build dataframes with distances
for (i in seq_along(us_dists)) {
  elem <- us_dists[[i]]
  d <- d_centroid <- rep(0, NROW(elem))
  if (nrow(elem) > 1) { for (j in 2:nrow(elem)) {
    id1 <- elem[j, 1]
    id2 <- elem[j, 3]
    d[j] <- get_distance_upstream(id1, id2, distances_river)
    d_centroid[j] <- get_distance_upstream(id1, id2, distances_centroids)
  } }
  # Accumulate distances (we can no longer accumulate over rows)
  acc_d <- acc_d_centroid <- numeric(nrow(elem))
  for (j in 1:nrow(elem)) {
    curr_d <- d[j]
    curr_d_centroid <- d_centroid[j]
    id2 <- elem[j, 3]
    while (!is.na(id2) && id2 != elem[1, 1]) {
      idx <- which(elem[, 1] == id2)
      if (length(idx) == 0) break # Traverse upstream until the first row
      curr_d <- curr_d + d[idx]
      curr_d_centroid <- curr_d_centroid + d_centroid[idx]
      id2 <- elem[idx, 3]
    }
    acc_d[j] <- curr_d
    acc_d_centroid[j] <- curr_d_centroid
  }
  # Create the dataframe
  us_dists[[i]] <- data.frame(
    basin_id = elem[, 1],
    dist_n = elem[, 2],
    mine_basin = treated_id[i],
    dist_km = acc_d,
    dist_km_centroid = acc_d_centroid
  )
}

# Merge upstream, downstream and mines ---
basins_ordered <- rbind(
  bind_rows(ds_dists) |> mutate(status = "downstream"),
  bind_rows(us_dists) |> mutate(status = "upstream")
) |> # Add mine status and -/+ dist_order
  mutate(
    status = ifelse(basin_id == mine_basin, "mine", status),
    dist_order = dist_n * ifelse(status == "upstream", -1, 1)
  )

# Basins may appear multiple times – we keep one each
if(ORDER_OVER_DOWNSTREAM) {
  # Basins may appear multiple times – we keep one each
  basins_ordered_unique <- basins_ordered |>
    group_by(basin_id) |>
    arrange( # We keep basin information in the order of
      status != "mine", # mines trump all
      dist_n, # closer order
      status != "downstream", # downstream > upstream
      dist_km, # closer > farther
      .by_group = TRUE) |>
    slice_head(n = 1) # Only keep the first per basin_id
} else {
  basins_ordered_unique <- basins_ordered |>
    group_by(basin_id) |>
    arrange( # We keep basin information in the order of
      status != "mine", # mines trump all
      status != "downstream", # downstream > upstream
      dist_n, dist_km, # closer > farther
      .by_group = TRUE) |>
    slice_head(n = 1) # Only keep the first per basin_id
}


# Add other info related to the mines ---

# Which mines are in which basin
basin_mines <- data.frame(
  "mine_basin" = rep(names(mine_ids), lengths(mine_ids)),
  "mine_id" = mine_ids |> purrr::map_dfr(as_tibble) |> pull(value)) |>
  left_join(m |> st_drop_geometry() |>
    transmute(
      mine_id = seq_len(nrow(m)),
      iso3c = ISO3_CODE,
      mine_area_km2 = AREA
    ), by = "mine_id")

# Keep the ISO3 of the largest mine's country
basin_mines_iso <- basin_mines |>
  group_by(mine_basin, iso3c) |>
  summarise(mine_area_km2 = sum(mine_area_km2)) |>
  group_by(mine_basin) |>
  slice_max(mine_area_km2, n = 1) |>
  select(-mine_area_km2)

# Add mine size, number, and ISO3
basin_mines <- basin_mines |>
  group_by(mine_basin) |>
  summarise(mine_area_km2 = sum(mine_area_km2), mine_number = n()) |>
  mutate(mine_avg_area_km2 = mine_area_km2 / mine_number) |>
  left_join(basin_mines_iso) |>
  mutate(mine_basin = as.double(mine_basin),
    iso3c = replace(iso3c, iso3c == "ESH", "MAR"))

# Which basins are associated with which mine basins?
mine_basins <- basins_ordered |>
  group_by(basin_id) |>
  summarise(mine_basins = paste(unique(mine_basin), collapse = ", "))
  # summarise(mine_basins = list(unique(mine_basin)))

so <- basins_ordered_unique |>
  left_join(s, by = join_by("basin_id" == "HYBAS_ID")) |>
  left_join(basin_area, by = join_by("basin_id" == "HYBAS_ID")) |>
  left_join(basin_mines, by = join_by("mine_basin")) |>
  left_join(mine_basins, by = join_by("basin_id")) |>
  relocate(geometry, .after = mine_basins)

file <- p("processed/basins/basins-order-", MAX_ORDER,
  if(ORDER_OVER_DOWNSTREAM) "-order-trumps-downstream-" else "",
  if(ADD_IPIS) "-ipis" else "",
  if(FILTER_LAKES) "-nolake" else "",
  if(CENTROID_INT) "-centroid.gpkg" else ".gpkg")
write_sf(so, file)
# write_sf(so, gsub("gpkg$", "shp", file))
