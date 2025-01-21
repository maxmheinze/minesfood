
pacman::p_load(
  sf,
  dplyr,
  GauPro #, # Used to create predictions
  # rnaturalnearth # Used for the prediction's extent -- bugs out
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

# Prepare the extent
countries <- rnaturalearth::ne_countries() |>
  dplyr::filter(continent == "Africa")

# Merge the proper points, multipoints, and polygon-derived points
shp <- st_read("outputs/commodity_locations.gpkg")
# Split up MULTIPOINTs and add the POLY -> POINT
shp <- rbind(
  shp[st_geometry_type(shp) == "POINT", ],
  shp[st_geometry_type(shp) == "MULTIPOINT", ] |> st_cast("POINT"),
  st_read("outputs/commodity_polys-to-points.gpkg")
)

# Hand-prepared list of commodities and naming variants
commodities <- list( # Precious metals --- mercury and cyanide used
  "gold" = c("gold", "or"),
    "silver", "platinum" = c("platinum", "PGE", "PGM", "palladium", "rhodium", "ruthenium", "iridium", "osmium"),
  "precious",
  # Gemstones --- conflict
  "diamonds" = c("diamond", "diamonds", "gems", "gem", "diamant"),
  # Energy --- large pits, acidification, dust
  "coal", "uranium" = c("uranium", "u308"),
  # Base metals --- large pits
  "copper" = c("copper", "cuivre"), "iron", "zinc", "lead", "nickel",
  # Industrial
  "industrial minerals",
  "chromium" = c("chromium", "chromite", "chrome"), "cobalt", "manganese",
  "tantalum" = c("tantalum", "coltan"),
  # Batteries
  "lithium", "vanadium",
  # Various
  "tungsten" = c("tungsten", "wolframite"),
  "tin" = c("tin", "cassitérite"),
  "titanium", "sands" = c("mineral sands", "rutile", "zircon", "ilmenite", "leucoxene"), "rare earth", "germanium", "gallium", "indium", "aluminium" = c("aluminum", "aluminium", "alumina", "bauxite"), "potassium" = c("potassium", "potash"),
  "phosphate" = c("phosphate", "phosphorous"), "graphite", "molybdenum", "zirconium", "beryllium", "fluor", "niobium", "asbestos", "limestone", "talc", "barite", "lignite", "sulfur"
)
names(commodities) <- ifelse(names(commodities) != "", names(commodities),
  vapply(commodities, \(x) x[1], character(1L)))

# Co-occurences: Copper-Lead-Zinc, Gold-Silver-PGE, Iron-Manganese-Chromium

# Now we need to create usable variables
for(comm in names(commodities)) {
  shp[[comm]] <- sapply(commodities[[comm]], \(c) { # There are many patterns
    grepl(c, shp[["commodity"]], ignore.case = TRUE)
  }) |> rowSums() |> pmax(0) |> pmin(1) # Yes or no
}
# Which observations remain unmatched?
print("Commodities (+ count) that were not matched with commodities of interest:")
tmp <- shp |> st_drop_geometry()
tmp[tmp[, -1] |> rowSums() == 0, 1L] |> table() |> sort() |> print()
# Looking reasonable

# How many matches do we have?
print("Commodities (+ count) that were matched:")
comm_matches <- shp[, -1] |> st_drop_geometry() |> colSums() |> sort()
comm_matches |> print()
# Subset to ones with enough locations
comm_matches <- comm_matches[comm_matches > 45]


# Prepare the regional mask (and projection) ---
shp <- st_transform(shp, "+proj=moll")
countries <- st_transform(countries, "+proj=moll")
continent_plain <- countries |> st_geometry() |> st_combine() |> st_make_valid() |>
  st_union()
continent <- continent_plain |> st_buffer(1000000)
plot(continent)
plot(countries, add = TRUE, color = "white")


# Now we are ready to model ---

# Squish predictions to [0, 1]
z <- \(x) (x - min(x)) / max(x - min(x))

mines <- st_read(p("mines/global_mining_polygons_v2.gpkg")) |>
  dplyr::filter(ISO3_CODE %in% countries[["iso_a3"]])
mine_crs <- st_crs(mines)
mines <- st_transform(mines, "+proj=moll")
mine_coords <- mines |> st_geometry() |> st_centroid() |> st_coordinates()

# We work on scaled coordinates
scaled_coords <- scale(mine_coords)
scale_coords <- \(x, y = scaled_coords) {
  sweep(sweep(x, 2, attr(y, "scaled:center"), "-"),
    2, attr(y, "scaled:scale"), "/")
}
rescale_coords <- \(x, y = scaled_coords) {
  sweep(sweep(x, 2, attr(y, "scaled:scale"), "*"),
    2, attr(y, "scaled:center"), "+")
}

# Dataframe to predict on for visualization
bbox <- st_bbox(continent_plain) # Mollweide ftw
pred <- expand.grid( # ~80km^2 per pixel
  x = seq(-2.4, 2.25, length.out = round((bbox["xmax"] - bbox["xmin"]) / 80000)),
  y = seq(-1.4, 2.8, length.out = round((bbox["ymax"] - bbox["ymin"]) / 80000))
)
pred[, c("X", "Y")] <- pred[, c("x", "y")] |> rescale_coords() # Model space to true

set.seed(42)
# for(comm in names(comm_matches)) {
for(comm in names(comm_matches)[c(-1, -2)]) {
  cat("Predicting", comm, "\n")

  # Where is comm present
  ones <- shp[shp[[comm]] == 1, c("geom", comm)]
  # Where it isn't
  zeros <- shp[shp[[comm]] == 0, c("geom", comm)]

  # Impute zeros elsewhere (farthest point sampling would be even better)
  # zeros <- st_sf(geom = continent |> # We want 5000 points
  #     st_sample(type = "regular", size = 1000))
  # zeros[[comm]] <- 0
  # st_crs(zeros) <- st_crs(ones) # Somehow needed again

  # Consider adding some noise to avoid model collapse
  noise <- st_sf(geom = continent |> # Do this everywhere
      st_bbox() |> st_as_sfc() |> # We want the equivalent of the zeros
    st_sample(type = "regular", size = 1000))
  noise[[comm]] <- rbeta(NROW(noise), 5, 100) # Mean 2 / 42, all should be below .4
  st_crs(noise) <- st_crs(ones) # Somehow needed again

  # Filter out overlaps via distance
  dist_zeros <- 1e6 # st_distance(zeros, ones) |> apply(1, min)
  dist_noise <- st_distance(noise, ones) |> apply(1, min)
  df <- rbind(
    ones,
    zeros[dist_zeros > 250000, ],
    noise[dist_noise > 250000, ]) # 250 km buffer around known sites
  coords <- st_coordinates(df) |> scale_coords()

  # Model
  k <- GauPro::Gaussian$new(beta = c(0, 0)) # Matern tends to collapse on few locations
  trend <- trend_0$new() # trend_c$new()
  model <- gpkm(coords, df[[comm]], kernel = k, trend = trend,
    nug.min = 1e-12, restarts = 1)

  # Plot the predictions
  # model$plot()
  pred$probability <- model$predict(pred[, c("x", "y")]) |> z()
  qu_cutoff <- .5 #quantile(pred$probability, .9)
  g <- pred |> filter(probability > qu_cutoff) |>
    ggplot() +
    geom_sf(data = continent_plain, aes()) +
    geom_tile(aes(x = X, y = Y, fill = probability)) +
    geom_point(data = as.data.frame(mine_coords), aes(x = X, y = Y),
      pch = 20, alpha = .5, size = .5) +
    geom_tile(aes(x = X, y = Y, fill = probability), alpha = .5) +
    ggtitle(paste("Mine locations & predictions for:", comm)) +
    theme_void() + scale_fill_viridis_c(begin = qu_cutoff / 2)
  ggsave(paste0("outputs/comm-pred_", comm, ".png"), g)

  # Predict on the mine locations
  mines[[comm]] <- scaled_coords |> model$predict() |> z()
  # plot(continent)
  # plot(mines |> select(comm) |> st_centroid(), add = TRUE, pch = 18)
  saveRDS(mines, "outputs/tmp.rds")
}

mines <- st_transform(mines, mine_crs)
saveRDS(mines, "outputs/africa_mining_polygons_v2-pred-commodity.rds")
st_write(mines, "outputs/africa_mining_polygons_v2-pred-commodity.gpkg")
