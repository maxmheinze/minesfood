
library("sf")
library("dplyr")
library("GauPro")

countries <- rnaturalearth::ne_countries() |> dplyr::filter(continent == "Africa")

# Back to sf
shp <- st_read("outputs/commodity_locations.gpkg")
# Split up MULTIPOINTs and add the POLY -> POINT
shp <- rbind(
  shp[st_geometry_type(shp) == "POINT", ],
  shp[st_geometry_type(shp) == "MULTIPOINT", ] |> st_cast("POINT"),
  st_read("outputs/commodity_polys-to-points.gpkg")
)

commodities <- list( # Precious metals --- mercury and cyanide used
  "gold", "silver", "platinum" = c("platinum", "PGE", "PGM", "palladium", "rhodium", "ruthenium", "iridium", "osmium"),
  # Gemstones --- conflict
  "diamonds" = c("diamond", "diamonds", "gems", "gem"),
  # Energy --- large pits, acidification, dust
  "coal", "uranium" = c("uranium", "u308"),
  # Base metals --- large pits
  "copper", "iron", "zinc", "lead", "nickel",
  # Industrial
  "industrial minerals", "chromium" = c("chromium", "chromite", "chrome"), "cobalt", "manganese", "tantalum",
  # Batteries
  "lithium", "vanadium",
  # Various
  "titanium", "sands" = c("mineral sands", "rutile", "zircon", "ilmenite", "leucoxene"), "rare earth", "germanium", "gallium", "indium", "alum" = c("aluminum", "aluminium", "alumina", "bauxite"), "potassium" = c("potassium", "potash"),
  "phosphate" = c("phosphate", "phosphorous"), "graphite", "tin", "tungsten", "molybdenum", "zirconium", "beryllium", "fluor", "niobium", "asbestos", "limestone", "talc", "barite", "lignite", "sulfur"
)
names(commodities) <- ifelse(names(commodities) != "", names(commodities),
  vapply(commodities, \(x) x[1], character(1L)))

# Co-occurences: Copper-Lead-Zinc, Gold-Silver-PGE, Iron-Manganese-Chromium

# Now we need to create usable variables
for(comm in names(commodities)) {
  shp[[comm]] <- sapply(commodities[[comm]], \(c) { # Many patterns
    grepl(c, shp[["commodity"]], ignore.case = TRUE)
  }) |> rowSums() |> pmax(0) |> pmin(1) # Yes or no
}
# Which observations remain unmatched?
tmp <- shp |> st_drop_geometry()
tmp[tmp[, -1] |> rowSums() == 0, 1L] |> table() |> sort()
# Looking reasonable

# How many matches do we have?
comm_matches <- shp[, -1] |> st_drop_geometry() |> colSums() |> sort()
comm_matches <- comm_matches[comm_matches > 25]

# Regional mask
shp <- st_transform(shp, "+proj=moll")
countries <- st_transform(countries, "+proj=moll")
continent <- countries |> st_geometry() |> st_combine() |> st_make_valid() |>
  st_union() |> st_buffer(100000)
plot(continent)
plot(countries, add = TRUE, color = "white")


# Now we are ready to model a mask ---
z <- \(x) (x - min(x)) / max(x - min(x))

mines <- st_read("data/mines/global_mining_polygons_v2.gpkg") |>
  dplyr::filter(ISO3_CODE %in% countries[["iso_a3"]])
mine_crs <- st_crs(mines)
mines <- st_transform(mines, "+proj=moll")

for(comm in names(comm_matches)) {
  cat("Predicting", comm, "\n")
  ones <- shp[shp[[comm]] == 1, c("geom", comm)]
  # We could be smarter about this (farthest point sampling)
  zeros <- st_sf(geom = continent |> st_sample(type = "regular", size = max(1000, NROW(ones) * 2)))
  zeros[[comm]] <- 0
  st_crs(zeros) <- st_crs(ones) # Somehow needed
  distances <- st_distance(zeros, ones) |> apply(1, min) # Filter overlaps
  df <- rbind(ones, zeros[distances > 250000, ]) # 250 km buffer

  # Model
  k <- GauPro::Matern52$new(beta = c(0, 0))
  trend <- GauPro::trend_0$new()
  coords <- df |> st_coordinates() |> scale()
  model <- gpkm(coords, df[[comm]], kernel = k, trend = trend)

  # Plot the predictions
  # model$plot()
  pred <- expand.grid(x = seq(-2.05, 2.05, length.out = 100), y = seq(-2.35, 1.75, length.out = 100))
  pred$prediction <- model$predict(pred) |> z()
  g <- ggplot(pred, aes(x = x, y = y, fill = prediction)) +
    geom_raster() +
    ggtitle(comm) +
    theme_minimal() + scale_fill_viridis_c()
  ggsave(paste0("outputs/comm-pred_", comm, ".png"), g)


  # Predict on the mine locations
  mine_coords <- mines |> st_geometry() |> st_centroid() |> st_coordinates()
  mines[[comm]] <- ((mine_coords - attr(coords, "scaled:center")) / attr(coords, "scaled:scale")) |> model$predict() |> z()
  saveRDS(mines, "tmp.rds")
}

mines <- st_transform(mines, mine_crs)
st_write(mines, "outputs/africa_mining_polygons_v2-pred-commodity.gpkg")
