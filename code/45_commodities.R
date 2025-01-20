
pacman::p_load(
  sf,
  dplyr,
  terra,
  rnaturalnearth # Used to crosscheck locations
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

# Make sure this exists
dir.create("outputs", showWarnings = FALSE)

# Sources of commodity info
files <- c(
  # Padilla et al. <https://www.sciencebase.gov/catalog/item/607611a9d34e018b3201cbbf>
  list.files(p("mines/commodities/"), pattern = "padilla"),
  # Jasansky et al. <https://www.nature.com/articles/s41597-023-01965-y>
  list.files(p("mines/commodities/"), pattern = "jasansky"),
  # <https://globalenergymonitor.org/>
  list.files(p("mines/commodities/"), pattern = "GEM")
)
shapes <- lapply(p("mines/commodities/", files), st_read)

# Check the locations ---
countries <- rnaturalearth::ne_countries() |> dplyr::filter(continent == "Africa")
countries |> st_geometry() |> plot(line = "gray")
for(shape in shapes) shape |> st_geometry() |> plot(add = TRUE)


# Check sources on various ores ---

deposits <- shapes[[grep("padilla_deposits", files)]]

comm_dep <- deposits[, paste0("DsgAttr", formatC(1:8, width = 2, flag = "0"))] |>
  st_drop_geometry() |> unlist()
comm_dep |> table() |> sort(decreasing = TRUE) |> head(40)
# Gold, copper, platinum, silver, iron, chromium, cobalt, lead, zinc, ...
deposits[, "commodity"] <-
  deposits[, grep("DsgAttr0[1-8]", names(deposits))] |> st_drop_geometry() |>
  apply(1, \(c) paste(c[!is.na(c) & !grepl("^ $", c)], collapse = ", "))


exploration <- shapes[[grep("padilla_exploration", files)]]

comm_exp <- exploration[, paste0("DsgAttr", formatC(1:8, width = 2, flag = "0"))] |>
  st_drop_geometry() |> unlist()
comm_exp |> table() |> sort(decreasing = TRUE) |> head(30)
# Gold, copper, diamond, uranium, platinum, cobalt, nickel, silver, zinc, ...
exploration[, "commodity"] <-
  exploration[, grep("DsgAttr0[1-8]", names(exploration))] |> st_drop_geometry() |>
  apply(1, \(c) paste(c[!is.na(c) & !grepl("^ $", c)], collapse = ", "))


gabon <- shapes[[grep("padilla_gabon", files)]]

comm_gab <- gabon |> pull(DsgAttr01)
comm_gab |> table() |> sort(decreasing = TRUE)
# Gold, iron, platinum, talc, diamonds, limestone, uranium, copper, ...
gabon[, "commodity"] <- gabon[["DsgAttr01"]]

mauritania <- shapes[[grep("padilla_mauritania", files)]]

comm_mau <- mauritania[, paste0("DsgAttr", formatC(1:6, width = 2, flag = "0"))] |>
  st_drop_geometry() |> unlist()
comm_mau |> table() |> sort(decreasing = TRUE)
# "Industrial minerals", uranium, copper, iron, gold, ...
mauritania[, "commodity"] <-
  mauritania[, grep("DsgAttr0[1-6]", names(mauritania))] |> st_drop_geometry() |>
  apply(1, \(c) paste(c[!is.na(c) & !grepl("^ $", c)], collapse = ", "))


jasansky <- shapes[[grep("jasansky", files)]]
jasansky <- jasansky |> filter(country_isoa3 %in% countries[["iso_a3"]])

comm_jas <- do.call(c, jasansky |> pull(commodities_list) |> strsplit(","))
comm_jas |> table() |> sort(decreasing = TRUE) |> head(20)
# Gold, coal, copper, silver, iron, zinc, lead
jasansky[, "commodity"] <- paste0(jasansky[["primary_commodity"]], ",",
  jasansky[["commodities_list"]])

# Processing facilities -- we skip those
# shapes[[grep("padilla_facilities.gpkg", files)]]


# S&P would have info on:
#   Gold, copper, diamonds, coal, uranium, iron, platinum, palladium, ...
# We have specific sources on:
#   Coal, copper, potassium, platinum

# Which commodities do we have? ---
comm <- structure(c(
  comm_dep, comm_exp, comm_gab, comm_mau, comm_jas
), names = NULL)
comm[-grep("^ $", comm)] |> table() |> sort() |>
  write.csv("outputs/commodities_present.csv", row.names = FALSE)


# Start off building one shapefile of commodity locations -----

add_shp <- \(x, y, commodity) {
  if(!missing(commodity)) y <- y |> mutate(commodity = commodity)
  rbind(x, y |> select(commodity))
}

# Coal from Padilla and GEM
shp <- shapes[[grep("padilla_coal", files)]] |> transmute(commodity = "coal")
shp <- shp |> add_shp(shapes[[grep("GEM", files)]] |>
  filter(country_isoa3 %in% countries[["iso_a3"]]), "coal")
# Copper
shp <- shp |> add_shp(shapes[[grep("padilla_copper", files)]], "copper")
# Platinum group
shp <- shp |> add_shp(shapes[[grep("padilla_platinum", files)]], "platinum")
# Potassium
shp <- shp |> add_shp(shapes[[grep("padilla_potassium", files)]], "potassium")

# Add the earlier sources with multiple commodities
shp <- shp |> add_shp(deposits)
shp <- shp |> add_shp(exploration)
shp <- shp |> add_shp(gabon)
shp <- shp |> add_shp(mauritania)
shp <- shp |> add_shp(jasansky)

shp <- shp |> st_make_valid()
st_write(shp, "outputs/commodity_locations.gpkg")

# Use terra to take samples from the polygons
polys <- terra::vect("outputs/commodity_locations.gpkg") # Only reads polys

# Take between 1 and 10 points per polygon (related to size) as sample
samples <- round(expanse(polys, unit = "km") / 100) |> pmax(1) |> pmin(10)
points <- polys |> # Simplify for computation
  simplifyGeom(tolerance = .01, preserveTopology = TRUE, makeValid = TRUE) |>
  spatSample(size = samples, method = "regular")
# Write the points
terra::writeVector(points, "outputs/commodity_polys-to-points.gpkg", overwrite = TRUE)
