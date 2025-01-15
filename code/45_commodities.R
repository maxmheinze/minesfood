
library("sf")
library("dplyr")

# Sources ---
files <- c(
  # Padilla et al. <https://www.sciencebase.gov/catalog/item/607611a9d34e018b3201cbbf>
  list.files("data/commodities/", pattern = "padilla"),
  # Jasansky et al. <https://www.nature.com/articles/s41597-023-01965-y>
  list.files("data/commodities/", pattern = "jasansky"),
  # <https://globalenergymonitor.org/>
  list.files("data/commodities/", pattern = "GEM")
)

shapes <- lapply(paste0("data/commodities/", files), st_read)

# Countries for a plot ---
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

# S&P would have info on:
# Gold, copper, diamonds, coal, uranium, iron, platinum, palladium, ...

# Processing facilities -- we skip those
# shapes[[grep("padilla_facilities.gpkg", files)]]

# We have specific sources on:
# coal, copper, potassium, platinum

# Which commodities do we have? ---
comm <- structure(c(
  comm_dep, comm_exp, comm_gab, comm_mau, comm_jas
), names = NULL)
comm[-grep("^ $", comm)] |> table() |> sort() |>
  write.csv("outputs/commodities.csv", row.names = FALSE)


# Start off building a shapefile -----

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

# Add sources with multiple commodities
shp <- shp |> add_shp(deposits)
shp <- shp |> add_shp(exploration)
shp <- shp |> add_shp(gabon)
shp <- shp |> add_shp(mauritania)
shp <- shp |> add_shp(jasansky)

shp <- shp |> st_make_valid()
st_write(shp, "outputs/commodity_locations.gpkg")

# Use terra to take samples from the polygons
library("terra")
polys <- terra::vect("outputs/commodity_locations.gpkg")
samples <- round(expanse(polys, unit = "km") / 100) |> pmax(1) |> pmin(5)
points <- polys |> # There's at least one extremely detailed polygon that crashes the regular sampling
  simplifyGeom(tolerance = .01, preserveTopology = TRUE, makeValid = TRUE) |>
  spatSample(size = samples, method = "regular")
# Write the points
terra::writeVector(points, "outputs/commodity_polys-to-points.gpkg", overwrite = TRUE)
