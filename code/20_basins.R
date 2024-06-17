
library("sf")
library("dplyr")
library("tmap")
library("countrycode")

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



