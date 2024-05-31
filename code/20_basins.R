
library("sf")
library("dplyr")
library("tmap")

# Mining polygons
mines <- st_read("data/mines/global_mining_polygons_v2.gpkg")
# Subset to Tanzania and use centroids for faster processing
m <- mines |> filter(ISO3_CODE == "TZA") |> st_centroid()

# https://data.hydrosheds.org/file/hydrobasins/standard/hybas_af_lev01-12_v1c.zip
lvl <- "08"
s <- st_read(paste0("data/hybas_af_lev01-12_v1c/hybas_af_lev", lvl, "_v1c.shp"))
s <- st_make_valid(s)
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
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] == id]
  }
  if(all(id_next == 0) || length(id_next) == 0) return()
  return(c(id_next, Recall(id_next[1L], n = n + 1L)))
}
downstream_ids <- sapply(treated_id, stream, down = TRUE)
upstream_ids <- sapply(treated_id, stream, down = FALSE)
# Todo:
#   There may be multiple upstreams, only the first one ([1L]) is taken
#   We should probably at least track the order of a basin

s <- s |> mutate(
  status = ifelse(HYBAS_ID %in% treated_id, "mine",
    ifelse(HYBAS_ID %in% unlist(downstream_ids), "downstream",
      ifelse(HYBAS_ID %in% unlist(upstream_ids), "upstream",
        NA_character_)))
  )

# Plot areas
t <- st_crop(s, st_bbox(s |> filter(!is.na(status)))) |>
  tm_shape() +
  tm_fill("status") +
  tm_borders() +
  tm_shape(m) +
  tm_dots(col = "black", border.col = "white", size = .5, shape = 23)
tmap_save(t, "basins_mines.png")
