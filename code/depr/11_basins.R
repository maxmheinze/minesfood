
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
