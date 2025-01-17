# Load Packages -----------------------------------------------------------

rm(list = ls())

pacman::p_load(
  sf,
  tidyverse,
  dplyr,
  magrittr,
  parallel,
  stars,
  terra,
  ncdf4
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

basins <- st_make_valid(st_read(p("processed/relevant_basins_ordered-ipis.gpkg")))

africa <- st_bbox(rnaturalearth::ne_countries(continent = "africa"))

years <- 2000:2022
files <- list.files(p("land_use/", pre = "/data/redd/"), full.names = T)
files <- files[which(grepl(paste0(years, collapse = "|"), files))]

lc_list <- mclapply(
  files, 
  function(x) {
    lc_data <- rast(x, subds = "lccs_class")
    lc_data <- crop(lc_data, africa)
    lc_temp <- terra::extract(lc_data, basins, exact = T, weights = T) |> 
      left_join(tibble(ID = seq_len(nrow(basins)), HYBAS_ID = basins$HYBAS_ID), by = "ID") |>
      relocate(HYBAS_ID, .before = ID) |> dplyr::select(-ID) |> 
      group_by(HYBAS_ID, lccs_class) |> 
      summarise(lc_share = sum(weight, na.rm = T)) |> 
      group_by(HYBAS_ID) |> 
      mutate(year = substr(time(lc_data), 1, 4), 
             lc_share = lc_share / sum(lc_share), 
             .after = "HYBAS_ID")
    
    return(lc_temp)
  },
  mc.cores = 8
)

lc_df <- lc_list |> 
  bind_rows() |> 
  arrange(HYBAS_ID, year)

saveRDS(lc_df, "/data/jde/processed/CCI_lc_raw.RDS")

lc_df <- readRDS("/data/jde/processed/CCI_lc_raw.RDS")
# LC classes see page 81-82 here: https://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf
lc_summarised <- lc_df |> 
  mutate(lccs_class = case_when(
    lccs_class %in% c(10, 11, 12) ~ "crops_rainfed",
    lccs_class %in% c(20) ~ "crops_irrigated",
    lccs_class %in% c(30, 40) ~ "crops_veg_mosaic",
    lccs_class %in% c(50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100) ~ "trees",
    lccs_class %in% c(110, 120, 121, 122) ~ "shrubland",
    lccs_class %in% c(130, 140) ~ "grasslands",
    lccs_class %in% c(150, 151, 152, 153) ~ "sparse_veg",
    lccs_class %in% c(160, 170, 180) ~ "flooded_veg",
    lccs_class %in% c(190) ~ "urban",
    lccs_class %in% c(200, 201, 202) ~ "bare",
    lccs_class %in% c(210) ~ "water",
    lccs_class %in% c(220) ~ "snow_ice",
    .default = "character"
  )) |> 
  group_by(HYBAS_ID, year, lccs_class) |> 
  summarise(lc_share = sum(lc_share, na.rm = T)) |> 
  tidyr::pivot_wider(names_from = lccs_class, values_from = lc_share, 
                     names_prefix = "lc_", values_fill = 0)

saveRDS(lc_summarised, file = "/data/jde/processed/CCI_lc_summarised.RDS")

