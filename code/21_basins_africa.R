

# Header ------------------------------------------------------------------

pacman::p_load(
  sf,
  dplyr,
  tmap,
  magrittr,
  parallel
)

source("./code/91_pfaf_functions.R")


# Mining Polygons ---------------------------------------------------------

mines <- st_read("/data/jde/mines/global_mining_polygons_v2.gpkg")

mines %<>%
  st_centroid()


# Basin Polygons ----------------------------------------------------------

# Use HydroBASINS with lakes 
lvl <- "12"
basins <- st_read(paste0("/data/jde/hybas_lakes/hybas_lake_af_lev", lvl, "_v1c.shp"))

basins %<>%
  st_make_valid()


# Extract IDs of Relevant Basins ------------------------------------------

# Find out what basins have a mine in them
basins_intersect_with_mines <- st_intersects(basins, mines)

# Extract their ID
basins_with_mines <- basins[lengths(basins_intersect_with_mines),]$PFAF_ID %>%
  as.character()

# IDs of all basins
all_basins <- basins$PFAF_ID %>%
  as.character()

# List of basins downstream of a mine -- Runtime 13 minutes
mines_downstream_list <- mclapply(basins_with_mines, pfaf_downstream_all, unique(all_basins), HydroBASINS = TRUE, reflexive = FALSE, mc.cores = 15)
names(mines_downstream_list) <- basins_with_mines

# List of basins upstream of a mine -- Runtime 13.5 minutes
mines_upstream_list <- mclapply(basins_with_mines, pfaf_upstream_all, unique(all_basins), HydroBASINS = TRUE, reflexive = FALSE, mc.cores = 15)
names(mines_upstream_list) <- basins_with_mines


# To Do: Order and distance (in basins)
# To Do 2: shp
 

# Save intermediate
save(mines_downstream_list, mines_upstream_list, file = "./data/intermediate_outputs/updownlists.RData")

# Load intermediate
load("./data/intermediate_outputs/updownlists.RData")