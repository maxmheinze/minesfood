
# Header ------------------------------------------------------------------

pacman:: p_load(
  tidyverse,
  terra,
  magrittr,
  parallel
)

sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

relevant_basins <- st_read(p("processed/relevant_basins.gpkg"))



# Read Pop Files ----------------------------------------------------------

rstlist <- paste0(p("worldpop/"), list.files(path = p("worldpop/"))) 

worldpop <- mclapply(
  rstlist,
  function(x) {
    current_rstr <- terra::rast(x)
    
    extracted <- terra::extract(terra::subset(current_rstr, 1), relevant_basins, fun = sum, exact = T, na.rm = T)
    
    return(extracted)
  },
  mc.cores = 12
)

worldpop1 <- worldpop

for (i in seq_along(worldpop)) {
  worldpop[[i]] <- dplyr::mutate(worldpop[[i]], year = 2010 + i,
                                 HYBAS_ID = relevant_basins$HYBAS_ID)
  colnames(worldpop[[i]]) <- c("id", "pop", "year", "HYBAS_ID")
}

worldpop %<>%
  bind_rows() %>%
  select(HYBAS_ID, year, pop)


# Write CSV ---------------------------------------------------------------

write_csv(worldpop, p("processed/basin_pop.csv"))


