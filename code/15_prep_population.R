
# Header ------------------------------------------------------------------

pacman:: p_load(
  tidyverse,
  terra,
  magrittr,
  parallel
)

sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})

relevant_basins <- st_make_valid(st_read(p("processed/relevant_basins_ordered-ipis.gpkg")))



# Read Pop Files ----------------------------------------------------------

rstlist <- paste0(p("worldpop/"), list.files(path = p("worldpop/"))) 

worldpop <- mclapply(
  rstlist,
  function(x) {
    current_rstr <- terra::rast(x)
    
    extracted <- terra::extract(terra::subset(current_rstr, 1), 
                                relevant_basins, 
                                fun = sum, exact = T, na.rm = T)
    
    return(extracted)
  },
  mc.cores = 12
)

for (i in seq_along(worldpop)) {
  worldpop[[i]] <- dplyr::mutate(worldpop[[i]], year = 2010 + i,
                                 HYBAS_ID = relevant_basins$HYBAS_ID)
  colnames(worldpop[[i]]) <- c("id", "pop", "year", "HYBAS_ID")
}

worldpop %<>%
  bind_rows() %>%
  select(HYBAS_ID, year, pop)


# Extend data and merge ---------------------------------------------------

worldpop_2015 <- worldpop |> filter(year == 2015) |> 
  transmute(HYBAS_ID, pop_2015 = pop)

worldpop_pred <- worldpop |> rename(pop_pred = pop)
for(i in unique(worldpop_pred$HYBAS_ID)) {
  if(all(is.na(worldpop_pred |> filter(HYBAS_ID == i) |> pull(pop_pred)))) {
    worldpop_pred <- worldpop_pred |> rows_append(
      tibble(HYBAS_ID = i, year = 2021:2023, pop_pred = NA)
    )
  } else {
    lm_temp <- lm(pop_pred ~ year, data = worldpop_pred |> filter(HYBAS_ID == i))
    worldpop_pred <- worldpop_pred |> rows_append(
      tibble(HYBAS_ID = i, year = 2021:2023, 
             pop_pred = predict(lm_temp, 
                                newdata = tibble(year = 2021:2023)))
    )
  }
}

worldpop <- full_join(worldpop_pred, worldpop) |> 
  left_join(worldpop_2015, by = "HYBAS_ID") |> 
  group_by(HYBAS_ID) |> 
  mutate(pop_const = pop, 
         pop_const = replace(pop_const, year %in% 2021:2023, 
                             pop_const[year == 2020]))

# Write CSV ---------------------------------------------------------------

write_csv(worldpop, p("processed/basin_pop.csv"))


