

# Header ------------------------------------------------------------------

pacman::p_load(
  sf,
  tidyverse,
  magrittr,
)


# Mining Polygons ---------------------------------------------------------

mean_evi_basins <- st_read("data_local/mean_evi_basins.csv")

# downloading and cleaning the total EVI 

mean_evi_basins_clean <- mean_evi_basins %>%
  mutate(year = year(image_date), 
         PFAF_ID = as.numeric(PFAF_ID)) %>%
  arrange(PFAF_ID, year) %>%
  group_by(PFAF_ID, year) %>%
  filter(mean_EVI == max(mean_EVI)) %>%
  rename(max_EVI = mean_EVI, 
         date_all_land = image_date) %>%
  select(!c(basin_id))

# downloading and cleaning the cropland EVI 

mean_cropland_evi_basins <- read_csv("data_local/mean_cropland_evi_basins.csv")

mean_cropland_evi_basins_clean <- mean_cropland_evi_basins %>%
  mutate(year = year(image_date)) %>%
  arrange(PFAF_ID, year) %>%
  group_by(PFAF_ID, year) %>%
  filter(mean_EVI == max(mean_EVI)) %>%
  rename(max_cropland_EVI = mean_EVI, 
         date_cropland = image_date) %>%
  select(!c(basin_id))


# joining the data --------------------------------------------------------

basin_evi <- left_join(mean_evi_basins_clean, mean_cropland_evi_basins_clean)

write_csv(basin_evi, "data_local/basin_evi.csv")
