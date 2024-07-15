

# Header ------------------------------------------------------------------

pacman::p_load(
  sf,
  tidyverse,
  magrittr,
)


# Mining Polygons ---------------------------------------------------------
#evi_basins_1 <- read_csv("/data/jde/basins_evi/mean_evi_basins.csv")
evi_basins_2 <- read_csv("/data/jde/basins_evi/01_evi_basin.csv") 

# downloading and cleaning the total EVI 
max_evi_basins <- evi_basins_2 %>%
  dplyr::select(HYBAS_ID, date, meanEVI) %>%
  mutate(year = year(date), 
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(meanEVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |> 
  rename(max_EVI = meanEVI, 
         max_EVI_date = date)

min_evi_basins <- evi_basins_2 %>%
  dplyr::select(HYBAS_ID, date, meanEVI) %>%
  mutate(year = year(date), 
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(meanEVI, n = 1, na_rm = TRUE) %>% 
  slice_head(n = 1) |> 
  rename(min_EVI = meanEVI, 
         min_EVI_date = date)

mean_evi_basins <- evi_basins_2 %>%
  dplyr::select(HYBAS_ID, date, meanEVI) %>%
  mutate(year = year(date), 
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI = mean(meanEVI, na.rm = T))

max_evi_basins |> group_by(HYBAS_ID) |> 
  count() |> filter(n < 24)
# 8 basins miss a few years of EVI observations


# downloading and cleaning the cropland EVI 
cropland_evi_basins <- read_csv("/data/jde/basins_evi/mean_cropland_evi_basins.csv")

max_cropland_evi_basins <- cropland_evi_basins %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>% 
  slice_head(n = 1) |> 
  rename(max_cropland_EVI = mean_EVI, 
         max_cropland_EVI_date = image_date) %>%
  select(-basin_id)

min_cropland_evi_basins <- cropland_evi_basins %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>% 
  slice_head(n = 1) |> 
  rename(min_cropland_EVI = mean_EVI, 
         min_cropland_EVI_date = image_date) %>%
  select(-basin_id)

mean_cropland_evi_basins <- cropland_evi_basins %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_cropland_EVI = mean(mean_EVI, na.rm = T))


# joining the data --------------------------------------------------------

basin_evi <- left_join(max_evi_basins, min_evi_basins) |> 
  left_join(mean_evi_basins) |> 
  left_join(max_cropland_evi_basins) |> 
  left_join(min_cropland_evi_basins) |> 
  left_join(mean_cropland_evi_basins) |> 
  relocate(year, .after = HYBAS_ID)

write_csv(basin_evi, "/data/jde/basins_evi/basin_evi.csv")
