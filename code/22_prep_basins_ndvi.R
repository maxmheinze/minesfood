

# Header ------------------------------------------------------------------

pacman::p_load(
  sf,
  tidyverse,
  magrittr,
)


# Mining Polygons ---------------------------------------------------------
evi_basins_1 <- read_csv("/data/jde/basins_evi/mean_evi_basins.csv")
evi_basins_2 <- read_csv("/data/jde/basins_evi/01_evi_basin.csv") 

# downloading and cleaning the new EVI dataset
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

# preparing the old EVI dataset 
max_evi_basins_1 <- evi_basins_1 %>%
  dplyr::select(HYBAS_ID, image_date, mean_EVI) %>%
  mutate(year = year(image_date), 
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |> 
  rename(max_EVI = mean_EVI, 
         max_EVI_date = image_date)

# checking whether the two datasets have the same EVI values 
view(left_join(max_evi_basins, max_evi_basins_1, by = c("HYBAS_ID", "year")))

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
cropland_evi_basins_1 <- read_csv("/data/jde/basins_evi/mean_cropland_evi_basins.csv")

cropland_evi_basins_2 <- read_csv("/data/jde/basins_evi/02_cropland_evi_basin.csv")

max_cropland_evi_basins_1 <- cropland_evi_basins %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>% 
  slice_head(n = 1) |> 
  rename(max_cropland_EVI = mean_EVI, 
         max_cropland_EVI_date = image_date) %>%
  dplyr::select(-basin_id)

max_cropland_evi_basins_2 <- cropland_evi_basins_2 %>%
  mutate(year = year(date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  mutate(meanEVI = ifelse(meanEVI == -9999, NA, meanEVI)) %>%
  slice_max(meanEVI, n = 1, na_rm = TRUE) %>% 
  slice_head(n = 1) |> 
  rename(max_cropland_EVI = meanEVI, 
         max_cropland_EVI_date = date) %>%
  dplyr::select(-`system:index`)

# checking whether the two datasets have the same EVI values 
view(left_join(max_evi_basins, max_cropland_evi_basins_2, by = c("HYBAS_ID", "year")))

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


