

# Header ------------------------------------------------------------------

pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# Mining Polygons ---------------------------------------------------------
evi_basins_1 <- read_csv(p("basins_evi/01_mean_evi_basin.csv"))

# preparing the EVI dataset
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

min_evi_basins <- evi_basins_1 %>%
  dplyr::select(HYBAS_ID, image_date, mean_EVI) %>%
  mutate(year = year(image_date),
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(min_EVI = mean_EVI,
         min_EVI_date = image_date)

mean_evi_basins <- evi_basins_1 %>%
  dplyr::select(HYBAS_ID, image_date, mean_EVI) %>%
  mutate(year = year(image_date),
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI = mean(mean_EVI, na.rm = T))

max_evi_basins_1 |> group_by(HYBAS_ID) |>
  count() |> filter(n < 24)
# 8 basins miss a few years of EVI observations


# downloading and cleaning the cropland EVI
cropland_evi_basins_1 <- read_csv(p("basins_evi/02_mean_africover_cropland_evi_basin.csv"))

cropland_evi_basins_3 <- read_csv(p("basins_evi/03_ESA_cropland_evi.csv"))

max_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(max_cropland_EVI_africover = mean_EVI,
         max_cropland_EVI_date_africover = image_date) %>%
  dplyr::select(-basin_id)

max_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(max_cropland_EVI_ESA = mean_EVI,
         max_cropland_EVI_date_ESA = image_date) %>%
  dplyr::select(-basin_id)


# checking whether the two datasets have the same EVI values
max_cropland_evi_basins <- full_join(max_cropland_evi_basins_1, max_cropland_evi_basins_3, by = c("HYBAS_ID", "year"))


min_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(min_cropland_EVI_africover = mean_EVI,
         min_cropland_EVI_date_africover = image_date) %>%
  dplyr::select(-basin_id)

mean_cropland_evi_basins <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_cropland_EVI_africover = mean(mean_EVI, na.rm = T))


min_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(min_cropland_EVI_ESA = mean_EVI,
         min_cropland_EVI_date_ESA = image_date) %>%
  dplyr::select(-basin_id)

min_cropland_evi_basins <- full_join(min_cropland_evi_basins_3, min_cropland_evi_basins_1)

mean_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_cropland_EVI_africover_ESA = mean(mean_EVI, na.rm = T))

mean_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_cropland_EVI_africover_ESA = mean(mean_EVI, na.rm = T))

mean_cropland_evi_basins <- full_join(mean_cropland_evi_basins_1, mean_cropland_evi_basins_3)

## UPDATE
# joining the data --------------------------------------------------------

basin_evi <- left_join(max_evi_basins_1, min_evi_basins) |>
  left_join(mean_evi_basins) |>
  left_join(max_cropland_evi_basins) |>
  left_join(min_cropland_evi_basins) |>
  left_join(mean_cropland_evi_basins) |>
  relocate(year, .after = HYBAS_ID)

write_csv(basin_evi, p("basins_evi/basin_evi.csv"))


