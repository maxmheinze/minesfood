# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# EVI data for all areas of basins ---------------------------------------------
evi_basins_1 <- read_csv(p("basins_evi/01_mean_evi_basin.csv"))

# max EVI
max_evi_basins <- evi_basins_1 %>%
  dplyr::select(HYBAS_ID, image_date, mean_EVI) %>%
  mutate(year = year(image_date),
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(max_EVI = mean_EVI,
         max_EVI_date = image_date)

# min EVI
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

# mean EVI
mean_evi_basins <- evi_basins_1 %>%
  dplyr::select(HYBAS_ID, image_date, mean_EVI) %>%
  mutate(year = year(image_date),
         HYBAS_ID = as.numeric(HYBAS_ID)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI = mean(mean_EVI, na.rm = T))

max_evi_basins |> group_by(HYBAS_ID) |>
  count() |> filter(n < 24)
# 8 basins miss a few years of EVI observations


# EVI data for croplpand areas of basins ---------------------------------------

# Africover cropland mask
cropland_evi_basins_1 <- read_csv(p("basins_evi/02_mean_africover_cropland_evi_basin.csv"))

# ESA cropland mask
cropland_evi_basins_3 <- read_csv(p("basins_evi/03_ESA_cropland_evi.csv"))

# max cropland EVI
max_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(max_c_EVI_af = mean_EVI,
         max_c_EVI_date_af = image_date) %>%
  dplyr::select(-basin_id)
max_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(max_c_EVI_ESA = mean_EVI,
         max_c_EVI_date_ESA = image_date) %>%
  dplyr::select(-basin_id)

max_cropland_evi_basins <- full_join(max_cropland_evi_basins_1, 
                                     max_cropland_evi_basins_3, 
                                     by = c("HYBAS_ID", "year"))

# min cropland EVI
min_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(min_c_EVI_af = mean_EVI,
         min_c_EVI_date_af = image_date) %>%
  dplyr::select(-basin_id)
min_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_min(mean_EVI, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1) |>
  rename(min_c_EVI_ESA = mean_EVI,
         min_c_EVI_date_ESA = image_date) %>%
  dplyr::select(-basin_id)

min_cropland_evi_basins <- full_join(min_cropland_evi_basins_1, 
                                     min_cropland_evi_basins_3, 
                                     by = c("HYBAS_ID", "year"))


# mean cropland EVI
mean_cropland_evi_basins_1 <- cropland_evi_basins_1 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_c_EVI_af = mean(mean_EVI, na.rm = T))
mean_cropland_evi_basins_3 <- cropland_evi_basins_3 %>%
  mutate(year = year(image_date)) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_c_EVI_ESA = mean(mean_EVI, na.rm = T))

mean_cropland_evi_basins <- full_join(mean_cropland_evi_basins_1, 
                                      mean_cropland_evi_basins_3, 
                                      by = c("HYBAS_ID", "year"))


# joining the data -------------------------------------------------------------

basin_evi <- left_join(max_evi_basins, min_evi_basins) |>
  left_join(mean_evi_basins) |>
  left_join(max_cropland_evi_basins) |>
  left_join(min_cropland_evi_basins) |>
  left_join(mean_cropland_evi_basins) |>
  relocate(year, .after = HYBAS_ID)


# saving the data -------------------------------------------------------------

write_csv(basin_evi, p("basins_evi/basin_evi.csv"))

