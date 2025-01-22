# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# No mask -----------------------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_nomask <- read_csv(p("basins_evi/update_revision/00_ndvi-mean_nomask.csv"))

# maximum of 16-day interval
max_mean_ndvi_nomask <- ndvi_mean_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date, # exclude date of maximum NDVI for now
            max_NDVI_16_nomask = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_nomask, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_nomask <- ndvi_mean_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_nomask = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_nomask = mean(mean_NDVI_16_nomask, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_nomask <- read_csv(p("basins_evi/update_revision/10_ndvi-max_nomask.csv"))

mean_max_ndvi_nomask <- ndvi_max_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_nomask = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_nomask <- max_mean_ndvi_nomask %>%
  full_join(mean_mean_ndvi_nomask, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_nomask, by = c("HYBAS_ID", "year"))

# for some basins the average of the maximum NDVI per pixel is lower than the
# maximum of the 16-day basin averages, makes that sense? maybe because of infrastructure?
ndvi_nomask |> filter(max_NDVI_px_nomask < max_NDVI_16_nomask)


# Vegetation masks ESA-CCI ------------------------------------------------

### Broad vegetation (includes flooded forest, and sparse vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_veg_broad <- read_csv(p("basins_evi/update_revision/01_ndvi-mean_veg_broad.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_veg_broad <- ndvi_mean_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_veg_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_veg_broad, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_veg_broad <- ndvi_mean_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_veg_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_veg_broad = mean(mean_NDVI_16_cci_veg_broad, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_veg_broad <- read_csv(p("basins_evi/update_revision/11_ndvi-max_veg_broad.csv"))

mean_max_ndvi_cci_veg_broad <- ndvi_max_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_veg_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_veg_broad <- max_mean_ndvi_cci_veg_broad %>%
  full_join(mean_mean_ndvi_cci_veg_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_veg_broad, by = c("HYBAS_ID", "year"))


### Narrow vegetation (excludes flooded forest, and sparse vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_veg_narrow <- read_csv(p("basins_evi/update_revision/02_ndvi-mean_veg_narrow.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_veg_narrow <- ndvi_mean_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_veg_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_veg_narrow, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_veg_narrow <- ndvi_mean_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_veg_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_veg_narrow = mean(mean_NDVI_16_cci_veg_narrow, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_veg_narrow <- read_csv(p("basins_evi/update_revision/12_ndvi-max_veg_narrow.csv"))

mean_max_ndvi_cci_veg_narrow <- ndvi_max_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_veg_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_veg_narrow <- max_mean_ndvi_cci_veg_narrow %>%
  full_join(mean_mean_ndvi_cci_veg_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_veg_narrow, by = c("HYBAS_ID", "year"))


### Flooded/sparse vegetation (used as validity check for NDVI calculation only)

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_veg_fs <- read_csv(p("basins_evi/update_revision/07_ndvi-mean_flooded_sparse.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_veg_fs <- ndvi_mean_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_veg_fs = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_veg_fs, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_veg_fs <- ndvi_mean_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_veg_fs = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_veg_fs = mean(mean_NDVI_16_cci_veg_fs, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_veg_fs <- read_csv(p("basins_evi/update_revision/17_ndvi-max_flooded_sparse.csv"))

mean_max_ndvi_cci_veg_fs <- ndvi_max_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_veg_fs = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_veg_fs <- max_mean_ndvi_cci_veg_fs %>%
  full_join(mean_mean_ndvi_cci_veg_fs, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_veg_fs, by = c("HYBAS_ID", "year"))


# Cropland masks ESA-CCI ---------------------------------------------------

### Broad croplands (includes also mosaics of croplands and natural vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_c_broad <- read_csv(p("basins_evi/update_revision/03_ndvi-mean_c_broad.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_c_broad <- ndvi_mean_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_c_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_c_broad, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_c_broad <- ndvi_mean_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_c_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_c_broad = mean(mean_NDVI_16_cci_c_broad, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_c_broad <- read_csv(p("basins_evi/update_revision/13_ndvi-max_c_broad.csv"))

mean_max_ndvi_cci_c_broad <- ndvi_max_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_c_broad = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_c_broad <- max_mean_ndvi_cci_c_broad %>%
  full_join(mean_mean_ndvi_cci_c_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_c_broad, by = c("HYBAS_ID", "year"))


### Narrow croplands (excludes also mosaics of croplands and natural vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_c_narrow <- read_csv(p("basins_evi/update_revision/04_ndvi-mean_c_narrow.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_c_narrow <- ndvi_mean_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_c_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_c_narrow, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_c_narrow <- ndvi_mean_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_c_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_c_narrow = mean(mean_NDVI_16_cci_c_narrow, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_c_narrow <- read_csv(p("basins_evi/update_revision/14_ndvi-max_c_narrow.csv"))

mean_max_ndvi_cci_c_narrow <- ndvi_max_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_c_narrow = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_c_narrow <- max_mean_ndvi_cci_c_narrow %>%
  full_join(mean_mean_ndvi_cci_c_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_c_narrow, by = c("HYBAS_ID", "year"))


### Irrigated croplands

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_c_irrigated <- read_csv(p("basins_evi/update_revision/05_ndvi-mean_c_irrigated.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_c_irrigated <- ndvi_mean_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_c_irrigated = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_c_irrigated, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_c_irrigated <- ndvi_mean_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_c_irrigated = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_c_irrigated = mean(mean_NDVI_16_cci_c_irrigated, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_c_irrigated <- read_csv(p("basins_evi/update_revision/15_ndvi-max_c_irrigated.csv"))

mean_max_ndvi_cci_c_irrigated <- ndvi_max_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_c_irrigated = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_c_irrigated <- max_mean_ndvi_cci_c_irrigated %>%
  full_join(mean_mean_ndvi_cci_c_irrigated, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_c_irrigated, by = c("HYBAS_ID", "year"))


### Rainfed croplands

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_cci_c_rainfed <- read_csv(p("basins_evi/update_revision/06_ndvi-mean_c_rainfed.csv"))

# maximum of 16-day interval
max_mean_ndvi_cci_c_rainfed <- ndvi_mean_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_cci_c_rainfed = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_cci_c_rainfed, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_cci_c_rainfed <- ndvi_mean_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_cci_c_rainfed = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_cci_c_rainfed = mean(mean_NDVI_16_cci_c_rainfed, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_cci_c_rainfed <- read_csv(p("basins_evi/update_revision/16_ndvi-max_c_rainfed.csv"))

mean_max_ndvi_cci_c_rainfed <- ndvi_max_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_cci_c_rainfed = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_cci_c_rainfed <- max_mean_ndvi_cci_c_rainfed %>%
  full_join(mean_mean_ndvi_cci_c_rainfed, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_cci_c_rainfed, by = c("HYBAS_ID", "year"))


# Cropland mask Africover -------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_af_c <- read_csv(p("basins_evi/update_revision/09_ndvi-mean_africover.csv"))

# maximum of 16-day interval
max_mean_ndvi_af_c <- ndvi_mean_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_af_c = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_af_c, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_af_c <- ndvi_mean_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_af_c = mean_NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_af_c = mean(mean_NDVI_16_af_c, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_af_c <- read_csv(p("basins_evi/update_revision/19_ndvi-max_africover.csv"))

mean_max_ndvi_af_c <- ndvi_max_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_af_c = mean_NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_af_c <- max_mean_ndvi_af_c %>%
  full_join(mean_mean_ndvi_af_c, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_af_c, by = c("HYBAS_ID", "year"))


# Vegetation mask ESRI ----------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_esri_veg <- read_csv(p("basins_evi/update_revision/s24_ndvi_mean_vegetation.csv"))

# maximum of 16-day interval
max_mean_ndvi_esri_veg <- ndvi_mean_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_esri_veg = NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_esri_veg, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_esri_veg <- ndvi_mean_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_esri_veg = NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_esri_veg = mean(mean_NDVI_16_esri_veg, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_esri_veg <- read_csv(p("basins_evi/update_revision/s23_ndvi_max_vegetation.csv"))

mean_max_ndvi_esri_veg <- ndvi_max_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_esri_veg = NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_esri_veg <- max_mean_ndvi_esri_veg %>%
  full_join(mean_mean_ndvi_esri_veg, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_esri_veg, by = c("HYBAS_ID", "year"))



# Cropland mask ESRI ------------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
ndvi_mean_esri_c <- read_csv(p("basins_evi/update_revision/s22_ndvi_mean_croplands.csv"))

# maximum of 16-day interval
max_mean_ndvi_esri_c <- ndvi_mean_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_NDVI_16_esri_c = NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_NDVI_16_esri_c, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_ndvi_esri_c <- ndvi_mean_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_NDVI_16_esri_c = NDVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_NDVI_16_esri_c = mean(mean_NDVI_16_esri_c, na.rm = T))

# data with maximum NDVI per pixel per year, averaged across pixels per year
ndvi_max_esri_c <- read_csv(p("basins_evi/update_revision/s21_ndvi_max_croplands.csv"))

mean_max_ndvi_esri_c <- ndvi_max_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_NDVI_px_esri_c = NDVI) %>%
  arrange(HYBAS_ID, year)

# join
ndvi_esri_c <- max_mean_ndvi_esri_c %>%
  full_join(mean_mean_ndvi_esri_c, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_ndvi_esri_c, by = c("HYBAS_ID", "year"))


# Joining data from all masks ---------------------------------------------

basin_ndvi <- full_join(ndvi_nomask, ndvi_cci_veg_broad,
                       by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_veg_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_veg_fs, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_c_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_c_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_c_irrigated, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_cci_c_rainfed, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_af_c, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_esri_c, by = c("HYBAS_ID", "year")) %>%
  full_join(ndvi_esri_veg, by = c("HYBAS_ID", "year"))


# saving the data -------------------------------------------------------------

write_csv(basin_ndvi, p("basins_evi/basin_ndvi_update_revision.csv"))
saveRDS(basin_ndvi, p("basins_evi/basin_ndvi_update_revision.RDS"))

