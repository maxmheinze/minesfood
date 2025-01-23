# Header ------------------------------------------------------------------
pacman::p_load(
  sf,
  tidyverse,
  magrittr
)
sapply(list.files("R", ".R$"), \(f) {source(paste0("R/", f)); TRUE})


# No mask -----------------------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_nomask <- read_csv(p("basins_evi/update_revision/00_evi-mean_nomask.csv"))

# maximum of 16-day interval
max_mean_evi_nomask <- evi_mean_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date, # exclude date of maximum EVI for now
            max_EVI_16_nomask = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_nomask, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_nomask <- evi_mean_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_nomask = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_nomask = mean(mean_EVI_16_nomask, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_nomask <- read_csv(p("basins_evi/update_revision/10_evi-max_nomask.csv"))

mean_max_evi_nomask <- evi_max_nomask %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_nomask = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_nomask <- max_mean_evi_nomask %>%
  full_join(mean_mean_evi_nomask, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_nomask, by = c("HYBAS_ID", "year"))

# for some basins the average of the maximum EVI per pixel is lower than the
# maximum of the 16-day basin averages, makes that sense? maybe because of infrastructure?
evi_nomask |> filter(max_EVI_px_nomask < max_EVI_16_nomask)


# Vegetation masks ESA-CCI ------------------------------------------------

### Broad vegetation (includes flooded forest, and sparse vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_veg_broad <- read_csv(p("basins_evi/update_revision/01_evi-mean_veg_broad.csv"))

# maximum of 16-day interval
max_mean_evi_cci_veg_broad <- evi_mean_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_veg_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_veg_broad, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_veg_broad <- evi_mean_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_veg_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_veg_broad = mean(mean_EVI_16_cci_veg_broad, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_veg_broad <- read_csv(p("basins_evi/update_revision/11_evi-max_veg_broad.csv"))

mean_max_evi_cci_veg_broad <- evi_max_cci_veg_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_veg_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_veg_broad <- max_mean_evi_cci_veg_broad %>%
  full_join(mean_mean_evi_cci_veg_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_veg_broad, by = c("HYBAS_ID", "year"))


### Narrow vegetation (excludes flooded forest, and sparse vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_veg_narrow <- read_csv(p("basins_evi/update_revision/02_evi-mean_veg_narrow.csv"))

# maximum of 16-day interval
max_mean_evi_cci_veg_narrow <- evi_mean_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_veg_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_veg_narrow, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_veg_narrow <- evi_mean_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_veg_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_veg_narrow = mean(mean_EVI_16_cci_veg_narrow, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_veg_narrow <- read_csv(p("basins_evi/update_revision/12_evi-max_veg_narrow.csv"))

mean_max_evi_cci_veg_narrow <- evi_max_cci_veg_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_veg_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_veg_narrow <- max_mean_evi_cci_veg_narrow %>%
  full_join(mean_mean_evi_cci_veg_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_veg_narrow, by = c("HYBAS_ID", "year"))


### Flooded/sparse vegetation (used as validity check for EVI calculation only)

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_veg_fs <- read_csv(p("basins_evi/update_revision/07_evi-mean_flooded_sparse.csv"))

# maximum of 16-day interval
max_mean_evi_cci_veg_fs <- evi_mean_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_veg_fs = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_veg_fs, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_veg_fs <- evi_mean_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_veg_fs = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_veg_fs = mean(mean_EVI_16_cci_veg_fs, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_veg_fs <- read_csv(p("basins_evi/update_revision/17_evi-max_flooded_sparse.csv"))

mean_max_evi_cci_veg_fs <- evi_max_cci_veg_fs %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_veg_fs = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_veg_fs <- max_mean_evi_cci_veg_fs %>%
  full_join(mean_mean_evi_cci_veg_fs, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_veg_fs, by = c("HYBAS_ID", "year"))


# Cropland masks ESA-CCI ---------------------------------------------------

### Broad croplands (includes also mosaics of croplands and natural vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_c_broad <- read_csv(p("basins_evi/update_revision/03_evi-mean_c_broad.csv"))

# maximum of 16-day interval
max_mean_evi_cci_c_broad <- evi_mean_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_c_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_c_broad, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_c_broad <- evi_mean_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_c_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_c_broad = mean(mean_EVI_16_cci_c_broad, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_c_broad <- read_csv(p("basins_evi/update_revision/13_evi-max_c_broad.csv"))

mean_max_evi_cci_c_broad <- evi_max_cci_c_broad %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_c_broad = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_c_broad <- max_mean_evi_cci_c_broad %>%
  full_join(mean_mean_evi_cci_c_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_c_broad, by = c("HYBAS_ID", "year"))


### Narrow croplands (excludes also mosaics of croplands and natural vegetation)

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_c_narrow <- read_csv(p("basins_evi/update_revision/04_evi-mean_c_narrow.csv"))

# maximum of 16-day interval
max_mean_evi_cci_c_narrow <- evi_mean_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_c_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_c_narrow, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_c_narrow <- evi_mean_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_c_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_c_narrow = mean(mean_EVI_16_cci_c_narrow, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_c_narrow <- read_csv(p("basins_evi/update_revision/14_evi-max_c_narrow.csv"))

mean_max_evi_cci_c_narrow <- evi_max_cci_c_narrow %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_c_narrow = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_c_narrow <- max_mean_evi_cci_c_narrow %>%
  full_join(mean_mean_evi_cci_c_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_c_narrow, by = c("HYBAS_ID", "year"))


### Irrigated croplands

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_c_irrigated <- read_csv(p("basins_evi/update_revision/05_evi-mean_c_irrigated.csv"))

# maximum of 16-day interval
max_mean_evi_cci_c_irrigated <- evi_mean_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_c_irrigated = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_c_irrigated, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_c_irrigated <- evi_mean_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_c_irrigated = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_c_irrigated = mean(mean_EVI_16_cci_c_irrigated, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_c_irrigated <- read_csv(p("basins_evi/update_revision/15_evi-max_c_irrigated.csv"))

mean_max_evi_cci_c_irrigated <- evi_max_cci_c_irrigated %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_c_irrigated = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_c_irrigated <- max_mean_evi_cci_c_irrigated %>%
  full_join(mean_mean_evi_cci_c_irrigated, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_c_irrigated, by = c("HYBAS_ID", "year"))


### Rainfed croplands

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_cci_c_rainfed <- read_csv(p("basins_evi/update_revision/06_evi-mean_c_rainfed.csv"))

# maximum of 16-day interval
max_mean_evi_cci_c_rainfed <- evi_mean_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_cci_c_rainfed = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_cci_c_rainfed, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_cci_c_rainfed <- evi_mean_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_cci_c_rainfed = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_cci_c_rainfed = mean(mean_EVI_16_cci_c_rainfed, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_cci_c_rainfed <- read_csv(p("basins_evi/update_revision/16_evi-max_c_rainfed.csv"))

mean_max_evi_cci_c_rainfed <- evi_max_cci_c_rainfed %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_cci_c_rainfed = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_cci_c_rainfed <- max_mean_evi_cci_c_rainfed %>%
  full_join(mean_mean_evi_cci_c_rainfed, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_cci_c_rainfed, by = c("HYBAS_ID", "year"))


# Cropland mask Africover -------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_af_c <- read_csv(p("basins_evi/update_revision/09_evi-mean_africover.csv"))

# maximum of 16-day interval
max_mean_evi_af_c <- evi_mean_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_af_c = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_af_c, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_af_c <- evi_mean_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_af_c = mean_EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_af_c = mean(mean_EVI_16_af_c, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_af_c <- read_csv(p("basins_evi/update_revision/19_evi-max_africover.csv"))

mean_max_evi_af_c <- evi_max_af_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_af_c = mean_EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_af_c <- max_mean_evi_af_c %>%
  full_join(mean_mean_evi_af_c, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_af_c, by = c("HYBAS_ID", "year"))


# Vegetation mask ESRI ----------------------------------------------------

# # data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_esri_veg <- read_csv(p("basins_evi/update_revision/s14_evi_mean_vegetation.csv"))

# maximum of 16-day interval
max_mean_evi_esri_veg <- evi_mean_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_esri_veg = EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_esri_veg, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_esri_veg <- evi_mean_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_esri_veg = EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_esri_veg = mean(mean_EVI_16_esri_veg, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_esri_veg <- read_csv(p("basins_evi/update_revision/s13_evi_max_vegetation.csv"))

mean_max_evi_esri_veg <- evi_max_esri_veg %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_esri_veg = EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_esri_veg <- max_mean_evi_esri_veg %>%
  full_join(mean_mean_evi_esri_veg, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_esri_veg, by = c("HYBAS_ID", "year"))



# Cropland mask ESRI ------------------------------------------------------

# data with 16-day interval data, with mean across pixels within basin for each interval
evi_mean_esri_c <- read_csv(p("basins_evi/update_revision/s12_evi_mean_croplands.csv"))

# maximum of 16-day interval
max_mean_evi_esri_c <- evi_mean_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            # image_date,
            max_EVI_16_esri_c = EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  slice_max(max_EVI_16_esri_c, n = 1, na_rm = TRUE) %>%
  slice_head(n = 1)

# mean across 16-day intervals
mean_mean_evi_esri_c <- evi_mean_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            mean_EVI_16_esri_c = EVI) %>%
  arrange(HYBAS_ID, year) %>%
  group_by(HYBAS_ID, year) %>%
  summarise(mean_EVI_16_esri_c = mean(mean_EVI_16_esri_c, na.rm = T))

# data with maximum EVI per pixel per year, averaged across pixels per year
evi_max_esri_c <- read_csv(p("basins_evi/update_revision/s11_evi_max_croplands.csv"))

mean_max_evi_esri_c <- evi_max_esri_c %>%
  transmute(HYBAS_ID = as.numeric(HYBAS_ID),
            year = year(image_date),
            max_EVI_px_esri_c = EVI) %>%
  arrange(HYBAS_ID, year)

# join
evi_esri_c <- max_mean_evi_esri_c %>%
  full_join(mean_mean_evi_esri_c, by = c("HYBAS_ID", "year")) %>%
  full_join(mean_max_evi_esri_c, by = c("HYBAS_ID", "year"))


# Joining data from all masks ---------------------------------------------

basin_evi <- full_join(evi_nomask, evi_cci_veg_broad,
                       by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_veg_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_veg_fs, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_c_broad, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_c_narrow, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_c_irrigated, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_cci_c_rainfed, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_af_c, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_esri_c, by = c("HYBAS_ID", "year")) %>%
  full_join(evi_esri_veg, by = c("HYBAS_ID", "year"))


# saving the data -------------------------------------------------------------

write_csv(basin_evi, p("basins_evi/basin_evi_update_revision.csv"))
saveRDS(basin_evi, p("basins_evi/basin_evi_update_revision.RDS"))

