
# Africover cropland mask
cropland_evi_basins_1 <- read_csv(p("basins_evi/02_mean_africover_cropland_evi_basin.csv"))

evi_basins_1 <- read_csv(p("basins_evi/01_mean_evi_basin.csv"))

# ESA cropland mask
cropland_evi_basins_3 <- read_csv(p("basins_evi/03_ESA_cropland_evi.csv"))

# Prepare the datasets
evi_basins_test1 <- cropland_evi_basins_1 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  rename(mean_EVI_af = mean_EVI) %>%
  mutate(year = year(image_date), source = "africover")

evi_basins_test2 <- cropland_evi_basins_3 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  rename(mean_EVI_esa = mean_EVI) %>%
  mutate(year = year(image_date), source = "ESA")

evi_basins_test3 <- evi_basins_1 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  mutate(year = year(image_date), source = "Base")

# Combine the datasets
evi_basins_combined <- left_join(evi_basins_test1, evi_basins_test2) %>%
  left_join(evi_basins_test3)

# Filter out rows with NA values in EVI columns
evi_filtered <- evi_basins_combined %>%
  filter(!is.na(mean_EVI_af) | !is.na(mean_EVI_esa) | !is.na(mean_EVI)) %>%
  filter(year == 2010)


# Plot the point observations and smooth fit line
ggplot() +
  geom_point(data = evi_filtered, aes(x = image_date, y = mean_EVI_af, color = 'mean_EVI_af'), col = "blue") +
  geom_smooth(data = evi_filtered, aes(x = image_date, y = mean_EVI_af, color = 'mean_EVI_af'), col = "blue"), method = 'loess', span = 0.2) +
  geom_point(data = evi_filtered, aes(x = image_date, y = mean_EVI_esa, color = 'mean_EVI_esa'), col = "red")) +
  geom_smooth(data = evi_filtered, aes(x = image_date, y = mean_EVI_esa, color = 'mean_EVI_esa'), col = "red"), method = 'loess', span = 0.2) +
  geom_point(data = evi_filtered, aes(x = image_date, y = mean_EVI, color = 'mean_EVI'), col = "green")) +
  geom_smooth(data = evi_filtered, aes(x = image_date, y = mean_EVI, color = 'mean_EVI'),col = "green"), method = 'loess', span = 0.2) +
  labs(title = "EVI Values Over Time (2001)",
       x = "Date",
       y = "EVI Value",
       color = "EVI Type") +
  theme_minimal()
