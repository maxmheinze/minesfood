
# Africover cropland mask
cropland_evi_basins_1 <- read_csv(p("basins_evi/02_mean_africover_cropland_evi_basin.csv"))

evi_basins_1 <- read_csv(p("basins_evi/01_mean_evi_basin.csv"))

# ESA cropland mask
cropland_evi_basins_3 <- read_csv(p("basins_evi/03_ESA_cropland_evi.csv"))

# Prepare the datasets
evi_basins_test1 <- cropland_evi_basins_1 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  mutate(year = year(image_date), source = "africover")

evi_basins_test2 <- cropland_evi_basins_3 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  mutate(year = year(image_date), source = "ESA")

evi_basins_test3 <- evi_basins_1 %>% 
  filter(HYBAS_ID == "1121273560") %>%
  arrange(image_date) %>% 
  mutate(year = year(image_date), source = "Base")

# Combine the datasets
evi_basins_combined <- bind_rows(evi_basins_test1, evi_basins_test2, evi_basins_test3)

# Plot the smoothed fit for all three datasets with different colors
ggplot(evi_basins_combined %>% filter(year == 2019), aes(x = image_date, y = mean_EVI, color = source)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Mean EVI over Time with Polynomial Fit",
       x = "Date",
       y = "Mean EVI") +
  theme_minimal() +
  scale_color_manual(values = c("africover" = "black", "ESA" = "red", "Base" = "darkgreen"))


