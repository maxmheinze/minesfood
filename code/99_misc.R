library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)


# Plot of tail storage facilities
tsf <- read_sf("/data/jde/mines/tail_storage_facilities/Global_TSF_intact.shp")
world <- ne_countries()
world <- world |> filter(admin != "Antarctica")

pdf("./output/plots/map_tsf.pdf", width = 9, height = 6)
ggplot() + 
  geom_sf(data = world, color = "white", fill = "grey") + 
  geom_sf(data = tsf, 
          size = 1, shape = 22, fill = "blue", alpha = 0.75) + 
  theme_bw()
dev.off()

# getting subset of active mines from 
mines <- read_sf("/data/jde/mines/mining_polygons_predicted_allyears.gpkg")
mines <- mines |> mutate(mine_id = seq_len(nrow(mines)), .before = iso3)
basins <- read_sf("/data/jde/processed/relevant_basins_ordered.gpkg") |> 
  filter(order == 0)

int <- st_intersects(basins, mines)

# 165 mine basins feature no mine according to Philipp's data -> classify as inactive
sum(lengths(int) == 0)

names(int) <- basins$mine_basin
int_mines <- int[which(lengths(int) > 0)]

basin_mines <- data.frame(
  "mine_basin" = rep(names(int_mines), lengths(int_mines)),
  "mine_id" = int_mines |> purrr::map_dfr(as_tibble) |> pull(value)) |> 
  left_join(mines |> st_drop_geometry(), by = "mine_id") |> 
  group_by(mine_basin, year) |> 
  summarise(mine_area = sum(mine_area, na.rm = T)) |> 
  arrange(mine_basin, year)

grid_basins <- expand.grid(mine_basin = unique(basin_mines$mine_basin), year = 2016:2023) |> 
  arrange(mine_basin, year)

g_basin_mines <- left_join(grid_basins, basin_mines) |> 
  group_by(mine_basin) |> 
  mutate(mine_area = replace(mine_area, is.na(mine_area), 0),
         yearly_growth = mine_area - lag(mine_area, order_by = year), 
         abs_growth_2016 = mine_area - mine_area[year == 2016], 
         abs_growth_2017 = mine_area - mine_area[year == 2017], 
         rel_growth_2016 = (mine_area - mine_area[year == 2016]) / mine_area[year == 2016], 
         rel_growth_2016 = ifelse(is.infinite(rel_growth_2016), 1, rel_growth_2016),
         rel_growth_2016 = ifelse(is.na(rel_growth_2016), 0, rel_growth_2016),
         rel_growth_2017 = (mine_area - mine_area[year == 2017]) / mine_area[year == 2017], 
         rel_growth_2017 = ifelse(is.infinite(rel_growth_2017), 0, rel_growth_2017), 
         rel_growth_2017 = ifelse(is.na(rel_growth_2017), 1, rel_growth_2017)) 

# 1024 mine basins show positive growth, 670 negative growth, 41 no growth (0 area in 2017 and 2023)
g_basin_mines |> filter(abs_growth_2017 > 0, year == 2023)
g_basin_mines |> filter(abs_growth_2017 < 0, year == 2023)
g_basin_mines |> filter(abs_growth_2017 == 0, year == 2023)

id_growth_abs_2016 <- g_basin_mines |> filter(abs_growth_2016 > 0, year == 2023) |> pull(mine_basin)
id_growth_abs_2017 <- g_basin_mines |> filter(abs_growth_2017 > 0, year == 2023) |> pull(mine_basin)
id_growth_rel0.05_2016 <- g_basin_mines |> filter(rel_growth_2016 > 0.05, year == 2023) |> pull(mine_basin)
id_growth_rel0.05_2017 <- g_basin_mines |> filter(rel_growth_2017 > 0.05, year == 2023) |> pull(mine_basin)
id_growth_rel0.1_2016 <- g_basin_mines |> filter(rel_growth_2016 > 0.1, year == 2023) |> pull(mine_basin)
id_growth_rel0.1_2017 <- g_basin_mines |> filter(rel_growth_2017 > 0.1, year == 2023) |> pull(mine_basin)
id_growth_rel0.25_2016 <- g_basin_mines |> filter(rel_growth_2016 > 0.25, year == 2023) |> pull(mine_basin)
id_growth_rel0.25_2017 <- g_basin_mines |> filter(rel_growth_2017 > 0.25, year == 2023) |> pull(mine_basin)

id_active_mines <- tibble(mine_basin = unique(basins$mine_basin)) |> 
  mutate(active_abs_2016 = ifelse(mine_basin %in% id_growth_abs_2016, 1, 0),
         active_abs_2017 = ifelse(mine_basin %in% id_growth_abs_2017, 1, 0),
         active_rel0.05_2016 = ifelse(mine_basin %in% id_growth_rel0.05_2016, 1, 0),
         active_rel0.05_2017 = ifelse(mine_basin %in% id_growth_rel0.05_2017, 1, 0),
         active_rel0.1_2016 = ifelse(mine_basin %in% id_growth_rel0.1_2016, 1, 0),
         active_rel0.1_2017 = ifelse(mine_basin %in% id_growth_rel0.1_2017, 1, 0),
         active_rel0.25_2016 = ifelse(mine_basin %in% id_growth_rel0.25_2016, 1, 0),
         active_rel0.25_2017 = ifelse(mine_basin %in% id_growth_rel0.25_2017, 1, 0))

saveRDS(id_active_mines, "/data/jde/processed/filter_active_mines.RDS")


