
library("dplyr")
library("ggplot2")
sapply(list.files("../R", ".R$"), \(f) {source(paste0("../R/", f)); TRUE})

theme_set(theme_minimal(base_size = 12) + theme(
  plot.title = element_text(color = "#333333", size = 14, face = "bold"),
  axis.title.x = element_text(color = "#333333", size = 11, face = "bold"),
  axis.title.y = element_text(color = "#333333", size = 11, face = "bold"),
  text = element_text(family = "Lato Sans"),
  legend.position = "none")
)


restr_year <- 2016:2023

df_reg <- readRDS(p("processed/df_reg.RDS")) |> filter(year %in% restr_year)

df_reg <- df_reg |> 
  mutate( # Signed shorthands
    dist = distance * ifelse(status == "upstream", -1, 1),
    ord = order * ifelse(status == "upstream", -1, 1),
  )

# Subset to mines w/ up- and downstream
df_reg |> group_by(mine_basin) |> 
  summarize(min = min(ord), max = max(ord)) |> 
  group_by(min, max) |> 
  summarize(n = n(), value = cut(n(), c(-Inf, 0, 1, 2, 3, 4, 5, 10, 20, 50, 100, Inf))) |> 
  ggplot(aes(x = -min, y = max, fill = value)) + 
  scale_fill_viridis_d() + 
  scale_color_viridis_d(direction = -1) +
  geom_raster() + geom_text(aes(label = n, col = value)) +
  ggtitle("Mine basin extents") +
  ylab("Number of downstream basins") + xlab("Number of upstream basins")

ggsave("outputs/mine-basin_extents.png", height = 4, width = 5)
  
ids <- df_reg |> group_by(mine_basin) |> 
  summarize(min = min(ord), max = max(ord)) |> 
  filter(min < -3, max > 3) |> pull(mine_basin)

df_reg |> filter(mine_basin %in% ids) |> group_by(mine_basin, status) |> 
  summarize(value = mean(max_EVI)) |> tidyr::pivot_wider(names_from = status)

df_reg |> filter(mine_basin %in% ids) |> group_by(mine_basin, status) |> 
  summarize(value = mean(max_EVI)) |> tidyr::pivot_wider(names_from = status) |> 
  filter(downstream < upstream) |> 
  transmute(v = downstream - upstream) |> arrange(v) |> pull(mine_basin) -> id_discrep

df_reg |> filter(mine_basin %in% ids) |> group_by(mine_basin, status) |> 
  filter(order <= 1) |>
  summarize(value = mean(max_EVI)) |> tidyr::pivot_wider(names_from = status) |> 
  filter(downstream < upstream) |> 
  transmute(v = downstream - upstream) |> arrange(v) |> pull(mine_basin) -> id_jump

df_reg |> filter(mine_basin %in% id_jump[!id_jump %in% id_hand]) |>
  ggplot(aes(x = dist, y = max_EVI, 
    group = factor(year), color = order)) +
  geom_point() + geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c() +
  geom_vline(xintercept = -0.5, col = "gray") +
  facet_wrap(~ mine_basin, scales = "free_y")

id_hand <- c(
  1120626610, 1120646490, 1120750930, 1120779630, 1120789240, 1120917960, 
  1121351150, 1121856160, 1120676692, 1120956310, 1121167700, 1121227390,
  1121234010, 1121401751, 1122192740, 1120113840, 1121290010,
  1120149060, 1120486920, 1120701340, 1120771890, 1121284950, 1121335732,
  1121374460, 1121406230, 1121583640, 1121611840, 1121908682, 1121920630,
  1121960502, 1122205610, 1122289440
)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = dist, y = max_EVI, color = order)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_point() + geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_dist-discont.png", height = 6, width = 8, scale = 1.5)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = dist, y = max_EVI, color = order)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_point() + # geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_dist-discont_wo.png", height = 6, width = 8, scale = 1.5)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = ord, y = max_EVI, color = distance)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_point() + geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_ord-discont.png", height = 6, width = 8, scale = 1.5)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = ord, y = max_EVI, color = distance)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_point() + # geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_ord-discont_wo.png", height = 6, width = 8, scale = 1.5)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = ord, y = max_EVI)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_jitter(aes(col = distance), alpha = .5) +
  geom_boxplot(aes(group = ord), outliers = FALSE) + 
  geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  # scale_color_viridis_d(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_ord-boxplot.png", height = 6, width = 8, scale = 1.5)

df_reg |> filter(mine_basin %in% id_hand) |> # filter(distance <= 50) |> 
  mutate(label = paste0(mine_basin, " (", iso3c, ")")) |> 
  ggplot(aes(x = ord, y = max_EVI)) +
  geom_vline(xintercept = 0, col = "gray", linetype = 2) +
  geom_jitter(aes(col = distance), alpha = .5) +
  geom_boxplot(aes(group = ord), outliers = FALSE) + 
  # geom_smooth(aes(group = status), method = "lm") +
  scale_color_viridis_c(begin = .2, end = .8, direction = -1) +
  # scale_color_viridis_d(begin = .2, end = .8, direction = -1) +
  facet_wrap(~ label, scales = "free_y") +
  ggtitle("Mine basin discontinuity") +
  ylab("Maximum EVI") + xlab("Distance to the mine basin")
ggsave("outputs/mine-basin_ord-boxplot_wo.png", height = 6, width = 8, scale = 1.5)
