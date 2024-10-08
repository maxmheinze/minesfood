# Header ------------------------------------------------------------------

# remotes::install_github("dickoa/rhdx")

rm(list = ls())

pacman::p_load(
  tidyverse,
  rhdx,
  countrycode,
  magrittr,
  terra,
  countrycode,
  haven,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  readxl,
  fixest
)


# load food prices  -------------------------------------------------------

foodprices <- read_csv("data/foodprices.csv")



# looking at the data -----------------------------------------------------

GHA_prices <- foodprices %>%
  filter(countryiso3 == "GHA") %>%
  filter(commodity %in% c("Maize","Wheat", "Yam"))

# plotting Ghana Maize Prices across markets
GHA_maize <- GHA_prices %>%
  filter(commodity == "Maize") %>%
  filter(unit %in% c("100 KG"))

ggplot(GHA_maize, aes(x = date, y = price)) +
  geom_line() +
  facet_wrap(~market)


# Creating Map of Market Locations ----------------------------------------


TZA_prices <- foodprices %>%
  filter(countryiso3 == "TZA") %>%
  filter(commodity %in% c("Maize")) %>%
  filter(unit %in% c("100 KG")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84")

ggplot(TZA_prices, aes(x = date, y = price)) +
  geom_line() +
  facet_wrap(~admin1)

TAZ_map <- ne_countries(scale = 110, country = "United Republic of Tanzania", returnclass = "sf")



# loading commodity prices from world bank pink sheet ---------------------

# Replace 'path_to_file.xlsx' with the actual file path
commodity <- read_excel("data/CMO-Historical-Data-Annual.xlsx", sheet = 'Annual Prices (Real)', skip = 6)

# Filter the data for the gold price column and the years 2014 to 2023
commodity_prices <- commodity %>%
  select('...1', 'Gold', 'Zinc', 'Aluminum', 'Copper', 'Coal, South Afican', 'Iron ore, cfr spot', 'Nickel', 'Crude oil, average', 'Cocoa', 'Sugar, world', 'Coffee, Robusta', 'Coffee, Arabica') %>%
  filter(...1 >= 1995 & ...1 <= 2023) %>%
  rename("year" = "...1") %>%
  rename("coal" = "Coal, South Afican") %>%
  rename("iron" = "Iron ore, cfr spot") %>%
  mutate(across(c(2:10), as.numeric)) %>%
  mutate(l_gold = lag(Gold, 1)) %>%
  mutate(l2_gold = lag(Gold, 2)) %>%
  mutate(l_nickel = lag(Nickel, 1)) %>%
  mutate(l2_nickel = lag(Nickel, 2)) %>%
  mutate(l_zinc = lag(Zinc, 1)) %>%
  mutate(l2_zinc = lag(Zinc, 2)) %>%
  mutate(l_aluminium = lag(Aluminum, 1)) %>%
  mutate(l2_aluminium = lag(Aluminum, 2)) %>%
  mutate(l_copper = lag(Copper, 1))  %>%
  mutate(l2_copper = lag(Copper, 2))  %>%
  mutate(l_iron = lag(iron, 1)) %>%
  mutate(l2_iron = lag(iron, 2)) %>%
  mutate(l_coal = lag(coal, 1)) %>%
  mutate(l2_coal = lag(coal, 2))



# merging food prices and global commodity prices -------------------------

prices <- foodprices %>%
  filter(countryiso3 %in% c("GHA"))  %>%
  filter(commodity %in% c("Maize")) %>% # Selecting the Commodity
  filter(unit %in% c("KG")) %>%
  filter(pricetype == "Retail") %>%
  mutate(year = year(date)) %>%
  left_join(commodity_prices) %>%
  group_by(market, year) %>%
  mutate(price = mean(price)) %>%
  distinct(market,year, admin1, price, admin2, commodity, countryiso3, longitude, latitude, Gold, l_gold) %>%
  filter(!is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84" , agr = "constant")


# loading mining polygons -------------------------------------------------

mines_sf <- st_read("data/polygons/polygons_V2.shp")

Mines <- mines_sf #%>%
  #filter(COUNTRY %in% c("United Republic of Tanzania","Zambia", "Ghana"))

# Transform both datasets to a local projection (for example, UTM zone for Ghana)
mines_sf_ <- st_transform(Mines, 32630)  # UTM zone 30N
markets_sf <- st_transform(prices, 32630)

# Filtering for market locations
market_points <- markets_sf %>%
  distinct(market, geometry)

# Calculate distances to nearest mine
distance_to_mine <- st_distance(market_points, mines_sf_) # matrix of markets to mining polygons // We will have to make this faster to scale the code
nearest_distance <- apply(distance_to_mine, 1, min)  # get the minimum distance for each market

# Adding names to columns of market points distance vector
market_distances <- cbind(market_points, nearest_distance)

# converting to tibble to conduct merging operation
market_distances %<>%
  as_tibble()

# Adding Distances to Market Locations
markets_sf_gha_dist <- left_join(markets_sf, market_distances)

markets_distances <- markets_sf_gha_dist %>%
  mutate(nearest_distance = nearest_distance/1000) %>%
  mutate(mine = ifelse(nearest_distance < 50, 1, 0))


# Descriptices and Sanity Checks on the Food Price Data -------------------

m1 <- summary(lm(log(price) ~
  (mine) * log(Gold) + as.factor(year) * admin1, markets_distances))

summary(fixest::feols(log(price) ~
  (mine) * log(l_gold) | admin1^year, markets_distances))

m3 <- feols(log(price) ~
  (mine) * log(Gold)  | admin1 + date, markets_distances)

m1$coefficients["mine:log(Gold)", ]
m1$coefficients["mine:log(l_gold)", ]

m2 <- summary(lm(log(price) ~
  (nearest_distance) + as.factor(year) + admin1, markets_distances))
m2

fixest::feols(log(price) ~ (nearest_distance) | admin1 + year, markets_distances)

# loading PRIO-GRID raster ------------------------------------------------
