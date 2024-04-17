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
  readxl
)


# load food prices  -------------------------------------------------------

foodprices <- read_csv("data_local/foodprices.csv")


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


# Creating Map of Market Locatiosn ----------------------------------------


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
commodity <- read_excel("data_local/CMO-Historical-Data-Annual.xlsx", sheet = 'Annual Prices (Real)', skip = 6)

# Filter the data for the gold price column and the years 2014 to 2023
commodity_prices <- commodity %>%
  select('...1', 'Gold', 'Zinc', 'Aluminum', 'Copper', 'Coal, South Afican', 'Iron ore, cfr spot', 'Nickel') %>%
  filter(...1 >= 1995 & ...1 <= 2023) %>%
  rename("year" = "...1") %>%
  rename("coal" = "Coal, South Afican") %>%
  rename("iron" = "Iron ore, cfr spot") %>%
  mutate(across(c(2:8), as.numeric)) %>%
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
  filter(commodity == "Maize") %>%
  filter(unit %in% c("100 KG")) %>%
  mutate(year = year(date)) %>%
  left_join(commodity_prices) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>%
  filter(countryiso3 %in% c("GHA"))

summary(lm(log(price/usdprice) ~ log(Gold) + as.character((market))*as.factor(date), prices))



# loading mining polygons -------------------------------------------------

mines_sf <- st_read("data_local/polygons/polygons_V2.shp")

# Transform both datasets to a local projection (for example, UTM zone for Ghana)
mines_sf <- st_transform(mines_sf, 32630)  # UTM zone 30N
markets_sf <- st_transform(prices, 32630)

# Calculate distances to nearest mine
distance_to_mine <- st_distance(prices, mines_sf, by_element = TRUE)
nearest_distance <- apply(distance_to_mine, 1, min)  # get the minimum distance for each market

# Add Distance to Data Frame
markets$distance_to_nearest_mine <- nearest_distance



# loading PRIO-GRID raster ------------------------------------------------



