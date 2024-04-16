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
  sf
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
  filter(unit %in% c("100 KG"))

ggplot(TZA_prices, aes(x = date, y = price)) +
  geom_line() + 
  facet_wrap(~admin1)

TAZ_map <- ne_countries(scale = 110, country = "United Republic of Tanzania", returnclass = "sf")

st_as_sf(TZA_prices)

ggplot() +
  geom_sf(data = TAZ_map, fill = "lightgrey", color = "black") +  # Base map
  geom_sf(data = TZA_prices, aes(color = primary_commodity), alpha = 0.75) +  # Mines with categorized fill
  scale_colour_manual(values = cols) +
  geom_sf(data = GH_TZ_polygons_V2, fill = "yellow", alpha = 0.5) +  # Highlighted areas
  ggtitle("Extent of Mines in Ghana") +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  labs(fill = "Primary Commodity")  # Rename legend title


