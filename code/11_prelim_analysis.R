# Header ------------------------------------------------------------------

# remotes::install_github("dickoa/rhdx")

rm(list = ls())

pacman::p_load(
  tidyverse,
  rhdx,
  countrycode,
  magrittr
)


# load food prices  -------------------------------------------------------

foodprices <- read_csv("data_local/foodprices.csv")


# looking at the data -----------------------------------------------------

GHA_prices <- foodprices %>% 
  filter(countryiso3 == "GHA") %>%
  filter(commodity %in% c("Maize","Yam")) %>%
  filter(unit == "KG") 

# plotting Ghana Maize Prices across markets
GHA_maize <- GHA_prices %>% filter(commodity == "Maize")

ggplot(GHA_maize, aes(x = date, y = price)) +
  geom_line() + 
  facet_wrap(~market)



GHA_prices <- foodprices %>% 
  filter(countryiso3 == "TZA") %>%
  filter(commodity %in% c("Wheat")) %>%
  filter(unit == "100 KG") 

ggplot(GHA_prices, aes(x = date, y = price)) +
  geom_line() + 
  facet_wrap(~admin_1)

