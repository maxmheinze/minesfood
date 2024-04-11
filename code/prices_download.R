

# Header ------------------------------------------------------------------

# remotes::install_github("dickoa/rhdx")

rm(list = ls())

pacman::p_load(
  tidyverse,
  rhdx,
  countrycode,
  magrittr
)

set_rhdx_config(hdx_site = "prod")



# Define List of Countries ------------------------------------------------

# Create a lookup table of country "names" and ISO3 codes

wfp_countries <- pull_dataset("global-wfp-food-prices") %>%
  get_resource(1) %>%
  read_resource() %>%
  dplyr::select(1,2) %>%
  mutate(hdx_code = substr(url, 54, length(url))) %>%
  dplyr::select(-url)

# Extract a vector of the country names used on HDX

pullcountries <-  wfp_countries$hdx_code



# Get Food Prices from HDX ------------------------------------------------

# Extract dataset for all of the countries in the vector

for (ctry in pullcountries) {
  
  # Read in the dataset
  
  countrydata <- pull_dataset(paste0("wfp-food-prices-for-", ctry)) %>%
    get_resource(1) %>%
    read_resource()
  
  # Add the ISO code from the lookup table
  
  countrydata %<>%
    mutate(country = ctry) %>%
    left_join(wfp_countries, by = join_by(country == hdx_code)) %>%
    dplyr::select(-country) %>%
    relocate(countryiso3, .before = admin1)
  
  # Assign to an object with the HDX country name as name
  
  assign(ctry, countrydata)
  
}

# Remove everything that's not a country-level food prices df

rm(ctry, countrydata, pullcountries, wfp_countries)

# Rbind all of them into one dataset

foodprices <- do.call("rbind", mget(ls()))

# Save it

write_csv(foodprices, "./data/foodprices.csv")




