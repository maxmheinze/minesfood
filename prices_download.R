

# Header ------------------------------------------------------------------

# remotes::install_github("dickoa/rhdx")

pacman::p_load(
  tidyverse,
  rhdx
)

set_rhdx_config(hdx_site = "prod")



# Define List of Countries ------------------------------------------------

# search_datasets("Food Prices Zambia", rows = 1)

pullcountries <-  c("united-republic-of-tanzania", "zambia")


# Get Food Prices from HDX ------------------------------------------------

for (ctry in pullcountries) {
  countrydata <- pull_dataset(paste0("wfp-food-prices-for-", ctry)) %>%
    get_resource(1) %>%
    read_resource()
  
  assign(ctry, countrydata)
  
  rm(countrydata)
}


