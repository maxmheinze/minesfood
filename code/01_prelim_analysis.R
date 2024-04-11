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
pacman::p_load(
  tidyverse,
  rhdx,
  countrycode,
  magrittr
)
