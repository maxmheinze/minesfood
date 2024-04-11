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
