
# setup -------------------------------------------------------------------

rm(list = ls())

library(rinat)
library(tidyverse)

# data --------------------------------------------------------------------

my_species <- 'Isthmohyla_pseudopuma'
query = 'Isthmohyla pseudopuma'

#total of observations

inat_metadata <-
  get_inat_obs(
    query = query, # nombre de la especie sin guion
    meta = TRUE) %>%
  pluck('meta')

# download data

inat_data <-
  get_inat_obs(
    query = query,
    quality = 'research',
    geo = TRUE,
    maxresults = 10000,
    meta = FALSE) %>%
  as_tibble()

# save data ---------------------------------------------------------------

inat_data |>
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_inat_raw.rds'))
