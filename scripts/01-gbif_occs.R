# Hector Zumbado
# descarga de datos gbif

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(tidyverse)

# data --------------------------------------------------------------------

my_species <- 'Isthmohyla_pseudopuma'

key <-
  name_backbone(my_species) %>%
  pull(usageKey)

key

gbif_download <-
  occ_download(
    pred('taxonKey', key),
    format = 'SIMPLE_CSV',
    user = 'hzumbado',
    pwd = 'Hugozum1980',
    email = 'zumbadohector@gmail.com')

gbif_download

# save citation -----------------------------------------------------------

# para salvar metadata

gbif_download %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_key.rds'))

# leer metadata

gbif_download <-
  read_rds(
  paste0(
  'data/raw/',
  my_species,
  '_key.rds'))

# check download processing -----------------------------------------------

# ver avance de la descarga en datasets grandes

occ_download_wait(gbif_download)

# crear objeto de descarga

data <-
  occ_download_get(
    gbif_download,
    path = 'data/raw',
    overwrite = TRUE) %>%
  occ_download_import()

# data <-
#   occ_download_get(
#     '0006515-250227182430271',
#     path = 'data/raw',
#     overwrite = TRUE)

# save data ---------------------------------------------------------------

data %>%
  write_csv(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.csv'))

data %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))
