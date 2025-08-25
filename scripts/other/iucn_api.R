# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# UICN API

# setup -------------------------------------------------------------------

rm(list = ls())

#install.packages("devtools")
devtools::install_github("IUCN-UK/iucnredlist")

library(iucnredlist)
library(tidyverse)

api <-
  init_api("f8tyLRP2mDVkiDQn24J4dYsiipuWhNV9N9TE")

c_taurus <-
  assessments_by_name(
    api,
    genus = "craugastor",
    species = "taurus")
