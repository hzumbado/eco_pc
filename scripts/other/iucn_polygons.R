
#install.packages("devtools")
devtools::install_github("IUCN-UK/iucnredlist")


library(iucnredlist)
library(sf)
library(tmap)
library(tidyverse)

api <- init_api("f8tyLRP2mDVkiDQn24J4dYsiipuWhNV9N9TE")

c_taurus <-
  assessments_by_name(
    api,
    genus = "craugastor",
    species = "taurus")

class(species)

species <-
  read_sf('shapefiles/c_melanostictus/data_0.shp')




species %>%
  tm_shape() +
  tm_polygons() +
  tm_shape(
    species %>%
      filter(
        COMPILER == 'IUCN')) +
  tm_polygons('red')


species %>%
  write_sf('shapefiles/c_melanostictus.gpkg')


sp1 <- read_sf('shapefiles/c_fitzingeri.gpkg')

sp2 <- read_sf('shapefiles/c_melanostictus.gpkg')

tm_shape(sp2) +
  tm_borders()

bind_rows(sp1, sp2) %>%
  st_union() %>%



