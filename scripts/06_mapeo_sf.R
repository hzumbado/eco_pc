# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Mapeo terra

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(geodata)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(car)
library(mapmisc)

# data --------------------------------------------------------------------

species_coords <-
  read_csv("data/raw/species_coords.csv") %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326,
    remove = FALSE)

list.files(
  'shapefiles',
  pattern = ('cit|lak|nam|riv|world'),
  full.names = TRUE) %>%
  map(~.x %>%
        read_sf() %>%
        st_make_valid()) %>%
  set_names(
    'cities',
    'rivers_europe',
    'lakes',
    'namibia_adm',
    'rivers',
    'world') %>%
  list2env(.GlobalEnv)

# Africa ------------------------------------------------------------------

species_coords


world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(species_coords) +
  tm_symbols(
    fill = 'species',
    size = 0.3,
    col = 'black',
    fill.legend = tm_legend('Species'),
    fill.scale =
      tm_scale_categorical(values = "brewer.set2"))


tm_shape(world %>%
           filter(COUNTRY == 'Namibia')) +
  tm_borders()

# cities ------------------------------------------------------------------


# plot with colours defined by column values:

world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(cities) +
  tm_symbols(
    fill = 'CAPITAL',
    size = 0.4,
    col = 'black',
    fill.legend = tm_legend('Capital'),
    fill.scale =
      tm_scale_categorical(values = "brewer.set2"))




# countries ---------------------------------------------------------------



# rivers------------------------------------------------------------------

world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers) +
  tm_lines(
    col = 'blue')

world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers) +
  tm_lines(
    col = 'SYSTEM')


# lakes -------------------------------------------------------------------

world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(lakes) +
  tm_polygons(
    fill = 'blue')

# Namibia -----------------------------------------------------------------

tm_shape(namibia_adm) +
  tm_polygons(
    fill = 'wheat')


sort(unique(namibia_adm$NAME_2))

nam_subset <-
  namibia_adm %>%
  filter(
    NAME_2 %in% c(
      "Luderitz",
      "Gibeon",
      "Karas"))

tm_shape(namibia_adm) +
  tm_polygons(
    fill = 'wheat') +
  tm_shape(nam_subset) +
  tm_polygons(
    fill = 'darkgreen') +
  tm_shape(nam_subset) +
  tm_borders(
    lwd = 3) +
  tm_shape(nam_subset) +
  tm_borders(
    lwd = 1, col = 'white')



# all layers map ----------------------------------------------------------

plot(
  countries,
  "region",
  main = "Map of the world",
  col = terrain.colors(6))

plot(
  cities,
  cex = 0.2,
  col = "darkred",
  add = TRUE)

plot(
  rivers,
  col = "blue",
  add = TRUE)

plot(
  lakes,
  col = "lightblue",
  border = NA,
  add = TRUE)

plot(
  species_points,
  col = "darkgreen",
  pch = 4,
  cex = 0.2,
  add = TRUE)

# specific ranges ---------------------------------------------------------

plot(
  countries,
  border = "grey")

my_window <-
  ext(-20, 60, -40, 40)  # xmin, xmax, ymin, ymax

plot(
  countries,
  border = "grey",
  ext = my_window)

plot(
  rivers,
  col = "blue",
  add = TRUE)

plot(
  cities,
  col = "darkred",
  add = TRUE)

plot(
  species_points,
  col = "darkgreen",
  pch = 4,
  add = TRUE)

# labels ------------------------------------------------------------------

plot(
  countries,
  border = "darkgrey",
  ext = my_window)

text(
  x = countries,
  "name",
  cex = 0.7)

countries_centroids <-
  countries |>
  centroids() |>
  crds()

plot(
  countries,
  border = "darkgrey",
  ext = my_window)

set.seed(1)

label_coords <-
  pointLabel(
  x = countries_centroids,
  labels = countries$name,
  cex = 0.7)

label_coords$y[which(countries$name == "South Africa")]

label_coords$y[which(countries$name == "South Africa")] <- -32

#map again

plot(
  countries,
  border = "darkgrey",
  ext = my_window)

text(
  label_coords$x,
  label_coords$y,
  countries$name,
  col = "blue",
  cex = 0.7)

# plots -------------------------------------------------------------------

plot(bathy)
plot(ncdf$pr)
plot(elevation)
plot(population)

plot(
  elevation,
  col = cm.colors(
    100,
    rev = T))

plot(
  population,
  add = TRUE,
  alpha = 0.6,
  legend = FALSE)


# check layers ------------------------------------------------------------

elevation
population  # notice different dimensions, resolution, extent
countries

ext(elevation)  # spatial extent (min and max coordinates)
crs(elevation, proj = TRUE)  # crs in 'proj' format
res(elevation)  # spatial resolution (pixel size)

ext(countries)
crs(countries, proj = TRUE)
head(countries)


countries$startswithB <-
  startsWith(as.character(countries$name), "B")

plot(
  countries,
  "startswithB",
  lwd = 0.1)


# colours -----------------------------------------------------------------

colours()  # you can search "R colours" online for images

plot(
  countries,
  ext = my_window,
  col = "wheat",
  border = "wheat4",
  background = "lightblue")

plot(
  rivers,
  col = "royalblue",
  lwd = 2,
  add = TRUE)

plot(
  cities,
  pch = 20,
  col = "darkred",
  cex = 0.7,
  add = TRUE)

text(
  subset(
    cities,
    cities$CAPITAL == "Y"),
  "NAME",
  cex = 0.7,
  col = "darkgreen",
  halo = TRUE)


# palette rasters ---------------------------------------------------------

plot(elevation, col = hcl.colors(100))
plot(elevation, col = hcl.colors(100, palette = "inferno"))
plot(elevation, col = hcl.colors(100, palette = "spectral"))
plot(elevation, col = hcl.colors(100, palette = "geyser"))
plot(elevation, col = rainbow(100))
plot(elevation, col = terrain.colors(100))
plot(elevation, col = topo.colors(100))
plot(elevation, col = cm.colors(100))
plot(elevation, col = heat.colors(100))

plot(elevation, col = topo.colors(100, rev = TRUE))
plot(elevation, col = hcl.colors(100, palette = "spectral", rev = TRUE))

plot(
  species_points,
  "species",
  col = hcl.colors(length(unique(species_points$species))))

plot(
  species_points,
  "country",
  col = rainbow(length(unique(species_points$country))))

plot(countries, "pop2005")
plot(
  countries,
  "pop2005",
  col = hcl.colors(100, palette = "geyser"))


# legend breaks -----------------------------------------------------------

pretty_breaks <-
  pretty(countries$pop2005, n = 10)

plot(
  countries,
  "pop2005",
  col = hcl.colors(
    100,
    palette = "geyser"),
  breaks = pretty_breaks)

breaks <-
  seq(
    0,
    max(countries$pop2005),
    by = 100000000)

breaks

plot(
  countries,
  "pop2005",
  col = hcl.colors(
    100,
    palette = "geyser"),
  breaks = breaks)

plot(population)

plot(
  population,
  breaks = quantile(
    values(population),
    na.rm = TRUE))

plot(
  countries,
  "pop2005",
  breaks =
    quantile(countries$pop2005))

plot(
  countries,
  "pop2005",
  breaks = quantile(
    countries$pop2005,
    probs = seq(0, 1, 0.1)))

plot(
  countries,
  "pop2005",
  breaks = quantile(
    countries$pop2005,
    probs = seq(0, 1, 0.1)),
  col = hcl.colors(length(countries),
                   palette = "geyser"),
  main = "World population by country\nin 2005")

# north arrow, scale, inset -----------------------------------------------

plot(
  countries,
  border = "grey",
  ext = my_window)

plot(species_points, add = TRUE)

north(col = "darkblue", font = 2, lwd = 5)

sbar(1000)  # default

sbar(xy = c(-15, -25), d = 1000, type = "bar", below = "km")

inset(
  countries,
  loc = "topleft",
  border = "grey",
  box = ext(my_window),
  pbox = list(col = "darkred", lwd = 2))

terra::perim(rivers)
st_length(st_as_sf(rivers))

# 'mapmisc'
insetMap(
  crs(countries),
  pos = "topleft",
  map = "osm",
  border = NA)

# change the zoom level of theinset map:
insetMap(
  crs(countries),
  pos = "topleft",
  map = "osm",
  border = NA,
  zoom = 2)

# add the inset map at a specific position:
insetMap(
  crs(countries),
  pos = c(-40, 20),
  map = "osm",
  border = NA)


# bubble maps -------------------------------------------------------------

cities$is_capital <-
  ifelse(cities$CAPITAL == "Y", 1, 0)

st_as_sf(cities) |>
  mutate(cap = if_else(CAPITAL == 'Y', 1, 0), .before = 4)

plot(countries, border = "grey")
plot(
  cities,
  pch = 1,
  col = "darkblue",
  cex = cities$is_capital + 0.5,
  add = TRUE)

# pdf map -----------------------------------------------------------------

pdf(
  "outputs2/countries_map.pdf",
  width = 7,
  height = 4)

plot(countries,
     "region",
     main = "Map of the world",
     col = hcl.colors(6, "geyser"))

dev.off()

# CRS ---------------------------------------------------------------------

world %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers_europe) +
  tm_lines(col = 'blue')

st_crs(world)
st_crs(rivers_europe)


# CRS raster --------------------------------------------------------------

elev_laea <-
  project(elevation, countries_laea)

plot(elev_laea)

elev_Namibia <-
  elevation_30s(
    country = "Namibia",
    path = "outputs2/elevation",
    mask = FALSE)

plot(elev_Namibia)
crs(elev_Namibia, proj = TRUE)  # longlat WGS84

elev_Namibia
elev_Namibia[]  # show the values of the raster pixels
values(elev_Namibia)  # same

elev_Namibia_laea <-
  terra::project(
    elev_Namibia,
    countries_laea,
    method = "bilinear")

plot(elev_Namibia_laea)
plot(countries_laea, add = TRUE)

countries_moll <-
  project(countries, "+proj=moll")

plot(countries_moll)

countries_eur <-
  subset(
    countries,
    countries$region == 150
    & countries$name != "Russia")

countries_eur_laea <- project(countries_eur, countries_laea)

plot(
  countries_eur_laea,
  col = "burlywood",
  border = "burlywood4")

# *** HOMEWORK ASSIGNMENT ***

# check available country names and ISO codes:
 names(geodata::country_codes())

 geodata::country_codes()[ , c("NAME", "ISO3")]

 geodata::country_codes() |>
   as_tibble


# ggplot ------------------------------------------------------------------


crs(species_points) <- crs(countries)

world  %>%
  ggplot() +
  geom_sf(
    aes(fill = continent),
    lwd = 0.2) +
  geom_sf(
    data = st_as_sf(species_coords),
    colour = "darkgreen",
    size = 0.5) +
  geom_sf(
    data = st_as_sf(rivers_Eur),
    color = "blue",
    lwd = 0.6) +
  geom_sf(
    data =
      st_as_sf(cities),
    colour = "darkred",
    size = 0.5,
    shape = 23) +
  coord_sf(crs = "+proj=moll")


st_as_sf(cities) %>%
  mutate(cap = if_else(CAPITAL == 'Y', 1, 0), .before = 4)
