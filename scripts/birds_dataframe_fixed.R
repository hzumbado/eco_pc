# Ecologia de poblaciones
# Codigo 01a Lectura de datos pajaros reparado
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(tidyverse)

# data -------------------------------------------------------------

pajaritos_df <-
  read_csv('data/raw/pajaritos.csv') %>%
  select(
    site = 'Punto de muestreo',
    x = X,
    y = Y,
    species = Sp,
    sex = Sexo,
    age = "Estado de desarrollo",
    substrate = Estrato,
    human_presence = "Presencia humana") %>%
  mutate(
    campus = 'omar_dengo',
    .before = site) %>%
  mutate(
    year = 2025,
    month = 08,
    day = 05) %>%
  unite(
    col = 'date', #new column
    year:day,
    sep = '-') %>%
  mutate(
    date = as_date(date),
    across(
      c(campus, site, sex:substrate),
      ~.x %>% as_factor()))

pajaritos_df %>%
  write_csv('data/processed/birds_dataframe.csv')
