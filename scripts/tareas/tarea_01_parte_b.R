# Ecologia de poblaciones
# Tarea 01_parte_b Lectura de datos ordenadas
# Profesor Hector Zumbado Ulate

# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

angola_ungulates <-
  read_rds("data/raw/angola_ungulates.rds") %>%
  bind_rows()


# data ------------------------------------------

ungulates <-
  read_rds('data/processed/ungulates_data.rds')

angola_ungulates <-
  read_csv('data/raw/angola_ungulates.csv')

lobstr::ref(.GlobalEnv)

# 1- Arregle la fecha creando una columna 'date' con las funciones unite() y as_date(). La fecha debe estar ordenada como year, month, day. Asigne al objeto resultante el nombre angola_ungulates_date_fix

angola_ungulates_date_fix <-
  angola_ungulates %>%

# 2- Haga una nueva columna con el nombre cientifico (sci_name) con la informacion de las columnas llamadas 'genus' y 'species' utilizando la funcion unite. Como argumento separador use sep = ' ' de manera que quede un espacio entre el genero y la especie. Luego utilice la funcion rename para cambiar la columna sci_name a 'species'. Asigne al objeto resultante el nombre angola_ungulates_spp_fix.

angola_ungulates_spp_fix <-
  angola_ungulates_date_fix %>%

# 3- Separe la columna taxonomia en tres columnas llamadas 'class', 'order', 'family utilizando la funcion separate. Como argumento separador use sep = '-' de manera que R entienda que cada guion separa un nombre correspondiente a cada columna.

angola_ungulates_taxonomy_fix <-
  angola_ungulates_spp_fix %>%

# haga una lista con 2 elementos llamados 'taxonomy' y 'observations'. Para observations seleccione date, user_login, species). Para el objeto taxonomy utilice species, order, family, common_name. Como el dato Class es el mismo para ambos ordenes (Mammalia) es innecesario ponerlo.

taxonomy <-
  select() %>%
  distinct()

taxonomy

observations <-
  select()

observations
