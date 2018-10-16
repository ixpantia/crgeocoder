library(tabulizer)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(feather)

# Extraccion de tablas del archivo ----------------------------------------
archivo <- ("data/cuadros_coordenadas.pdf")
tabla <- extract_tables(archivo, pages = 1:14)
tabla <- do.call("rbind", tabla)
resguardo <- tabla
tabla <- as.data.frame(tabla)


# Arreglo desfase de hoja 1 -----------------------------------------------
## La hoja 1 hay que darle un tratamiento diferente
## porque tiene columnas diferentes:
## va de la fila 1 a la 22
hoja_1 <- tabla %>%
  slice(1:22)

hoja_1 <- hoja_1 %>%
   select(-V3)

## Tomar primeras dos columnas correctas
v1_2 <- hoja_1 %>%
  select(V1, V2)

## Unir en una sola columna V4 y V5
v4_5 <- hoja_1 %>%
  select(V4, V5)

## Subset de v4 hasta row 4 para unir v5 desde row 5
v4 <- v4_5 %>%
  slice(1:4) %>%
  select(V4)

v5 <- v4_5 %>%
   slice(-(1:4)) %>%
  select(V5)

## Ponerle nombre V4 a columna V5 para que el rbind funcione:
colnames(v4) <- "V3"
colnames(v5) <- "V3"

## Unir los nuevos v4  y v5 en columna llamada v3
v3 <- rbind(v4, v5)

## Unir con primeras dos columnas que habiamos separado al inicio:
stopifnot(nrow(v3) == nrow(hoja_1))

v1_2_3 <- data.frame(v1_2, v3)

## Aca hasta V3 estamos bien. En columnas V6 y V7 hay que separarlas
## cada una en dos columnas con LATITUD y LONGITUD respectivamente:

## v6 y v7 sin primeros 4 rows
v6_7 <- hoja_1 %>%
  select(V6, V7) %>%
  slice(-(1:4))

v6 <- v6_7 %>%
  select(V6) %>%
  separate(V6, into = c("V4", "V5"), sep = " ")

v7 <- v6_7 %>%
  select(V7) %>%
  separate(V7, into = c("V6", "V7"), sep = " ")

stopifnot(nrow(v6) == nrow(v7))

## AHora hay que unirlas y dejar 4 rows en blanco para que filas calcen
## como antes de hacer el subconjunto
blanco <- data.frame("V4" = c(NA, NA , NA, NA), "V5" = c(NA, NA , NA, NA),
                     "V6" = c(NA, NA , NA, NA), "V7" = c(NA, NA , NA, NA))
## Unir los subconjuntos creados con columnas separadas v6 y v7:
v4_5_6_7 <- data.frame(v6, v7)

## Poner primeros 4 espacios en blanco en v4_5_6_7
stopifnot(ncol(blanco) == ncol(v4_5_6_7))

v4_5_6_7 <- rbind(blanco, v4_5_6_7)

## Unir v4_5_6_7 a primeras tres columnas limpias:
stopifnot(nrow(v1_2_3) == nrow(v4_5_6_7))

hoja_1 <- data.frame(v1_2_3, v4_5_6_7)

## Unir a tabla completa sin desfase
### Primero eliminar el segmento que corresponde hoja_1 de la tabla completa
tabla <- tabla %>%
  slice(-(1:22))

### Ahora unir con hoja_1 limpia sin columnas desfasadas:
stopifnot(ncol(hoja_1) == ncol(tabla))

tabla <- rbind(hoja_1, tabla)

# Nombres de la tabla -----------------------------------------------------
nombres_tabla <- c("unidad_territorial", "area", "poblacion",
                   "latitud_geografica", "longitud_geografica",
                    "latitud_crtm05", "longitud_crtm05")

colnames(tabla) <- nombres_tabla


# Eliminar ultima fila ----------------------------------------------------

tabla <- tabla[-556, ]

# Eliminacion filas en blanco ---------------------------------------------
prueba_na <- tabla

## Usar janitor para remover espacios en blanco:
prueba_na[prueba_na == ""] = NA

### Prueba para tener misma cantidad de observaciones en original como con NA
stopifnot(nrow(prueba_na) == nrow(tabla))

### Contar cantidad de filas con NA:
sum(is.na(prueba_na$unidad_territorial))

## Remover NA basados en la columna de unidad territorial:
prueba_na <- prueba_na %>%
  drop_na(unidad_territorial)

stopifnot(sum(is.na(prueba_na$unidad_territorial)) == 0)

tabla <- prueba_na

## ELiminar filas que contienen "UNIDAD TERRITORIAL"
tabla <- tabla %>%
  filter(unidad_territorial != "UNIDAD TERRITORIAL")

## Conteo cantidad de cantones presentes en el conjunto de dato:
sum(str_detect(tabla$unidad_territorial, "^CANTÓN")) # DA 81

#### En la publicacon de la gaceta hay 81 cantones porque el canton
#### de rio cuarto que es el #82 se creo hasta el 2017

# Seleccion de distritos --------------------------------------------------

## Generar nueva columna que contenga el canton al que pertenece el distrito:

### Con solucion de stackoverflow:
x <- gsub('^CANTÓN', '', tabla$unidad_territorial)
x[!grepl('^CANTÓN', tabla$unidad_territorial)] <- NA
tabla$canton <- ave(x, cumsum(!is.na(x)), FUN = function(xx) xx[1])

### Este sirve para tener los distritos porque estan completos con los datos:
distritos <- tabla %>%
  filter(!str_detect(tabla$unidad_territorial, "^CANTÓN"))

stopifnot((nrow(tabla) - nrow(distritos)) == 81)

# Seleccion de cantones ---------------------------------------------------
## Seleccion fila con canton y la siguiente fila a cada una de estas:
canton_posicion <- which(str_detect(tabla$unidad_territorial, "CANTÓN"))
stopifnot(nrow(canton_posicion) == 81)

canton_posicion_1 <- which(str_detect(tabla$unidad_territorial, "CANTÓN")) + 1
stopifnot(nrow(canton_posicion_1) == 81)

cantones <- tabla[c(canton_posicion, canton_posicion_1), ]
cantones <- cantones %>%
  arrange(canton)

stopifnot(length(cantones) != 162)

## Copiar la latitud y longitud de distrito de cabecera a cada canton
cantones_coordenadas <- cantones %>%
  select(-area, -poblacion) %>%
  filter(!str_detect(cantones$unidad_territorial, "^CANTÓN")) %>%
  select(-unidad_territorial)

stopifnot(nrow(cantones_coordenadas) == nrow(canton_posicion))

## Unir datos de cantones con datos de cantones coordenadas
## cantones_coordenadas ahora poseen coordenadas de sus cantones de cabecera

cantones_interes <- cantones %>%
  filter(is.na(latitud_geografica)) %>%
  select(area, poblacion, canton)

# cantones_interes <- cantones %>%
#   filter(!str_detect(cantones$unidad_territorial, "^CANTÓN")) %>%
#   select(area, poblacion, canton)

stopifnot(nrow(cantones_interes) == 81)
stopifnot(length(unique(cantones_coordenadas$canton)) == length(unique(cantones_interes$canton)))

cantones <- left_join(cantones_coordenadas, cantones_interes, by = "canton")

stopifnot(nrow(prueba) == 81) # EN algun lado hay dos cantones mas

# Formato de valores en coordenadas y nombres -----------------------------

## Para datos de distritos ----

### Unidad territorial y canton:
distritos <- distritos %>%
  mutate(unidad_territorial = str_replace(unidad_territorial, "([0-9\\s]+)", ""),
         canton = str_replace(canton, "([0-9\\s]+)", ""))

distritos <-  distritos %>%
  select(canton, everything())

### lat y long:


## Para datos de cantones ----
cantones <- cantones %>%
  mutate(canton = str_replace(canton, "([0-9\\s]+)", ""))

## Pasar columna canton al inicio del connjunto de datos
cantones <- cantones %>%
  select(canton, everything())

### lat y long:


# Exportacion de datos ----------------------------------------------------

write_feather(cantones, "data/cantones.feather")
write_feather(distritos, "data/distritos.feather")
