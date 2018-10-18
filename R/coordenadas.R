#' @import dplyr
NULL


#' Muestra datos disponibles para los cantones
#'
#' Muestra los datos que tenemos para cantones. Sin argumentos (o con el
#' argumento "todo") la función va mostrar todos los datos disponibles.
#'
#' @param canton
#'
#' @return
#' @export
crgeo_canton <- function(canton = "todo") {

  cantones <- crgeodata$cantones

  if (canton == "todo") {
    coordenadas <- cantones
  } else {
  coordenadas  <- cantones %>%
    filter(canton == !!canton)
  }
  return(coordenadas)
}

#' Muestra datos disponibles para los distritos
#'
#' Muestra los datos que tenemos para distritos. Sin argumentos (o con el
#' argumento "todo") la función va mostrar todos los datos disponibles.
#'
#' @param canton
#'
#' @return coordenadas
#' @export
crgeo_distrito <- function(distrito = "todo") {

  distritos <- crgeodata$distritos

  if (distrito == "todo") {
    coordenadas <- distritos
  } else {
  coordenadas  <- cantones %>%
    filter(distrito == !!distrito)
  }
  return(coordenadas)
}

#' Busca coordenadas de cantones en formato WSG84-GPS
#'
#' Para poder crear mapas con, por ejemplo, Leaflet necesitamos coordenadas
#' sobre el sistema WSG84-GPS, pero los datos oficiales de costa rica usan
#' CRTM05. Esta función devuelve datos en WSG84-GPS.
#'
#' @param canton
#'
#' @return coordenadas Coordenadas del canton en WSG84-GPS
#' @export
canton_wsg84 <- function(canton) {

  cantones <- crgeodata$cantones

  coordenadas  <- cantones %>%
    filter(canton == !!canton) %>%
    rename(lat = latitud_wgs84) %>%
    rename(lng = longitud_wsg84) %>%
    select(lat, lng)

  return(coordenadas)
}


#' Busca coordenadas de distritos en formato WSG84-GPS
#'
#' Para poder crear mapas con, por ejemplo, Leaflet necesitamos coordenadas
#' sobre el sistema WSG84-GPS, pero los datos oficiales de costa rica usan
#' CRTM05. Esta función devuelve datos en WSG84-GPS.
#'
#' @param canton
#'
#' @return coordenadas Coordenadas del distrito en WSG84-GPS
#' @export
distrito_wsg84 <- function(canton, unidad_territorial) {

  distritos <- crgeodata$distritos

  coordenadas  <- distritos %>%
    filter(canton == !!canton) %>%
    filter(unidad_territorial == !!unidad_territorial) %>%
    rename(lat = latitud_wgs84) %>%
    rename(lng = longitud_wsg84) %>%
    select(lat, lng)

  if (nrow(coordenadas) == 0 ) {
    mensaje <- paste("No pude encontrar esa combinacion de canton y unidad territorial\n",
                      canton, unidad_territorial )
    warning(mensaje)
    coordenadas <- data.frame(lat = NA,
                              lng = NA)
  }

  return(coordenadas)
}
