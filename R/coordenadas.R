#' @import dplyr
NULL

#' Busca coordenadas de cantones por nombre
#'
#' @param canton
#'
#' @return
#' @export
coord_canton <- function(canton) {
  cantones <- crgeodata$cantones

  coordenadas  <- cantones %>%
    filter(canton == !!canton)

  return(coordenadas)
}

#' Busca coordenadas de cantones en formato leaflet
#'
#' @param canton
#'
#' @return
#' @export
canton_leaflet <- function(canton) {
  cantones <- crgeodata$cantones

  coordenadas <- cantones %>%
    select(latitud_geografica, longitud_geografica) %>%
    rowwise() %>%
    mutate(lat = str_replace(latitud_geografica, "[°,o]", ".")) %>%
    mutate(lat = str_replace(lat, "’", "")) %>%
    mutate(lat = str_replace(lat, "”", "")) %>%
    mutate(lng = str_replace(longitud_geografica, "[°,o]", ".")) %>%
    mutate(lng = str_replace(lng, "’", "")) %>%
    mutate(lng = str_replace(lng, "”", "")) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lng = -as.numeric(lng)) %>%
    select(lat, lng)

  coordenadas  <- cantones %>%
    filter(canton == !!canton)

  return(coordenadas)
}
