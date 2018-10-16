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

## como traer coordenadas para leflet lo dejamos en "crudo" o en otro
## script ponemos para crear un mapa o solo para traer formato de coordenadas
## especificas a leaflet
