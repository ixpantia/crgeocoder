#' Busca coordenadas de cantones por nombre
#'
#' @param canton
#'
#' @return
#' @export
coord_canton <- function(canton) {
  cantones <- crgeodata$cantones
  coordenadas  <- cantones %>%
    filter(canton == canton)
  return(coordenadas)
}
