#' Title
#'
#' @param x
#' @param y
#' @param zoom
#'
#' @return
#'
#' @import magrittr
createLeaflet <- function(lng = 17.1117078,lat = 48.1473227, zoom=13) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView(lat = lat, lng = lng, zoom = zoom)
}
