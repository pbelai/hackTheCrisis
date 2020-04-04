redrawMap <- function(map, data, userLocation) {
  .updateMap <- function(map, fun, ...) {
    params <- list(...)
    if (any(unlist(lapply(params, is.null)))) map else map %>% fun(map = ., ...)
  }
  map %>% .updateMap(addPolygons,data) %>% .updateMap(leaflet::addMarkers, lat = userLocation$lat, lng = userLocation$lng)
}
