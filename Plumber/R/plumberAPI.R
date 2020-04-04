#' Returns points of interest in the specified radius
#'
#' @param lng Longitude
#' @param lat Latitude
#' @param radius Radius
#'
#' @get /POIs
function(lng, lat, radius){
  data <- plmbr:::getPOIsInRadius(lng, lat, radius)
  data
}

#' Returns radius polygon
#'
#' @param lng Longitude
#' @param lat Latitude
#' @param radius Radius
#'
#' @get /radius
function(lng, lat, radius){
  data <- plmbr:::getRadiusPolygon(lng, lat, radius)
  data
}

#' Add people to database
#'
#' @post /people
function(req){
  # this will be probably edite - depends on request body JSON characters escaping
  body <- jsonlite::fromJSON(jsonlite::fromJSON(req$postBody))
  plmbr:::addPeople(body)
}
