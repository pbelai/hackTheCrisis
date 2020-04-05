#' Returns points of interest in the specified radius
#'
#' @param lng Longitude
#' @param lat Latitude
#' @param radius Radius
getPOIsInRadius <- function(lng = 48.1446364, lat = 17.1103739, radius = 300) {
  # for now, lng, lat and radius overriden by dummy values
  queryMainSelect <- "SELECT osm_id, amenity, name, ST_AsGeoJson(geoway) as geojson, ST_Area(geoway) AS size FROM"
  queryPoints <- paste("SELECT osm_id, amenity, name, ST_Buffer(p.way::geography, 5) as geoway FROM planet_osm_point p",
    "WHERE ST_DWithin(p.way::geography, ST_SetSRID(ST_Point($2,$1),4326)::geography, $3)",
    "AND p.name IS NOT null")
  queryPolygons <- paste("SELECT osm_id, amenity, name, way as geoway FROM planet_osm_polygon p",
    "WHERE ST_DWithin(p.way::geography, ST_SetSRID(ST_Point($2,$1),4326)::geography, $3)",
    "AND p.name IS NOT null")
  queryAliases <- "as p"#, people pp"
  #queryConditions <- "WHERE ST_Within(ST_SetSRID(pp.way, 4326), ST_SetSRID(p.geoway::geometry, 4326))"
  #queryGroupBy <- "GROUP BY osm_id, amenity, name, geoway"

  POIsQuery <- paste(queryMainSelect, "(", queryPoints, "UNION", queryPolygons, ")", queryAliases)#, queryConditions, queryGroupBy)

  row <- c(lng = lng, lat = lat, radius = radius)

  queryResult <- dbGetQuery(connection, POIsQuery, row)
  queryResult
}

#' Get area by gived corrdinates
#'
#' @param min
#' @param max
#' @param amount
randomNumberInteger <- function(min, max, amount) {
  sample(min:max, amount)
}

#' Returns radius polygon
#'
#' @param lng Longitude
#' @param lat Latitude
#' @param radius Radius
getRadiusPolygon <- function(lng = 48.1446364, lat = 17.1103739, radius = 300) {
  # for now, lng, lat and radius overriden by dummy values
  radiusPolygonQuery <- "SELECT ST_AsGeoJson(ST_Buffer(ST_SetSRID(ST_Point($2,$1),
                         4326)::geography, $3)) as geojson;"

  row <- c(lng = lng, lat = lat, radius = radius)
  queryResult <- dbGetQuery(connection, radiusPolygonQuery, row)
  queryResult
}

#' Inserts people coordinates and traffic to database
#'
#' @param postBodyJson Request body with data to insert
#'
#' @examples
addPeople <- function(body) {
  message("Inserting people to database")
  sqlInsertStatement = paste("INSERT INTO \"people\"",
                        "(\"traffic\", \"live_traffic\", \"way\")",
                        "VALUES",
                        "($1, $2, ST_GeomFromGeoJSON($3))")
  apply(body, 1, function(x){
    traffic <- x[["traffic"]]
    liveTraffic <- x[["liveTraffic"]]
    way <- paste0(jsonlite::toJSON(list(type = "Point", coordinates = list(x[["lat"]], x[["long"]])),
                                           auto_unbox = T))
    row <- c(traffic = traffic, liveTraffic = liveTraffic, way = way)
    dbExecute(connection, sqlInsertStatement, row)
  })
  message("Inserting finished")
}
