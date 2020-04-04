#' Connects to Postgres database
#'
#' @return Database connection object to perform DB actions with
connectToPostgresDB <- function() {
  connection <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(), dbname = "gis-assignment",
                   host = "localhost", port = 5432,
                   user = "postgres", password = "postgres")
  connection
}


#' Disconnects from postgres database
#'
#' @param connection Postgres connection object
disconnectFromPostgresDB <- function(connection) {
  message("Disconnecting from Postgres database")
  RPostgreSQL::dbDisconnect(connection)
}
