#' Starts plumber API server
#'
#' @export
#'
#' @import magrittr
startServer <- function() {
  # DB connection set via plumber run
  connection <<- connectToPostgresDB()
  message("Starting plumber API server")
  pr <- plumber::plumb("R/plumberAPI.R")
  pr$registerHook("exit", function(){
    message("Stopping plumber API server")
    disconnectFromPostgresDB(connection)
  })
  pr$run(host = '127.0.0.1', port = 8000)
}
