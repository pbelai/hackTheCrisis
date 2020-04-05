getArea <- function(areaName) {
  10
}

getNumberOfPeopleInArea <- function(areaName) {
  200
}

getCurrentLocation <- function(lat, lng) {
  data.frame(numberOfPeople = 720, area = 1000, name = "Meno")
  data.frame(numberOfPeople = 2000, area = 1000, name = "Meno")
  data.frame(numberOfPeople = 1, area = 1000, name = "Meno")
}


getAreaData <- function() {
  csvFile <- read.csv("dump.csv",encoding = "UTF-8")
  csvFile$area <- c(50,100,150,1)
  csvFile$numberOfPeople <- seq(1, 250, 75)

  csvFile
}

getAreaDataFromAPI <- function(lng, lat, radius) {
  urlBase <- "http://127.0.0.1:8000/POIs"
  url <- glue::glue("{urlBase}?lng={lng}&lat={lat}&radius={radius}")
  response <- httr::GET(url = url)
  data <- jsonlite::fromJSON(httr::content(response, "text")) %>% dplyr::rename(area = size)
  data
}

