getArea <- function(areaName) {
  10
}

getNumberOfPeopleInArea <- function(areaName) {
  200
}

getAreas <- function() {
  data.frame(
    pointOfInterest = paste(c("area", "location", "position"), 1:15),
    numberOfPeople = seq(11, 1000, 66),
    area = c(50,100,150)
  )
}

getAreaData <- function() {
  csvFile <- read.csv("dump.csv",encoding = "UTF-8")
  csvFile$area <- c(50,100,150,1)
  csvFile$numberOfPeople <- seq(1, 250, 75)

  csvFile
}
