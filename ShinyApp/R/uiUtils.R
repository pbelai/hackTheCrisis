getInformationBoxes <- function() {
  area <- getArea()
  numberOfPeople <- getNumberOfPeopleInArea()
  color <- getColor(numberOfPeople, area)


  list(
    shinydashboard::valueBox(
      "Miesto",
      "Miesto",
      color = color,
      icon = shiny::icon("map-marker-alt")
    ),
    shinydashboard::valueBox(
      numberOfPeople,
      "Počet ľudí",
      color = color,
      icon = shiny::icon(getPeopleIcon(numberOfPeople, area))
    ),
    shinydashboard::valueBox(
      area,
      "Plocha (m²)",
      color = color,
      icon = shiny::icon("map")
    )
  )
}


generatePoIDataTable <- function(data) {
  data <- data %>% dplyr::rename(
    Miesto = pointOfInterest, "Počet ľudí" = numberOfPeople, Plocha = area
  )
  DT::renderDataTable(data, server = FALSE)
}


getColor <- function(numberOfPeople, areaM2 = 10) {
  concentration <- numberOfPeople / areaM2
  if (concentration < 0.5) {
    "green"
  } else if (concentration < 2) {
    "yellow"
  } else {
    "red"
  }
}

getPeopleIcon <- function(numberOfPeople, areaM2 = 10) {
  concentration <- numberOfPeople / areaM2
  if (concentration < 0.5) {
    "smile"
  } else if (concentration < 2) {
    "meh"
  } else {
    "frown"
  }
}
