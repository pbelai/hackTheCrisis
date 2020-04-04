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

addPolygons <- function(map, data) {
  map %>%
    leaflet::addGeoJSON(createFeatureCollection(data$st_asgeojson[data$color == "green"]), fillColor = "green", color = "green") %>%
    leaflet::addGeoJSON(createFeatureCollection(data$st_asgeojson[data$color == "yellow"]), fillColor = "yellow", color = "yellow") %>%
    leaflet::addGeoJSON(createFeatureCollection(data$st_asgeojson[data$color == "red"]), fillColor = "red", color = "red")
}

generatePoIDataTable <- function(data) {
  .getDensity <- function(x){switch(x,
    "green" = "Nízka",
    "yellow" = "Priemerná",
    "red" = "Vysoká!"
  )}
  data$density <- sapply(data$color, .getDensity)
  data <- data %>% dplyr::select(
    Miesto = name, "Počet ľudí" = numberOfPeople, Plocha = area, "Hustota ľudí" = density
  )

  dataTable <-
    DT::datatable(
      data,
      style = "bootstrap",
      rownames = FALSE,
      autoHideNavigation = TRUE
    ) %>% DT::formatStyle(    'Hustota ľudí',
    target = 'row',
    backgroundColor = DT::styleEqual(c("Nízka", "Priemerná","Vysoká!"), c('#85ff85',"#fff66e", '#ff8585'))
  )

  DT::renderDataTable(dataTable, server = FALSE)
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


getDTStyles <- function() {
  "table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
    background-color: #151a1e69 !important;
    color: black;
    font-weight: bold;}"
}
