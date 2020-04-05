# Define server logic required to draw a histogram
generateServer <- function() {
  server <- function(input, output, session) {
    .genBaseMap <- function() {
      shiny::isolate( # make this non reactive so it does not redraw map when not needed
        if (!is.null(input$map_zoom)) {
          createLeaflet(lng = input$map_center$lng, lat = input$map_center$lat, input$map_zoom)
        } else {
          createLeaflet()
      })
    }

    values <- reactiveValues()
    values$location <- NULL
    values$currentLocation <- NULL

    map <- .genBaseMap()
    data <- getAreaData()
    #data <- getAreaDataFromAPI(48.1446364, 17.1103739, 300) %>% addColor()
    dataSubseted <- NULL

    observeEvent(input$tableOutput_rows_selected, {}) # for some reason I have to observe this event because the reactivity break down after location is allowed


    output$place <- shinydashboard::renderValueBox({
      location <- if (!is.null(values$currentLocation)) values$currentLocation$name else "N/A"
      color <- if (!is.null(values$currentLocation)) values$currentLocation$color else "black"
      shinydashboard::valueBox(
        location,
        "Lokácia",
        color = color,
        icon = shiny::icon("map-marker-alt")
      )
    })
    output$numberOfPeople <- shinydashboard::renderValueBox({
      numberOfPeople <- if (!is.null(values$currentLocation)) values$currentLocation$numberOfPeople else "N/A"
      color <- if (!is.null(values$currentLocation)) values$currentLocation$color else "black"
      icon <- if (!is.null(values$currentLocation)) getPeopleIcon(values$currentLocation$numberOfPeople, values$currentLocation$area) else "meh"

      shinydashboard::valueBox(
        numberOfPeople,
        "Počet ľudí",
        color = color,
        icon = shiny::icon(icon)
      )
    })
    output$area <- shinydashboard::renderValueBox({
      area <- if (!is.null(values$currentLocation)) values$currentLocation$area else "N/A"
      color <- if (!is.null(values$currentLocation)) values$currentLocation$color else "black"
      shinydashboard::valueBox(
        area,
        "Plocha (m²)",
        color = color,
        icon = shiny::icon("map")
      )
    })

    output$map <- leaflet::renderLeaflet({
      rows <- input$tableOutput_rows_selected
      dataSubseted <<- if (length(rows)) data[rows,] else data
      map <<- .genBaseMap()# %>% addPolygons(dataSubseted)
      redrawMap(map, dataSubseted, values$location)
      })
    output$tableOutput <- generatePoIDataTable(data)

    observeEvent(input$locationChange, {
      shinypop::push(title = "I can see you", "aaa")
      values$location <<- list(lng = input$locationChange[2], lat = input$locationChange[1])
      location <- getCurrentLocation(values$location$lat, values$location$lng)
      location <- addColor(location)
      values$currentLocation <<- location
    })
  }
  server
}
# Run the application
