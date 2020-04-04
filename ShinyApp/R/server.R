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

    map <- .genBaseMap();
    data <- getAreaData() %>% addColor()
    dataSubseted <- NULL

    observeEvent(input$tableOutput_rows_selected, {}) # for some reason I have to observe this event because the reactivity break down after location is allowed

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
    })
  }
  server
}
# Run the application
