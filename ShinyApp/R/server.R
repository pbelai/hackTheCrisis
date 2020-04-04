# Define server logic required to draw a histogram
generateServer <- function() {
  server <- function(input, output, session) {
    map <- createLeaflet()
    output$map <- leaflet::renderLeaflet(map)
    output$tableOutput <- generatePoIDataTable(getAreas())
    output$lat <- shiny::renderPrint({
      input$lat
    })

    output$long <- shiny::renderPrint({
      input$long
    })

    output$geolocation <- shiny::renderPrint({
      input$geolocation
    })
    observeEvent(input$locationChange, {
      shinypop::push(title = "I can see you", "aaa")
      output$map <- leaflet::renderLeaflet(map %>% leaflet::addMarkers(input$locationChange[2], input$locationChange[1]))
    })
  }
  server
}
# Run the application
