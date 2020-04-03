# Define server logic required to draw a histogram
generateServer <- function() {
  server <- function(input, output, session) {
    output$map <- leaflet::renderLeaflet(createLeaflet())
    output$tableOutput <- generatePoIDataTable(getAreas())
  }
  server
}
# Run the application
