#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram

generateUI <- function() {
    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "coRona finder"),
        shinydashboard::dashboardSidebar(),
        body = shinydashboard::dashboardBody(
            shinypop::use_push(),
            tags$head(tags$script(HTML(getGeolocation()))),

            fluidRow(getInformationBoxes()),
            fluidRow(leaflet::leafletOutput("map")),
            fluidRow(DT::dataTableOutput("tableOutput")),
            # shiny::tableOutput("mtcars"),
            shiny::verbatimTextOutput("lat"),
            shiny::verbatimTextOutput("long"),
            shiny::verbatimTextOutput("geolocation")
        )
    )
    ui
}
#' Starts shiny server
#'
#' @return
#' @export
startServer <- function() {
    shiny::shinyApp(ui = generateUI(), server = generateServer())
}

