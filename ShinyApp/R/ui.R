#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram

#' Title
#'
#' @return
#'
#' @import shiny
#' @import shinydashboard
generateUI <- function() {
    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "coRona finder"),
        shinydashboard::dashboardSidebar(),
        body = shinydashboard::dashboardBody(
            shinypop::use_push(),
            shiny::tags$head(shiny::tags$script(shiny::HTML(getGeolocation())),
                             shiny::tags$style(shiny::HTML(getDTStyles()))),


            fluidRow(shinydashboard::valueBoxOutput("place"),
                     shinydashboard::valueBoxOutput("numberOfPeople"),
                     shinydashboard::valueBoxOutput("area")
                    ),
            fluidRow(leaflet::leafletOutput("map")),
            fluidRow(DT::dataTableOutput("tableOutput"))
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

