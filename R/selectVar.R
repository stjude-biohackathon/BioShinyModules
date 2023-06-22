# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Louis Le Nézet (louislenezet@gmail.com)


#### Function needed to work #### ----------
#' Function to select a variable fitting a criterion in a dataframe
#'
#' @param df A dataframe.
#' @param filter A function to use as a filter (e.g is.numeric).
#' @returns the column fitting the filter.
find_vars <- function(df, filter) {
  stopifnot(is.data.frame(df))
  stopifnot(is.function(filter))
  names(df)[vapply(df, filter, logical(1))]
}

#### UI function of the module #### ----------
#' R Shiny module to select a variable that fits a criterion
#'
#' @param id A string.
#' @param label Title to prompt to the user.
#' @returns A Shiny UI.
#' @examples
#' \dontrun{
#' selectVar_demo()
#' }
#' @export selectVar_ui
selectVar_ui <- function(id, label = "Choose a column") {
  shiny::selectInput(shiny::NS(id, "var"), label, choices = NULL)
}

#### Server function of the module #### ----------
#' R Shiny module to select a variable that fits a criterion
#'
#' @param id A string.
#' @param df A reactive dataframe.
#' @param filter A function to use as a filter (default is.numeric).
#' @returns A Shiny server.
#'
#' @details The choice are updated in the server with the find_vars
#' function.
#' @examples
#' \dontrun{
#' selectVar_demo()
#' }
#' @export selectVar_server
selectVar_server <- function(id, df, filter = is.numeric) {
  stopifnot(shiny::is.reactive(df))
  stopifnot(!shiny::is.reactive(filter))

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(df(), {
      shiny::updateSelectInput(session, "var",
        choices = find_vars(df(), filter))
    })

    shiny::reactive(df()[[input$var]])
  })
}

#### Demo function of the module #### ----------
#' R Shiny demo app to select a variable that fits a criterion
#'
#' @param filter A function to use as a filter (default is.numeric).
#' @returns A Shiny app.
#'
#' @examples
#' \dontrun{
#' selectVar_demo(filter = is.numeric)
#' }
#' @export selectVar_demo
selectVar_demo <- function(filter = is.numeric) {
  ui <- shiny::fluidPage(
    selectVar_ui("var", "Choose a column"),
    shiny::verbatimTextOutput("out")
  )
  server <- function(input, output, session) {
    var <- selectVar_server("var", cars, filter = filter)
    output$out <- shiny::renderPrint(var())
  }

  shiny::shinyApp(ui, server)
}
