# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to select a variable fits a criterion
#'
#' @param id A string.
#' @param df A dataframe.
#' @param filer filter to use.
#' @returns A Shiny module.
#' @examples
#' selectVar_demo()
#### Library needed #### ----------
library(shiny)

#### Function needed to work #### ----------
# Documentation
#' Function to select a variable fitting a criterion in a dataframe
#'
#' @param df A dataframe.
#' @param filer filter to use.
#' @returns the column fitting the filter.

find_vars <- function(data, filter) {
  stopifnot(is.data.frame(data))
  stopifnot(is.function(filter))
  names(data)[vapply(data, filter, logical(1))]
}

#### UI function of the module #### ----------

selectVar_ui <- function(id, label = "Choose a column") {
  selectInput(NS(id, "var"), label, choices = NULL)
}

#### Server function of the module #### ----------

selectVar_server <- function(id, data, filter = is.numeric) {
  stopifnot(is.reactive(data))
  stopifnot(!is.reactive(filter))

  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      print(find_vars(data(), filter))
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })

    reactive(data()[[input$var]])
  })
}

#### Demo function of the module #### ----------
selectVar_demo <- function(filter = is.character) {
  source("dataImport.R")
  ui <- fluidPage(
    dataImport_ui("datafile", "User data"),
    selectVar_ui("var", "Choose a column"),
    verbatimTextOutput("out")
  )
  server <- function(input, output, session) {
    data <- dataImport_server("datafile")
    var <- selectVar_server("var", data, filter = filter)
    output$out <- renderPrint(var())
  }

  shinyApp(ui, server)
}
