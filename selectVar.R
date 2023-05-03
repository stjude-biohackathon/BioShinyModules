# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)

# Documentation
#' R Shiny module to select a variable fits a criterion
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(shiny)

#### Function needed to work #### ----------

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
                        updateSelectInput(session, "var", choices = find_vars(data(), filter))
                })

                reactive(data()[[input$var]])
        })
}

#### Demo function of the module #### ----------
source("csvImport.R")
selectVar_demo <- function(filter = is.factor) {
        ui <- fluidPage(
                csvImport_ui("datafile", "User data (.csv format)"),
                selectVar_ui("var", "Choose a column"),
                verbatimTextOutput("out")
        )
        server <- function(input, output, session) {
                data <- csvImport_server("datafile", stringsAsFactors = TRUE)
                var <- selectVar_server("var", data, filter = filter)
                output$out <- renderPrint(var())
        }

        shinyApp(ui, server)
}


# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation

