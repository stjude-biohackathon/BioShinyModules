# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.

# Documentation
#' R Shiny module to import CSV files
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(shiny)

#### UI function of the module #### ----------

csvImport_ui <- function(id, label = "Select csv file") {

        tagList(
                fileInput(NS(id, "file"), label),
                # checkboxInput(ns("heading"), "Has heading"),
                # selectInput(ns("quote"), "Quote", c(
                #         "None" = "",
                #         "Double quote" = "\"",
                #         "Single quote" = "'"
                # ))
        )
}


#### Server function of the module #### ----------

csvImport_server <- function(id, stringsAsFactors) {

        moduleServer(id, function(input, output, session) {

                # The selected file, if any
                userFile <- reactive({
                        # If no file is selected, don't do anything
                        validate(need(input$file, message = FALSE))
                        input$file
                })

                # The user's data, parsed into a data frame
                dataframe <- reactive({
                        read.csv(userFile()$datapath,
                                 # header = input$heading,
                                 # quote = input$quote,
                                 stringsAsFactors = stringsAsFactors, row.names = 1)
                })

                # We can run observers in here if we want to
                observe({
                        msg <- sprintf("File %s was uploaded", userFile()$name)
                        cat(msg, "\n")
                })

                # Return the reactive that yields the data frame
                return(dataframe)
        })
}


#### Demo function of the module #### ----------

csvFile_demo <- function() {

        ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                csvImport_ui("datafile", "User data (.csv format)")
                        ),
                        mainPanel(
                                dataTableOutput("table")
                        )
                )
        )

        server <- function(input, output, session) {
                datafile <- csvImport_server("datafile", stringsAsFactors = FALSE)

                output$table <- renderDataTable({
                        datafile() %>%
                                rownames_to_column(., "rownames")
                })
        }

        shinyApp(ui, server)
}

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation


