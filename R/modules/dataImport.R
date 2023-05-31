# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to import data files
#'
#' @param id A string.
#' @returns A Shiny module.
#' @examples
#' dataImport_demo()
#### Library needed #### ----------
library(shiny)
library(tidyr)
library(tibble)
library(readxl)
library(shinyWidgets)

#### UI function of the module #### ----------

dataImport_ui <- function(id, label = "Select data file") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    checkboxInput(ns("to_char"), "Load all data as strings"),
    checkboxInput(ns("stringsAsFactors"), "Strings as factors"),
    pickerInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'",
      "Both"="\"'"
    )),
    selectInput(ns("sep"), "Separator", c(
      "Comma" = ",",
      "Semi-colon" = ";",
      "Tabulation" = "\t",
      "Space" = " "
    )),
    uiOutput(ns("dfSelection"))
  )
}


#### Server function of the module #### ----------

dataImport_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # The selected file, if any
    user_file <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$file, message = FALSE))
      input$file
    })

    ## Data selection ------------------------
    df <- reactive({
      file <- user_file()$datapath
      if (!is.null(file)) {
        req(file)
        ext <- tools::file_ext(file)
        validate(need(ext %in% c("csv", "txt", "xls", "xlsx", "rda"),
          "Please upload a (csv, txt, xls, xlsx, rda) file"))
        if (input$to_char) {
          col_classes <- "character"
          col_types <- "text"
        } else {
          col_classes <- NA
          col_types <- NULL
        }
        if (ext %in% c("csv", "txt")) {
          df <- read.csv(file,
            sep = input$sep,
            quote = input$quote, header = input$heading,
            col_classes = col_classes
          )
        } else if (ext %in% c("xls", "xlsx")) {
          sheets_present <- excel_sheets(file)
          req(input$dfSelected)
          if (input$dfSelected %in% sheets_present) {
            df <- as.data.frame(readxl::read_excel(file,
              sheet = input$sheetSelected, col_names = input$heading,
              col_types = col_types
            ))
          } else {
            print("Error: Sheet selected isn't in file")
            df <- NULL
          }
        } else if (ext == "rda") {
          all_data <- load(file)
          req(input$dfSelected)
          if (input$dfSelected %in% all_data) {
            df <- get(input$dfSelected)
          }else {
            print("Error: Sheet selected isn't in file")
            df <- NULL
          }
        }
        df <- as.data.frame(unclass(df),
          stringsAsFactors = input$stringsAsFactors)
      } else {
        print("Error: data selected is null")
        df <- NULL
      }
    })

    # We can run observers in here if we want to
    observe({
      msg <- sprintf("File %s was uploaded", user_file()$name)
      cat(msg, "\n")
    })

    ns <- NS(id)

    output$dfSelection <- renderUI({
      file <- user_file()$datapath
      req(file)
      ext <- tools::file_ext(file)
      if (ext %in% c("xls", "xlsx")) {
        sheets_present <- excel_sheets(file)
        if (!is.null(sheets_present)) {
          selectInput(ns("dfSelected"),
            label = "Select dataframe to use",
            choices = sheets_present, selected = sheets_present[1]
          )
        } else {
          message("No sheets find in file")
          NULL
        }
      } else if (ext == "rda") {
        all_data <- load(file)
        selectInput(ns("dfSelected"),
                    label = "Select dataframe to use",
                    choices = all_data
        )
      } else {
        message("File not an xls, xlsx nor rda")
        NULL
      }
    })

    # Return the reactive that yields the data frame
    return(df)
  })
}


#### Demo function of the module #### ----------

dataImport_demo <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        dataImport_ui("datafile", "User data (csv, xls, xlsx format)")
      ),
      mainPanel(
        dataTableOutput("table")
      )
    )
  )

  server <- function(input, output, session) {
    datafile <- dataImport_server("datafile")

    output$table <- renderDataTable({
      datafile()
    })
  }

  shinyApp(ui, server)
}

dataImport_demo()