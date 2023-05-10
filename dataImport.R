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
    userFile <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$file, message = FALSE))
      input$file
    })

    ## Data selection ------------------------
    df <- reactive({
      file <- userFile()$datapath
      if (!is.null(file)) {
        req(file)
        ext <- tools::file_ext(file)
        validate(need(ext %in% c("csv", "txt", "xls", "xlsx", "rda"), "Please upload a (csv, txt, xls, xlsx, rda) file"))
        if (input$to_char) {
          colClasses <- "character"
          col_types <- "text"
        } else {
          colClasses <- NA
          col_types <- NULL
        }
        if (ext %in% c("csv","txt")) {
          df <- read.csv(file,
            sep = input$sep,
            quote = input$quote, header = input$heading,
            colClasses = colClasses
          )
          ?read.csv
        } else if (ext %in% c("xls", "xlsx")) {
          sheetsPresent <- excel_sheets(file)
          req(input$dfSelected)
          if (input$dfSelected %in% sheetsPresent) {
            df <- as.data.frame(readxl::read_excel(file,
              sheet = input$sheetSelected, col_names = input$heading,
              col_types = col_types
            ))
          } else {
            print("Error: Sheet selected isn't in file")
            df <- NULL
          }
        } else if (ext == "rda") {
          allData <- load(file)
          req(input$dfSelected)
          if (input$dfSelected %in% allData){
            df <- get(input$dfSelected)
          }else {
            print("Error: Sheet selected isn't in file")
            df <- NULL
          }
        }
        df <- as.data.frame(unclass(df), stringsAsFactors = input$stringsAsFactors)
      } else {
        print("Error: data selected is null")
        df <- NULL
      }
    })

    # We can run observers in here if we want to
    observe({
      msg <- sprintf("File %s was uploaded", userFile()$name)
      cat(msg, "\n")
    })

    ns <- NS(id)
    
    output$dfSelection <- renderUI({
      file <- userFile()$datapath
      req(file)
      ext <- tools::file_ext(file)
      if (ext %in% c("xls", "xlsx")) {
        sheetsPresent <- excel_sheets(file)
        if (!is.null(sheetsPresent)) {
          selectInput(ns("dfSelected"),
            label = "Select dataframe to use",
            choices = sheetsPresent, selected = sheetsPresent[1]
          )
        } else {
          message("No sheets find in file")
          NULL
        }
      } else if (ext == "rda"){
        allData <- load(file)
        selectInput(ns("dfSelected"),
                    label = "Select dataframe to use",
                    choices = allData
        )
      }else{
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