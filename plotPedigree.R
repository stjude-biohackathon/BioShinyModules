# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate pedigree graph
#'
#' @param id A string.
#' @param df A dataframe containing the information about the individual to plot
#' @returns A Shiny module.
#' @examples
#' Pedigree_demo()
#### Library needed #### ----------
library(kinship2)
library(shiny)

#### UI function of the module #### ----------
Pedigree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    uiOutput(ns("familySelection"))
  )
}

#### Server function of the module #### ----------
Pedigree_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)


    df_ped <- reactive({
      cbind(
        df(),
        family = families()
      )
    })

    families <- reactive({
      makefamid(df()$id, df()$dadid, df()$momid)
    })

    output$familySelection <- renderUI({
      famid <- sort(unique(families()))
      selectInput(ns("familySelected"),
        label = "Select family to plot",
        choices = famid, selected = "1"
      )
    })

    Pedigree_plot <- reactive({
      req(input$familySelected)
      req(df_ped)
      df_pedF <- df_ped()[df_ped()$family == input$familySelected, ]
      print(df_pedF)
      ped <- with(df_pedF, pedigree(id, dadid, momid, sex, affected))
      plot.pedigree(ped)
    })

    output$plot <- renderPlot({
      Pedigree_plot()
    })
    return(Pedigree_plot)
  })
}

#### Demo function of the module #### ----------
Pedigree_demo <- function() {
  df <- read.table("./example_data/TestPedigree.csv", header = TRUE, sep = ";") # example data
  ui <- fluidPage(Pedigree_ui("x"))
  server <- function(input, output, session) {
    Pedigree_server("x", reactive({
      df
    }))
  }
  shinyApp(ui, server)
}
