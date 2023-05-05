# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Lawryn Kasper (lawryn.kasper@stjude.org)
# author: Louis Le Nézet (louislenezet@gmail.fr)

# Documentation
#' R Shiny module to generate a scatterplot with hover text
#' 
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' plotScatter_demo()

#### Library needed #### ----------
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(colourpicker)

#### UI function of the module #### ----------
plotScatter_ui <- function(id, df) {
  ns <- NS(id)
  tagList(
    colourInput(ns("dotcol"),"Pick point color", value = "black"),
    uiOutput(ns("x_columns")),
    uiOutput(ns("y_columns")),
    textInput(ns("title"), "Enter title text", ""),
    plotlyOutput(ns("scatter"))
)
}

#### Server function of the module #### ----------
plotScatter_server <- function(id, df) {
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    plot <- reactive({
      req(input$y_columns_sel)
      req(input$x_columns_sel)
      print(input$y_columns_sel)
      print(input$x_columns_sel)
      ggplot(df())+ 
        geom_point(col=input$dotcol) + 
        ggtitle(input$title) + xlab(input$x_columns_sel) + ylab(input$y_columns_sel)
    })
    output$scatter <- renderPlotly({
      plot()
    })
    ns <- NS(id)
    output$x_columns <- renderUI({
      selectInput(ns("x_columns_sel"), "Select column to plot on x axis", choices=colnames(df()))
    })
    output$y_columns <- renderUI({
      selectInput(ns("y_columns_sel"), "Select column to plot on y axis", choices=colnames(df()))
    })
    
    return(plot)
    
  })
}

#### Demo function of the module #### ----------
plotScatter_demo <- function() {
  df <- read.delim("./example_data/L29_vitro_Control_vs_knockdown_diff.txt") %>%
    mutate(log_pval = -log10(P.Value)) %>%
    mutate(log_adjpval = -log10(adj.P.Val))
  
  ui <- fluidPage(
    plotScatter_ui("scatter",df)
  )
  server <- function(input, output, session) {
    plotScatter_server("scatter",reactive({
      df
    }))
  }
  shinyApp(ui, server)  
}

plotScatter_demo()
