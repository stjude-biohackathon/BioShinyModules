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

#### Function needed to work #### ----------
#' Differentially methylated region (DMR) plot
#'
#' @param df A data frame of B-values from DNA methylation array (eg. Illumina EPIC 850k); must be in ascending order by genomic position
#' @return plot
#' @export pdf

plotScatter <- function(df, colx, coly, dotcol, title){
  req(is.numeric(df[,colx]))
  req(is.numeric(df[,coly]))
  print(c(colx, coly))
  ggplot(df, aes(x=.data[[colx]],y=.data[[coly]])) + #Crashed R on my side
    geom_point(col=dotcol) + 
    ggtitle(title) + xlab(colx) + ylab(coly)
}


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
    Scatter_plot <- reactive({
      plotScatter(df(),input$x_columns_sel, input$y_columns_sel, input$dotcol, input$title)
    })
    output$scatter <- renderPlotly({
      Scatter_plot()
    })
    ns <- NS(id)
    output$x_columns <- renderUI({
      selectInput(ns("x_columns_sel"), "Select column to plot on x axis", choices=colnames(df()))
    })
    output$y_columns <- renderUI({
      selectInput(ns("y_columns_sel"), "Select column to plot on y axis", choices=colnames(df()))
    })
    
    return(Scatter_plot)
    
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
