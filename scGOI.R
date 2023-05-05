# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: @KMcC73
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate XXX graph
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' scGOI_demo()

#### Library needed #### ----------
library(Seurat)
library(shiny)

#### Function needed to work #### ----------
# Documentation
#' Function that process the data
#'
#' @param df A dataframe.
#' @returns A process data.

process_df <- function(df, resolution = 0.5) {
  df <- NormalizeData(df)
  df <- FindVariableFeatures(df, selection.method = "vst", nfeatures = 3000)
  df <- ScaleData(df, verbose = FALSE)
  df <- RunPCA(df, npcs = 15, verbose = FALSE)
  df <- RunUMAP(df, reduction = "pca", dims = 1:15)
  df <- FindNeighbors(df, reduction = "pca", dims = 1:15)
  df <- RunTSNE(df)
  df <- FindClusters(df, resolution = resolution)
  return(df)
}

#### UI function of the module #### ----------
scGOI_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("geneSelect"), "Type Gene:", value = "CHCHD2"),
    numericInput(ns("parameter"), "Select Resolution", value = 0.5),
    plotOutput(ns("GOIcluster"))
  )
}

#### Server function of the module #### ----------
scGOI_server <- function(id, df) {
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    plot <- reactive({
      # Processing data
      df_processed <- process_df(df(), input$parameter)
      
      GeneSel <- toupper(input$geneSelect)
      
      # Feature plot
      FeaturePlot(df_processed, features = GeneSel, cols = c("grey", "red"), order = T)
    })
    output$GOIcluster <- renderPlot({
      plot()
    })
    return(plot)
  })
}

#### Demo function of the module #### ----------
scGOI_demo <- function() {
  df <- readRDS("./example_data/pbmc3k.rds")
  ui <- fluidPage(
    scGOI_ui("GOIcluster")
  )
  server <- function(input, output, session) {
    scGOI_server("GOIcluster", reactive({
      df
    }))
  }
  shinyApp(ui, server)
}

# TODO add documentation