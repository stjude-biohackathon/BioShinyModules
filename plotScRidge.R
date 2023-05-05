# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: @KMcC73
# author: Louis Le Nézet (louislenezet@gmail.fr)

# Documentation
#' R Shiny module to generate a violin plot per gene
#'
#' @param id A string.
#' @param df_expr Dataframe with the expression information.
#' @param df_meta Dataframe with the meta data information per sample.
#' @returns A Shiny module.
#' @examples
#' scRidge_demo()
#'
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
scRidge_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("geneSelect"), "Type Gene:", value = "CHCHD2"),
    numericInput(ns("parameter"), "Select Resolution", value = 0.5),
    plotOutput(ns("scRidgePlot"))
  )
}

#### Server function of the module #### ----------
scRidge_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    scRidge_plot <- reactive({
      # Processing data
      df_processed <- process_df(df(), resolution = input$parameter)

      # RidgePlot
      RidgePlot(df_processed, features = toupper(input$geneSelect), ncol = 3, cols = ggsci::pal_igv()(50))
    })
    output$scRidgePlot <- renderPlot({
      scRidge_plot()
    })
    return(scRidge_plot)
  })
}

#### Demo function of the module #### ----------
scRidge_demo <- function() {
  df <- readRDS("./example_data/pbmc3k.rds")
  ui <- fluidPage(
    scRidge_ui("scRidgePlot")
  )
  server <- function(input, output, session) {
    scRidge_server("scRidgePlot", reactive({
      df
    }))
  }
  shinyApp(ui, server)
}
