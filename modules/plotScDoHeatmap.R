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
#' plotGeneViolin_demo()
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
scDoHeatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # textInput(ns("geneSelect"), "Type Gene:", value = "CHCHD2"), # Need to be implemented
    numericInput(ns("parameter"), "Select Resolution", value = 0.5),
    plotOutput(ns("DoHeatmap"))
  )
}

#### Server function of the module #### ----------
scDoHeatmap_server <- function(id, df) {
  stopifnot(is.reactive(df))
  moduleServer(id, function(input, output, session) {
    scDoHeatmap_plot <- reactive({
      # Processing data
      df_processed <- process_df(df(), resolution=input$parameter)
      
      print(VariableFeatures(df_processed))
      
      # Heatmap
      DoHeatmap(df_processed,
        features = VariableFeatures(df_processed)[1:10], cells = 1:20, size = 4, # Features not personnalized
        angle = 90
      ) + NoLegend()
    })
    output$DoHeatmap <- renderPlot({
      scDoHeatmap_plot()
    })
    return(scDoHeatmap_plot)
  })
}

#TODO make the genes selection possible

#### Demo function of the module #### ----------
scDoHeatmap_demo <- function() {
  df <- readRDS("../example_data/pbmc3k.rds")
  ui <- fluidPage(
    scDoHeatmap_ui("DoHeatmap")
  )
  server <- function(input, output, session) {
    scDoHeatmap_server("DoHeatmap", reactive({
      df
    }))
  }
  shinyApp(ui, server)
}
