# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Louis El Khoury (louis.elkhoury@stjude.org)
# author: @KMcC73 (kelly.mccastlain@stjude.org)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate XXX graph
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' scTSNE_demo()

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
scTSNE_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("parameter"), "Select Resolution", value = 0.5),
    plotOutput(ns("scTSNE"))
  )
}

#### Server function of the module #### ----------
scTSNE_server <- function(id, df) {
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    scTSNE_plot <- reactive({
      # Processing data
      df_processed <- process_df(df(), input$parameter)
      #TSNE
      DimPlot(df_processed, reduction = 'tsne', cols=ggsci::pal_igv()(50))
    })
    output$scTSNE <- renderPlot({
      scTSNE_plot()
    })
    return(scTSNE_plot)
  })
}

#### Demo function of the module #### ----------
scTSNE_demo <- function() {
  df <- readRDS("./example_data/pbmc3k.rds")
  ui <- fluidPage(
    scTSNE_ui("TSNE")
  )
  server <- function(input, output, session) {
    scTSNE_server("TSNE",reactive({
      df
    }))
  }
  shinyApp(ui, server)  
}

scTSNE_demo()

#TODO add documentation