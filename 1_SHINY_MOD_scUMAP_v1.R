library(Seurat)
library(shiny)

#file1
pbmc3k <- readRDS("C:/Users/kmccastl/Downloads/pbmc3k.rds")

scUMAP_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(NS(id,"parameter"), "Select Resolution", value = 0.5),
    plotOutput(NS(id,"UMAP"))
  )
}

scUMAP_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$UMAP <- renderPlot({
      #Processing data
      pbmc3k<- NormalizeData(pbmc3k)
      pbmc3k <- FindVariableFeatures(pbmc3k, selection.method = "vst", nfeatures = 3000)
      pbmc3k<- ScaleData(pbmc3k, verbose = FALSE)
      pbmc3k <- RunPCA(pbmc3k, npcs = 15, verbose = FALSE)
      pbmc3k <- RunUMAP(pbmc3k, reduction = "pca", dims = 1:15)
      pbmc3k <- FindNeighbors(pbmc3k , reduction = "pca", dims = 1:15)
      pbmc3k <- RunTSNE(pbmc3k)
      pbmc3k <- FindClusters(pbmc3k , resolution = input$parameter)
      GeneSel <- toupper(input$geneSelect)
      
      # UMAP
      DimPlot(pbmc3k , reduction = 'umap', cols=ggsci::pal_igv()(50))
      
    })
  })
}

UMAPApp <- function() {
  ui <- fluidPage(
    scUMAP_module_ui("UMAP")
  )
  server <- function(input, output, session) {
    scUMAP_module_server("UMAP")
  }
  shinyApp(ui, server)  
}

UMAPApp()