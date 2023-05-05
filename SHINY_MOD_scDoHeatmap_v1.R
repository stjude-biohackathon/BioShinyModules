library(Seurat)
library(shiny)


#file1
pbmc3k <- readRDS("pbmc3k.rds")

scDoHeatmap_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(NS(id,"geneSelect"),"Type Gene:", value="CHCHD2"),
    numericInput(NS(id,"parameter"), "Select Resolution", value = 0.5),
    plotOutput(NS(id,"DoHeatmap"))
  )
}

scDoHeatmap_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$DoHeatmap <- renderPlot({
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
      #DimPlot(pbmc3k , reduction = 'umap', cols=ggsci::pal_igv()(50))
      
      #Heatmap
      DoHeatmap(pbmc3k , features = VariableFeatures(pbmc3k)[1:10], cells = 1:20, size = 4,
                angle = 90) + NoLegend()
    })
  })
}

DoHeatmapApp <- function() {
  ui <- fluidPage(
    scDoHeatmap_module_ui("DoHeatmap")
  )
  server <- function(input, output, session) {
    scDoHeatmap_module_server("DoHeatmap")
  }
  shinyApp(ui, server)  
}

DoHeatmapApp()

