library(Seurat)
library(shiny)

#file1
pbmc3k <- readRDS("C:/Users/kmccastl/Downloads/pbmc3k.rds")

scGOI_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(NS(id,"geneSelect"),"Type Gene:", value="CHCHD2"),
    numericInput(NS(id,"parameter"), "Select Resolution", value = 0.5),
    plotOutput(NS(id,"GOIcluster"))
  )
}

scGOI_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$GOIcluster <- renderPlot({
      
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
      
      #FeaturePlot(pbmc3k , features = 'CHCHD2', cols = c('grey', 'red'), order = T)
      FeaturePlot(pbmc3k , features = GeneSel, cols = c('grey', 'red'), order = T)
      
    })
  })
}

GOIApp <- function() {
  ui <- fluidPage(
    scGOI_module_ui("GOIcluster")
  )
  server <- function(input, output, session) {
    scGOI_module_server("GOIcluster")
  }
  shinyApp(ui, server)  
}

GOIApp()