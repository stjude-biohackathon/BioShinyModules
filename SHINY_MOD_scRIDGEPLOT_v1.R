library(Seurat)
library(shiny)

#file1
pbmc3k <- readRDS("pbmc3k.rds")

scRidgePlot_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(NS(id,"geneSelect"),"Type Gene:", value="CHCHD2"),
    #numericInput(NS(id,"parameter"), "Select Resolution", value = 0.5),
    plotOutput(NS(id,"scRidgePlot"))
  )
}

scRidgePlot_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$scRidgePlot <- renderPlot({
      
      #Processing data
      pbmc3k<- NormalizeData(pbmc3k)
      pbmc3k <- FindVariableFeatures(pbmc3k, selection.method = "vst", nfeatures = 3000)
      pbmc3k<- ScaleData(pbmc3k, verbose = FALSE)
      pbmc3k <- RunPCA(pbmc3k, npcs = 15, verbose = FALSE)
      pbmc3k <- RunUMAP(pbmc3k, reduction = "pca", dims = 1:15)
      pbmc3k <- FindNeighbors(pbmc3k , reduction = "pca", dims = 1:15)
      pbmc3k <- RunTSNE(pbmc3k)
      pbmc3k <- FindClusters(pbmc3k , resolution = 0.5)
      GeneSel <- toupper(input$geneSelect)
      
      #RidgePlot
      RidgePlot(pbmc3k , features = 'CHCHD2', ncol = 3, cols = ggsci::pal_igv()(50))
      
    })
  })
}

scRidgePlotApp <- function() {
  ui <- fluidPage(
    scRidgePlot_module_ui("scRidgePlot")
  )
  server <- function(input, output, session) {
    scRidgePlot_module_server("scRidgePlot")
  }
  shinyApp(ui, server)  
}

scRidgePlotApp()
