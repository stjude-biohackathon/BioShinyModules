# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# reworked for 3D plotly PCA: Lawryn Kasper (lawryn.kasper@stjude.org)

# Documentation
#' R Shiny module to generate 3D interactive PCA plot
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(shiny)
library(ggplot2)
library(pcaMethods)
library(plotly)
library(RColorBrewer)


#### Function needed to work #### ----------
#' plot 3D PCA
#'
#' @param data A data frame with feature (row) by sample (column).
#' @param sample_anno A data frame containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param title Character. Plot title
#'
#' @return
#' @export
#'
#' @examples


ggplotly_3Dpca <- function(df, sample_anno, sample_anno_col, pal, title) {
require(plotly)

  data <- as.matrix(df)
  class(data) <- "numeric"
  
  labels <- as.matrix(sample_anno[sample_anno_col])
  
  pc1 <- pcaMethods::pca(t(data), nPcs = 3, scale = "pareto")
  pc1merged <- merge(cbind(labels, t(data)),
                     pcaMethods::scores(pc1), by=0)

  plot_ly(pc1merged, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d",
            color = sample_anno$sampleLabel, colors = pal,
             text = sample_anno$sampleName, hoverinfo = "text") %>% 
  layout(title = title, scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  ))
}

palettes <- rownames(RColorBrewer::brewer.pal.info)


#### UI function of the module #### ----------
plot3DPCA_ui <- function(id) {
  
  fluidRow(
    column(2, selectInput(NS(id, "palette"), "Choose color palette", choices=palettes, selected = "Dark2")),
    column(3, textInput(NS(id,"title"), "Title graph", value = "")),
    column(12, plotlyOutput(NS(id, "plot")))
   # column( 1, downloadButton(NS(id, "dnld"), label = "")) Is there a version that can download an html?
  )
  
}

#### Server function of the module #### ----------
plot3DPCA_server <- function(id, df, sample_anno, sample_anno_col) {
  
  moduleServer(id, function(input, output, session) {
    
    stopifnot(is.reactive(df))
    stopifnot(is.reactive(sample_anno))
    stopifnot(is.reactive(sample_anno_col))
    
    plot <- reactive({
      ggplotly_3Dpca(df(), sample_anno(), sample_anno_col(), input$palette, input$title)
      
    })
    output$plot <- renderPlotly({plot()})
    # output$dnld <- downloadHandler(
    #   filename = function() {paste0("3D_pca", '.png')},
    #   content = function(file) {ggsave(file, plot())}
   #)
    
  })
}

#### Demo functionof the module #### ----------
load("./example_data/MS_2.rda")
plotlyPCA_demo <- function() {
  
  df <- df
  sample_anno <- sample_meta
  sample_anno_col <- "sampleLabel"
  
  ui <- fluidPage(plot3DPCA_ui("x"))
  server <- function(input, output, session) {
    plot3DPCA_server("x", reactive({df}), reactive({sample_anno}),
                   reactive({sample_anno_col}))
  }
  shinyApp(ui, server)
}

