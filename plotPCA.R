# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)

# Documentation
#' R Shiny module to generate PCA plot
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

#### Function needed to work #### ----------
#' plot PCA
#'
#' @param data A data frame with feature (row) by sample (column).
#' @param sample_anno A data frame containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param title Character. Plot title
#'
#' @return GGplot PCA

ggplot_pca <- function(df, sample_anno, sample_anno_col, title = NULL) {
  require(pcaMethods)
  require(ggplot2)

  data <- as.matrix(df)
  class(data) <- "numeric"

  labels <- as.matrix(sample_anno[sample_anno_col])

  pc1 <- pcaMethods::pca(t(data), scale = "pareto")
  pc1merged <- merge(cbind(labels, t(data)),
    pcaMethods::scores(pc1),
    by = 0
  )
  ggplot(pc1merged, aes(PC1, PC2, colour = !!sym(sample_anno_col))) +
    geom_point() +
    stat_ellipse() +
    xlab(paste("PC1", round((pc1@R2[1] * 100), digits = 1), "% of the variance")) +
    ylab(paste("PC2", round((pc1@R2[2] * 100), digits = 1), "% of the variance")) +
    ggtitle(label = title)
}



#### UI function of the module #### ----------
plotPCA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    downloadButton(ns("dnld"), label = "")
  )
}

#### Server function of the module #### ----------
plotPCA_server <- function(id, df, sample_anno, sample_anno_col) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(df))
    stopifnot(is.reactive(sample_anno))
    stopifnot(is.reactive(sample_anno_col))

    plot <- reactive({
      ggplot_pca(df(), sample_anno(), sample_anno_col())
    })
    output$plot <- renderPlot({
      plot()
    })
    output$dnld <- downloadHandler(
      filename = function() {
        paste0("pca", ".png")
      },
      content = function(file) {
        ggsave(file, plot())
      }
    )
  })
}

#### Demo function of the module #### ----------
plotPCA_demo <- function() {
  load("./example_data/MS_2.rda")
  df <- df
  sample_anno <- sample_meta
  sample_anno_col <- "sampleLabel"

  ui <- fluidPage(plotPCA_ui("x"))
  server <- function(input, output, session) {
    plotPCA_server(
      "x", reactive({
        df
      }), reactive({
        sample_anno
      }),
      reactive({
        sample_anno_col
      })
    )
  }
  shinyApp(ui, server)
}
