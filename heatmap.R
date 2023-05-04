# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)
# author: Louis Le Nézet (louislenezet@gmail.fr)

# Documentation
#' R Shiny module to generate a heatmap
#'
#' @param id A string.
#' @param df A dataframe containing the value for each sample (column) for all the features (rows).
#' @param sample_anno A data frame containing the sample informations.
#' @param sample_anno_col The column name of sample category in sample_anno.
#' @param feature_anno A data frame containing the features informations.
#' @param feature_anno_col The column name of feature category in feature_anno.
#' @param rowname_switch A boolean.
#' @param colname_switch A boolean.
#' @returns A Shiny module.
#' @examples
#' heatmap_demo()

#### Library needed #### ----------
library(shiny)
library(pheatmap)
library(dplyr)

#### Function needed to work #### ----------
#' plot Heatmap
#'
#' @param df A data frame with feature (row) by sample (column)
#' @param sample_anno A data frame, containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param feature_anno A data frame, containing feature metadata
#' @param feature_anno_col Character. Feature annotation column in `feature_anno`
#' @param rowname_switch Logical, show feature annotation on heatmap
#' @param colname_switch Logical, show sample annotation on heatmap
#' @param main Character. Title of the heatmap
#'
#' @return A heatmap plot generated with pheatmap
#' @export
#'
#' @examples

plotHeatmap <- function(df, sample_anno, sample_anno_col,
                    feature_anno, feature_anno_col,
                    rowname_switch = TRUE, colname_switch = TRUE, main = "Heatmap") {
  require(pheatmap)
  require(dplyr)

  # select annotation columns for sample meta and feature meta
  sample_anno <- sample_anno[sample_anno_col] # data.frame
  feature_anno <- feature_anno[, feature_anno_col] # character vector


  # draw hm
  pheatmap(
    df,
    annotation_col = sample_anno,
    labels_row = feature_anno,
    annotation_names_col = FALSE,
    angle_col = 45,
    scale = "row",
    show_rownames = rowname_switch,
    show_colnames = colname_switch,
    # cluster_rows = T,cluster_cols = T,
    # cellwidth = 30.0,
    # fontsize_row = 8,
    # fontsize_col = 8,
    # treeheight_col = 25,
    # treeheight_row = 25,
    # fontsize_number = 6,
    main = main
  )
}


#### UI function of the module #### ----------
plotHeatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    downloadButton(ns("dnld"), label = "")
  )
}

#### Server function of the module #### ----------
plotHeatmap_server <- function(id, df, sample_anno, sample_anno_col,
                           feature_anno, feature_anno_col,
                           rowname_switch = TRUE, colname_switch = TRUE) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(sample_anno))
  stopifnot(is.reactive(sample_anno_col))
  stopifnot(is.reactive(feature_anno))
  stopifnot(is.reactive(feature_anno_col))

  moduleServer(id, function(input, output, session) {
    plot <- reactive({

            plotHeatmap(df(), sample_anno(), sample_anno_col(),
                        feature_anno(), feature_anno_col(),
                        rowname_switch = TRUE, colname_switch = TRUE,
                        main = paste("Heatmap: ", dim(df())[1], "features", dim(df())[2], "samples")
      )
    })
    output$plot <- renderPlot({
      plot()
    })
    output$dnld <- downloadHandler(
      filename = function() {
        paste0("heatmap", ".png")
      },
      content = function(file) {
        ggsave(file, plot(), width = 18, height = 10)
      }
    )
  })
}

#### Demo function of the module #### ----------
load("./example_data/MS_2.rda")
plotHeatmap_demo <- function() {
  df <- df
  sample_anno <- sample_meta
  sample_anno_col <- "sampleLabel"
  feature_anno <- feature_meta
  feature_anno_col <- "featureName"

  ui <- fluidPage(plotHeatmap_ui("x"))
  server <- function(input, output, session) {

          plotHeatmap_server(
                  "x", reactive({df}), reactive({sample_anno}), reactive({sample_anno_col}),
                  reactive({feature_anno}), reactive({feature_anno_col}) )
          }
  shinyApp(ui, server)
}

# TODO list for this template
# TODO update documentation at the top of the module to match the necessary parameters of the server.
