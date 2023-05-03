# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)

# Documentation
#' R Shiny module to generate a heatmap from a data matrix, a sample metadata and a feature metadata
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(shiny)
library(pheatmap)
library(dplyr)

#### Function needed to work #### ----------
#' plot Heatmap
#'
#' @param df A data frame or tibble. Normalized data matrix with feature (row) by sample (column)
#' @param sample_anno A data frame, containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param feature_anno A data frame, containing feature metadata
#' @param feature_anno_col Character. Feature annotation column in `feature_anno`
#' @param rowname_switch Logical, show feature annotation on heatmap
#' @param colname_switch Logical, show sample annotation on heatmap
#' @param main Character. Title of the heatmap
#'
#' @return
#' @export
#'
#' @examples
heatmap <- function(df, sample_anno, sample_anno_col,
                    feature_anno, feature_anno_col,
                    rowname_switch = TRUE, colname_switch = TRUE, main = "Heatmap") {

        require(pheatmap)
        require(dplyr)

        # select annotation columns for sample meta and feature meta
        sample_anno = sample_anno[sample_anno_col] # data.frame
        feature_anno = feature_anno[, feature_anno_col] # character vector


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
heatmap_ui <- function(id) {

        fluidRow(
                column(11, plotOutput(NS(id, "plot"))),
                column( 1, downloadButton(NS(id, "dnld"), label = ""))
        )

}

#### Server function of the module #### ----------
heatmap_server <- function(id, df, sample_anno, sample_anno_col,
                           feature_anno, feature_anno_col,
                           rowname_switch = TRUE, colname_switch = TRUE) {
        stopifnot(is.reactive(df))
        stopifnot(is.reactive(sample_anno))
        stopifnot(is.reactive(sample_anno_col))
        stopifnot(is.reactive(feature_anno))
        stopifnot(is.reactive(feature_anno_col))

        moduleServer(id, function(input, output, session) {

                plot <- reactive({
                        heatmap(df(), sample_anno(), sample_anno_col(),
                                feature_anno(), feature_anno_col(),
                                rowname_switch = TRUE, colname_switch = TRUE,
                                main = paste("Heatmap: ",dim(df())[1], "features", dim(df())[2], "samples"))
                })
                output$plot <- renderPlot({plot()})
                output$dnld <- downloadHandler(
                        filename = function() {paste0('heatmap', '.png')},
                        content = function(file) {ggsave(file, plot(), width = 18, height = 10)}
                )

        })
}

#### Demo functionof the module #### ----------
load("./example_data/MS_2.rda")
heatmap_demo <- function() {

        df <- df
        sample_anno <- sample_meta
        sample_anno_col <- "sampleLabel"
        feature_anno <- feature_meta
        feature_anno_col <- "featureName"

        ui <- fluidPage(heatmap_ui("x"))
        server <- function(input, output, session) {
                heatmap_server("x", reactive({df}), reactive({sample_anno}), reactive({sample_anno_col}),
                               reactive({feature_anno}), reactive({feature_anno_col}))
        }
        shinyApp(ui, server)

}

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation
