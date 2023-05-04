# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)

# Documentation
#' R Shiny module to generate sample-to-sample distance plot
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(shiny)
library(pheatmap)

#### Function needed to work #### ----------
#' plot sample-to-sample distance
#'
#' @param d A `dist` object.
#' @param sample_anno A data frame, containing sample metadata, with sample names as rownames.
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param rowname_switch Logical, show feature annotation on heatmap
#' @param colname_switch Logical, show sample annotation on heatmap
#'
#' @return
#' @export
#'
#' @examples
plotDist <- function(d, sample_anno, sample_anno_col,
                     rowname_switch = TRUE, colname_switch = TRUE) {

        require(pheatmap)
        require(dplyr)

        # select annotation columns for sample meta and feature meta
        sample_anno = sample_anno[sample_anno_col] # data.frame

        # check distance matrix
        if (class(d) != "dist"){stop("Provided `d` is not a `dist` object.")}
        if (length(d) != sum(1:(nrow(sample_anno)-1) ) ){stop("Provided `d` is not the corrected length based on provided `sample_anno`.")}

        # plot
        pheatmap(
                as.matrix(d),
                clustering_distance_rows = d,
                clustering_distance_cols = d,
                annotation_col = sample_anno,
                annotation_row = sample_anno,
                annotation_names_col = FALSE,
                # display_numbers = TRUE,
                angle_col = 45,
                scale = "none",
                show_rownames = rowname_switch,
                show_colnames = colname_switch,
                # cluster_rows = T,cluster_cols = T,
                # cellwidth = 30.0,
                # fontsize_row = 8,
                # fontsize_col = 8,
                # treeheight_col = 25,
                # treeheight_row = 25,
                # fontsize_number = 6,
                main = paste("Sample-to-sample distance matrix: ",
                             nrow(sample_anno), "samples")
        )
        if (isTRUE(save)) {
                ggsave(TITLE, path = folder,
                       device = "png",
                       plot = p3, dpi = 300, width = 17, height = 10
                )
        }
}


#### UI function of the module #### ----------
plotDist_ui <- function(id) {

        fluidRow(
                column(11, plotOutput(NS(id, "plot"))),
                column( 1, downloadButton(NS(id, "dnld"), label = ""))
        )

}

#### Server function of the module #### ----------
plotDist_server <- function(id, d, sample_anno, sample_anno_col,
                            rowname_switch = TRUE, colname_switch = TRUE) {

        moduleServer(id, function(input, output, session) {

                plot <- reactive({
                        plotDist(d(), sample_anno(), sample_anno_col(),
                                 rowname_switch = TRUE, colname_switch = TRUE)
                })
                output$plot <- renderPlot({plot()})
                output$dnld <- downloadHandler(
                        filename = function() {paste0("dist", '.png')},
                        content = function(file) {ggsave(file, plot())}
                )

        })
}

#### Demo functionof the module #### ----------
load("./example_data/MS_2.rda")
plotDist_demo <- function() {

        d <- dist(t(df))
        #d = dist(dist_matrix)
        sample_anno <- sample_meta
        sample_anno_col <- "sampleLabel"

        ui <- fluidPage(plotDist_ui("x"))
        server <- function(input, output, session) {
                plotDist_server("x", reactive({d}), reactive({sample_anno}), reactive({sample_anno_col}))
        }
        shinyApp(ui, server)

}

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation
