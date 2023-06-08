# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)


# Documentation
#' R Shiny module to generate a heatmap/distance/correlation using `heatmaply`
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' plotHeatmaply_demo()
#' plotHeatmaply_demo_2()
#### Library needed #### ----------
library(shiny)
library(heatmaply)
library(RColorBrewer)

#### Function needed to work #### ----------
#' plot Heatmap
#'
#' @param df A data frame
#' @param type Character, choose from one of three values for plot types
#' @param direction
#' @param select_dist Character
#' @param select_corr Character
#' @param select_scale Character
#' @param draw_dendrogram Character
#' @param showRowDendrogram Boolean
#' @param showColDendrogram Boolean
#' @param showRowLabel Boolean
#' @param showColLabel Boolean
#' @param pal Character
#' @param title Character
#'
#' @return A heatmap plot generated with pheatmap
#' @export

plotHeatmaply <- function(df, type = c("heatmap", "correlation", "distance"),
                        direction = ifelse(type == "heatmap", NULL, c("col", "row") ),
                        select_dist = ifelse(type == "distance",
                                             c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), NULL),
                        select_corr = ifelse(type == "correlation",
                                             c("pearson", "kendall", "spearman"), NULL),
                        select_hclust = c("ward.D", "ward.D2", "single", "complete", "average",
                                           "mcquitty", "median", "centroid"),
                        select_scale = ifelse(type == "heatmap", c("column", "row"), "none" ),
                        draw_dendrogram = c('none', 'row', 'column', 'both' ),
                        showRowDendrogram = TRUE,
                        showColDendrogram = TRUE,
                        showRowLabel = TRUE,
                        showColLabel = TRUE,
                        fontsize_col,
                        fontsize_row,
                        pal,
                        title
                        ) {

        df = data.frame(df)

        if (type == "heatmap"){

                cat("When 'type' == 'heatmap', 'direction', 'select_dist', 'select_corr' are set at NULL. \n ")
                mat = as.matrix(df)

        } else if (type == "correlation"){

                cat("When 'type' == 'correlation', 'select_scale' is set at 'none' and 'select_dist' is set at NULL. \n ")

                if (direction == "row"){
                        mat = cor(t(df), method = select_corr)
                        select_scale = "none"

                } else if (direction == "col"){
                        mat = cor(df, method = select_corr)
                        select_scale = "none"
                }
        } else if (type == "distance"){

                cat("When 'type' == 'distance', 'select_scale' is set at 'none', and 'select_corr' is set at NULL. \n")

                if (direction == "row"){
                        mat = as.matrix(dist(df, method = select_dist))
                        select_scale = "none"
                } else if (direction == "col"){
                        mat = as.matrix(dist(t(df), method = select_dist))
                        select_scale = "none"
                }
        }


        # select colors for hm and anno
        hm_color <- colorRampPalette(brewer.pal(n = palettes$maxcolors[rownames(palettes) == pal], name = pal))(256)

        # draw hm
        heatmaply( mat,
                   colors = hm_color,
                   scale = select_scale,
                   dendrogram = draw_dendrogram,
                   show_dendrogram = c(showRowDendrogram, showColDendrogram),
                   dist_method = NULL,
                   hclust_method = select_hclust,
                   showticklabels = c(showRowLabel, showColLabel),
                   fontsize_col = fontsize_col,
                   fontsize_row = fontsize_row,
                   main = title
        )
}

palettes <- RColorBrewer::brewer.pal.info
hm_palettes <- rownames(palettes[palettes$category == "div",])
dist_palettes <- rownames(palettes[palettes$category == "seq",])


#### UI function of the module #### ----------
plotHeatmaply_ui <- function(id) {
        ns <- NS(id)
        tagList(
                radioButtons(ns("plot_type"), "Select plot type",
                             choices = c("heatmap", "distance", "correlation"), selected = "heatmap"
                ),

                conditionalPanel(
                        condition = sprintf("input['%s'] == 'heatmap'", ns("plot_type")),
                                #"input.plot_type == 'heatmap'",
                        selectInput(ns("hm_palette"), "Choose color palette",
                                    choices = hm_palettes, selected = "RdYlBu"
                        ),
                        selectInput(ns("scale"), "Select scale",
                                    choices = c("none", "row", "column"), selected = "row"
                        )
                ),

                conditionalPanel(
                        condition = sprintf("input['%s'] == 'distance'", ns("plot_type")),
                                #"input.plot_type == 'distance'",
                        selectInput(ns("dist_palette"), "Choose color palette",
                                    choices = dist_palettes, selected = "YlGnBu"
                        ),
                        selectInput(ns("dist_direction"), "Select direction",
                                    choices = c("col", "row"), selected = "col"
                        ),
                        selectInput(ns("dist"), "Select distance metric",
                                    choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                                    selected = "euclidean"
                        )

                ),

                conditionalPanel(
                        condition = sprintf("input['%s'] == 'correlation'", ns("plot_type")),
                                #"input.plot_type == 'correlation'",
                        selectInput(ns("corr_palette"), "Choose color palette",
                                    choices = hm_palettes, selected = "RdBu"
                        ),
                        selectInput(ns("corr_direction"), "Select direction",
                                    choices = c("col", "row"), selected = "col"
                        ),
                        selectInput(ns("corr"), "Select correlation method",
                                    choices = c("pearson", "kendall", "spearman"),
                                    selected = "pearson"
                        )

                ),
                selectInput(ns("select_hclust"), "Select hclust method",
                            choices = c("ward.D", "ward.D2", "single", "complete", "average",
                                        "mcquitty", "median", "centroid"),
                            selected = "complete"
                ),

                selectInput(ns("dendrogram"), "Draw Dendrogram",
                            choices = c('none', 'row', 'column', 'both' ), selected = "both"
                ),
                checkboxInput(ns("showRowDendrogram"), "Show row dendrogram", value = TRUE),
                checkboxInput(ns("showColDendrogram"), "Show col dendrogram", value = TRUE),
                checkboxInput(ns("showRowLabel"), "Show row label", value = TRUE),
                checkboxInput(ns("showColLabel"), "Show col label", value = TRUE),
                sliderInput(ns("fontsize_col"),"Select font size for column label",
                            min = 5, max = 15, value = 8),
                sliderInput(ns("fontsize_row"),"Select font size for row label",
                            min = 5, max = 15, value = 8),
                textInput(ns("title"), "Title of the graph", value = ""),
                actionButton( ns("click_submit"), label = "Submit" ),
                plotOutput(ns("plot"))
        )
}

#### Server function of the module #### ----------
plotHeatmaply_server <- function(id, df) {
        stopifnot(is.reactive(df))

        moduleServer(id, function(input, output, session) {

                Heatmap_plot <- eventReactive( input$click_submit , {

                        if (input$plot_type == "heatmap"){

                                plotHeatmaply(df(),
                                              type = input$plot_type,
                                              select_scale = input$scale,
                                              select_hclust = input$select_hclust,
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
                                              fontsize_col = input$fontsize_col,
                                              fontsize_row = input$fontsize_row,
                                              pal = input$hm_palette,
                                              title = input$title
                                      )
                        } else if (input$plot_type == "distance"){
                                plotHeatmaply(df(),
                                              type = input$plot_type,
                                              direction = input$dist_direction,
                                              select_dist = input$dist,
                                              select_hclust = input$select_hclust,
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
                                              fontsize_col = input$fontsize_col,
                                              fontsize_row = input$fontsize_row,
                                              pal = input$dist_palette,
                                              title = input$title
                                )
                        } else if (input$plot_type == "correlation"){
                                plotHeatmaply(df(),
                                              type = input$plot_type,
                                              direction = input$corr_direction,
                                              select_corr = input$corr,
                                              select_hclust = input$select_hclust,
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
                                              fontsize_col = input$fontsize_col,
                                              fontsize_row = input$fontsize_row,
                                              pal = input$corr_palette,
                                              title = input$title
                                )
                        }


                })
                output$plot <- renderPlot({
                        Heatmap_plot()
                })
                return(Heatmap_plot)
        })
}

#### Demo function of the module #### ----------
plotHeatmap_demo <- function() {
        load("../../data-raw/MS_2.rda")
        df <- df

        ui <- fluidPage(
                plotHeatmaply_ui("plotHeatmaply")
        )

        server <- function(input, output, session) {

                my_plot <- plotHeatmaply_server("plotHeatmaply",
                                     reactive({df})
                )
        }
        shinyApp(ui, server)
}


plotHeatmap_demo_2 <- function() {
        source("../exportPlot.R")
        load("../../data-raw/MS_2.rda")
        df <- df

        ui <- fluidPage(
                plotHeatmaply_ui("plotHeatmaply"),
                exportPlot_ui("savepca")
        )

        server <- function(input, output, session) {

                my_plot <- plotHeatmaply_server("plotHeatmaply",
                                     reactive({df})
                                     )
                exportPlot_server("savepca", my_plot, is_plotly = TRUE)
        }
        shinyApp(ui, server)
}
