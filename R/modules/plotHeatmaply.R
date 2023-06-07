# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)


# Documentation
#' R Shiny module to generate a heatmap/distance/correlation using `heatmaply`
#'
#' @param id A string.
#' @param df A dataframe containing the value for each sample (column) for all the features (rows).
#' @returns A Shiny module.
#' @examples
#' plotHeatmaply_demo()
#### Library needed #### ----------
library(shiny)
library(heatmaply)
library(RColorBrewer)

#### Function needed to work #### ----------
#' plot Heatmap
#'
#' @param df A data frame with feature (row) by sample (column)
#'
#' @return A heatmap plot generated with pheatmap
#' @export

plotHeatmaply <- function(df, type = c("heatmap", "correlation", "distance"),
                        direction = ifelse(type == "heatmap", NULL, c("col", "row") ),
                        select_dist = ifelse(type == "distance",
                                             c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), NULL),
                        select_corr = ifelse(type == "correlation",
                                             c("pearson", "kendall", "spearman"), NULL),
                        #select_hclust = c("ward.D", "ward.D2", "single", "complete", "average",
                        #                   "mcquitty", "median", "centroid"),
                        select_scale = ifelse(type == "heatmap", c("column", "row"), NULL ),
                        draw_dendrogram = c('none', 'row', 'column', 'both' ),
                        showRowDendrogram = TRUE,
                        showColDendrogram = TRUE,
                        showRowLabel = TRUE,
                        showColLabel = TRUE,
                        pal,
                        title
                        ) {

        if (type == "heatmap"){

                cat("When 'type' == 'heatmap', 'direction', 'select_dist', 'select_corr' are set at NULL. ")
                mat = as.matrix(df)

        } else if (type == "correlation"){

                cat("When 'type' == 'correlation', 'select_scale' is set at 'none' and 'select_dist' is set at NULL. ")

                if (direction == "row"){
                        mat = cor(t(df), method = select_corr)
                        select_scale = "none"

                } else if (direction == "col"){
                        mat = cor(df, method = select_corr)
                        select_scale = "none"
                }
        } else if (type == "distance"){

                cat("When 'type' == 'distance', 'select_scale' is set at 'none', and 'select_corr' is set at NULL. ")

                if (direction == "row"){
                        mat = as.matrix(dist(df, method = select_dist))
                        select_scale = "none"
                } else if (direction == "col"){
                        mat = as.matrix(dist(t(df), method = select_dist))
                        select_scale = "none"
                }
        }


        # select colors for hm and anno
        hm_color <- colorRampPalette(brewer.pal(n = 11, name = pal))(256)

        # draw hm
        heatmaply( mat,
                   colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 8, name = pal)))(200),
                   scale = select_scale,
                   dendrogram = draw_dendrogram,
                   show_dendrogram = c(showRowDendrogram, showColDendrogram),
                   dist_method = NULL,
                   hclust_method = NULL,
                   showticklabels = c(showRowLabel, showColLabel),
                   main = title
        )
}

palettes <- RColorBrewer::brewer.pal.info
hm_palettes <- rownames(palettes[which(palettes$category == "div"),])
dist_palettes <- rownames(palettes[which(palettes$category == "seq"),])
#dist = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
#corr = c("pearson", "kendall", "spearman")
#scale = c("none", "row", "column")
#dendrogram = c('none', 'row', 'column', 'both' )

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
                                    choices = dist_palettes, selected = "Blues"
                        ),
                        selectInput(ns("direction"), "Select direction",
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
                                    choices = hm_palettes, selected = "RdYlBu"
                        ),
                        selectInput(ns("direction"), "Select direction",
                                    choices = c("col", "row"), selected = "col"
                        ),
                        selectInput(ns("corr"), "Select correlation method",
                                    choices = c("pearson", "kendall", "spearman"),
                                    selected = "pearson"
                        )

                ),

                selectInput(ns("dendrogram"), "Draw Dendrogram",
                            choices = c('none', 'row', 'column', 'both' ), selected = "both"
                ),
                checkboxInput(ns("showRowDendrogram"), "Show row dendrogram", value = TRUE),
                checkboxInput(ns("showColDendrogram"), "Show col dendrogram", value = TRUE),
                checkboxInput(ns("showRowLabel"), "Show row label", value = TRUE),
                checkboxInput(ns("showColLabel"), "Show col label", value = TRUE),
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
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
                                              pal = input$hm_palette,
                                              title = input$title
                                      )
                        } else if (input$plot_type == "distance"){
                                plotHeatmaply(df(),
                                              type = input$plot_type,
                                              direction = input$direction,
                                              select_dist = input$dist,
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
                                              pal = input$dist_palette,
                                              title = input$title
                                )
                        } else if (input$plot_type == "correlation"){
                                plotHeatmaply(df(),
                                              type = input$plot_type,
                                              direction = input$direction,
                                              select_corr = input$corr,
                                              draw_dendrogram = input$dendrogram,
                                              showRowDendrogram = input$showRowDendrogram,
                                              showColDendrogram = input$showColDendrogram,
                                              showRowLabel = input$showRowLabel,
                                              showColLabel = input$showColLabel,
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

        ui <- fluidPage(plotHeatmaply_ui("plotHeatmaply"))
        server <- function(input, output, session) {

                plotHeatmaply_server("plotHeatmaply",
                                     reactive({df})
                )
        }
        shinyApp(ui, server)
}
