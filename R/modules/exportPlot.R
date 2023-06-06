# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to save a plot to file, choosing between png and pdf
#'
#' @param id A string.
#' @param plot A plot to save.
#' @returns A Shiny module.
#' @examples
#' exportPlot_demo()
#### Library needed #### ----------
library(ggplot2)
library(shiny)
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido==0.1.*')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')

#### UI function of the module #### ----------
exportPlot_ui <- function(id) {
    ns <- NS(id)
    tagList(
        numericInput(ns("width"), "Figure width", value = 15, min = 0, max = 20),
        numericInput(ns("height"), "Figure height", value = 10, min = 0, max = 20),
        radioButtons(ns("ext"), label = "Select the file type", choices = list("png", "pdf", "html"), selected = "png"),
        downloadButton(ns("dnld"), label = "")
    )
}

#### Server function of the module #### ----------

exportPlot_server <- function(id, my_plot, is_plotly = FALSE) {
    stopifnot(is.reactive(my_plot))
    moduleServer(id, function(input, output, session) {
        output$dnld <- downloadHandler(
            filename = function() {
                paste("saveplot", input$ext, sep = ".")
            },
            content = function(file) {
                if (is_plotly) {
                    require(plotly)
                    if (input$ext == "html") {
                        htmlwidgets::saveWidget(file = file, my_plot())
                    } else {
                        # save_image(file = file, p = my_plot(), width = input$width, height = input$height)
                        message("Should export to html for interactive plot")
                        NULL
                    }
                } else {
                    if (input$ext == "html") {
                        htmltools::save_html(my_plot(), file = file)
                    } else {
                        ggsave(filename = file, plot = my_plot(), width = input$width, height = input$height, device = input$ext)
                    }
                }
            }
        )
    })
}

#### Demo function of the module #### ----------
exportPlot_demo <- function() {
    source("plotHist.R")
    source("../dataImport.R")
    ui <- fluidPage(
        dataImport_ui("datafile", "User data"),
        plotHist_ui("hist"),
        exportPlot_ui("savehist")
    )

    server <- function(input, output, session) {
        data <- dataImport_server("datafile")
        df <- reactive({
            print(summary(data()))
            data()
        })
        my_plot <- plotHist_server("hist", df)

        exportPlot_server("savehist", my_plot)
    }
    shinyApp(ui, server)
}

exportPlot_demo_2 <- function() {
    load("../example_data/MS_2.rda")
    source("plotHeatmap.R")
    df <- df
    sample_anno <- sample_meta
    sample_anno_col <- "sampleLabel"
    feature_anno <- feature_meta
    feature_anno_col <- "featureName"

    ui <- fluidPage(
        plotHeatmap_ui("plotHeatmap"),
        exportPlot_ui("savehist")
    )
    server <- function(input, output, session) {
        my_plot <- plotHeatmap_server(
            "plotHeatmap", reactive({
                df
            }), reactive({
                sample_anno
            }), reactive({
                sample_anno_col
            }),
            reactive({
                feature_anno
            }), reactive({
                feature_anno_col
            })
        )
        exportPlot_server("savehist", my_plot)
    }
    shinyApp(ui, server)
}