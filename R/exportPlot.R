# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)
# author: Louis Le Nézet (louislenezet@gmail.com)

#### Library needed #### ----------
usethis::use_package("ggplot2")
usethis::use_package("shiny")
usethis::use_package("htmltools")
usethis::use_package("htmlwidgets")

#### UI function of the module #### ----------
#' Export plot ui module
#'
#' @description R Shiny module UI to export plot
#'
#' @details This module allow to export multiple type of plot.
#' The file type currently supported are png, pdf and html.
#' The UI ask the user for the file type to export to, the
#' width and the height of the plot to generate.
#' When the plot is generated with plotly, set is_plotly to TRUE
#' and export it to html.
#'
#' @param id A string.
#' @returns A Shiny UI.
#' @examples
#' \dontrun{
#'     exportPlot_demo()
#' }
#' @keywords hplot
#' @export exportPlot_ui
exportPlot_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::numericInput(ns("width"), "Figure width",
            value = 15, min = 0, max = 20),
        shiny::numericInput(ns("height"), "Figure height",
            value = 10, min = 0, max = 20),
        shiny::radioButtons(ns("ext"), label = "Select the file type",
            choices = list("png", "pdf", "html"), selected = "png"),
        shiny::downloadButton(ns("dnld"), label = "")
    )
}

#### Server function of the module #### ----------
#' Export plot server module
#'
#' @description R Shiny module server to export plot
#'
#' @details This module allow to export multiple type of plot.
#' The file type currently supported are png, pdf and html.
#' The UI ask the user for the file type to export to, the
#' width and the height of the plot to generate.
#' When the plot is generated with plotly, set is_plotly to TRUE
#' and export it to html.
#'
#' @param id A string.
#' @param my_plot Reactive object containing the plot.
#' @param is_plotly Boolean defining if the plot is interactiv or not.
#' @returns A Shiny UI.
#' @examples
#' \dontrun{
#'     exportPlot_demo()
#' }
#' @keywords hplot
#' @export exportPlot_server
exportPlot_server <- function(id, my_plot, is_plotly = FALSE) {
    stopifnot(shiny::is.reactive(my_plot))
    shiny::moduleServer(id, function(input, output, session) {
        output$dnld <- shiny::downloadHandler(
            filename = function() {
                paste("saveplot", input$ext, sep = ".")
            },
            content = function(file) {
                if (is_plotly) {
                    if (input$ext == "html") {
                        htmlwidgets::saveWidget(file = file, my_plot())
                    } else {
                        message("Should export to html for interactive plot")
                        NULL
                    }
                } else {
                    if (input$ext == "html") {
                        htmltools::save_html(my_plot(), file = file)
                    } else {
                        ggplot2::ggsave(filename = file, plot = my_plot(),
                            device = input$ext,
                            width = input$width, height = input$height)
                    }
                }
            }
        )
    })
}

#### Demo function of the module #### ----------
#' Export plot demo app
#'
#' @description R Shiny app demo to export plot
#'
#' @details This demo allow to export an histogram plot from a
#' dataframe selected before hand.
#'
#' @returns A Shiny APP.
#' @examples
#' \dontrun{
#'     exportPlot_demo()
#' }
#' @keywords hplot
#' @export exportPlot_demo
exportPlot_demo <- function() {
    ui <- shiny::fluidPage(
        dataImport_ui("datafile", "User data"),
        plotHist_ui("hist"),
        exportPlot_ui("savehist")
    )

    server <- function(input, output, session) {
        data <- dataImport_server("datafile")
        df <- shiny::reactive({
            print(summary(data()))
            data()
        })
        my_plot <- plotHist_server("hist", df)

        exportPlot_server("savehist", my_plot)
    }
    shiny::shinyApp(ui, server)
}

#' Export plot server module
#'
#' @description R Shiny module serve to export plot
#'
#' @details This demo allow to export an heatmap plot from a
#' the example data MS_2.rda
#'
#' @returns A Shiny app.
#' @examples
#' \dontrun{
#'     exportPlot_demo_2()
#' }
#' @keywords hplot
#' @export exportPlot_demo_2
exportPlot_demo_2 <- function() {
    load("../../data_raw/MS_2.rda")
    source("modules/plotHeatmap.R")
    df <- df
    sample_anno <- sample_meta
    sample_anno_col <- "sampleLabel"
    feature_anno <- feature_meta
    feature_anno_col <- "featureName"

    ui <- shiny::fluidPage(
        plotHeatmap_ui("plotHeatmap"),
        exportPlot_ui("savehist")
    )
    server <- function(input, output, session) {
        my_plot <- plotHeatmap_server(
            "plotHeatmap", shiny::reactive({
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
    shiny::shinyApp(ui, server)
}