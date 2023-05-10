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
#' ggsaveBoth_demo()
#### Library needed #### ----------
library(ggplot2)
library(shiny)


#### UI function of the module #### ----------
ggsaveBoth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("var1"), "Figure width", value = 15, min = 0, max = 20),
    numericInput(ns("var2"), "Figure height", value = 10, min = 0, max = 20),
    radioButtons(ns("var3"), label = "Select the file type", choices = list("png", "pdf"), selected = "png"),
    downloadButton(ns("dnld"), label = "")
  )
}

#### Server function of the module #### ----------

ggsaveBoth_server <- function(id, my_plot) {
  stopifnot(is.reactive(my_plot))

  moduleServer(id, function(input, output, session) {
    output$dnld <- downloadHandler(
      filename = function() {
        paste("saveplot", input$var3, sep = ".")
      },
      content = function(file) {
        ggsave(filename = file, plot = my_plot(), width = input$var1, height = input$var2, device = input$var3)
      }
    )
  })
}

#### Demo function of the module #### ----------
ggsaveBoth_demo <- function() {
  source("plotHist.R")
  source("dataImport.R")
  ui <- fluidPage(
    dataImport_ui("datafile", "User data"),
    plotHist_ui("hist"),
    ggsaveBoth_ui("savehist")
  )

  server <- function(input, output, session) {
    data <- dataImport_server("datafile")
    df <- reactive({
      print(summary(data()))
      data()
    })
    my_plot <- plotHist_server("hist", df)

    ggsaveBoth_server("savehist", my_plot)
  }
  shinyApp(ui, server)
}

ggsaveBoth_demo_2 <- function() {
        load("../example_data/MS_2.rda")
        source("plotHeatmap.R")
        df <- df
        sample_anno <- sample_meta
        sample_anno_col <- "sampleLabel"
        feature_anno <- feature_meta
        feature_anno_col <- "featureName"

        ui <- fluidPage(plotHeatmap_ui("plotHeatmap"),
                        ggsaveBoth_ui("savehist"))
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
                ggsaveBoth_server("savehist", my_plot)
        }
        shinyApp(ui, server)
}

# TODO rename the function
