# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)

# Documentation
#' R Shiny module to save a plot to file, choosing between png and pdf
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
library(ggplot2)
library(shiny)

#### Function needed to work #### ----------


#### UI function of the module #### ----------

ggsaveBoth_ui <- function(id) {

        tagList(
                numericInput( NS(id, "var1"), "Figure width", value = 15, min = 0, max = 20),
                numericInput( NS(id, "var2"), "Figure height", value = 10, min = 0, max = 20),
                radioButtons( NS(id, "var3"), label = "Select the file type", choices = list("png", "pdf"), selected = "png"),
                downloadButton( NS(id, "dnld"), label = "")
                )
}

#### Server function of the module #### ----------

ggsaveBoth_server <- function(id, plot) {

        stopifnot(is.reactive(plot))

        moduleServer(id, function(input, output, session) {

                output$dnld <- downloadHandler(

                        filename =  function() {paste("saveplot", input$var3, sep=".")},
                        content = function(file) {
                                ggsave(file, plot(), width = input$var1, height = input$var2, device = input$var3)
                                # if(input$var3 == "png")
                                #         png(file) # open the png device
                                # else
                                #         pdf(file) # open the pdf device
                                # dev.off()
                        }
                )

        })
}

#### Demo functionof the module #### ----------
load("./example_data/MS_2.rda")
source("plotHist.R")

ggsaveBoth_demo <- function() {

        ui <- fluidPage(
                csvImport_ui("datafile", "User data (.csv format)"),
                histogram_ui("hist"),
                ggsaveBoth_ui("savehist")
                )

        server <- function(input, output, session) {
                data <- csvImport_server("datafile", stringsAsFactors = TRUE)

                df <- reactive({
                        data() %>%
                                unlist()
                })

                plot = histogram_server("hist", df)

                getplot = reactive({
                        plot()
                })
                ggsaveBoth_server("savehist", getplot )
        }
        shinyApp(ui, server)
}



ggsaveBoth_demo()
# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation
