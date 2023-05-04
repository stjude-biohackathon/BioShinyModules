# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate a histogram
#'
#' @param id A string.
#' @param df A dataframe.
#' @param title A string, the title to use.
#' @returns A Shiny module.
#' @examples
#' histogram_demo()
#' histogram_demo_1()
#' histogram_demo_2()

#### Library needed #### ----------
library(shiny)
library(ggplot2)

#### Function needed to work #### ----------
#' plot Histogram
#'
#' @param data A numeric vector.
#' @param breaks Numeric. Number of bins. Default is 50
#' @param title Character. Plot title.
#'
#' @return A ggplot histogram with a title

ggplot_truehist <- function(data, breaks = 50, title) {
  data <- as.numeric(data)
  ggplot() +
    aes(data) +
    geom_histogram(aes(y = after_stat(density)),
      bins = breaks,
      fill = "cornflowerblue", color = "gray30"
    ) +
    labs(title = title) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    )
}

#### UI function of the module #### ----------

histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bins"), "bins", 20, min = 1, step = 1),
    plotOutput(ns("hist")),
    downloadButton(ns("dnld"), label = "")
  )
}

#### Server function of the module #### ----------

histogram_server <- function(id, df, title = reactive("Histogram")) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(title))

  moduleServer(id, function(input, output, session) {
    output$hist <- renderPlot({
      req(is.numeric(df()))
      main <- paste0(title(), " [", input$bins, "]")
      ggplot_truehist(df(), breaks = input$bins, title = main)
      # hist(df(), breaks = input$bins, main = main)
    })

    output$dnld <- downloadHandler(
      filename = function() {
        paste0("histogram", ".png")
      },
      content = function(file) {
        ggsave(file, plot(), width = 18, height = 10)
      }
    )
  })
}

#### Demo function of the module #### ----------
load("./example_data/MS_2.rda")

histogram_demo <- function() {
  d <- unlist(df)
  ui <- fluidPage(histogram_ui("hist"))
  server <- function(input, output, session) {
    histogram_server("hist", reactive({
      d
    }))
  }
  shinyApp(ui, server)
}

source("csvImport.R")
histogram_demo_1 <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        csvImport_ui("datafile", "User data (.csv format)"),
      ),
      mainPanel(
        histogram_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    data <- csvImport_server("datafile", stringsAsFactors = TRUE)

    df <- reactive({
      data() %>%
        unlist()
    })

    histogram_server("hist", df)
  }
  shinyApp(ui, server)
}

source("selectVar.R")
histogram_demo_2 <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        csvImport_ui("datafile", "User data (.csv format)"),
        selectVar_ui("var", "Choose a column"),
      ),
      mainPanel(
        histogram_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    data <- csvImport_server("datafile", stringsAsFactors = TRUE)
    var <- selectVar_server("var", data, filter = is.numeric)
    histogram_server("hist", var)
  }
  shinyApp(ui, server)
}

# TODO list for this template
# TODO add minimal data for testing
# TODO csv import doesn't seems to work with .rda
