# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate a histogram
#'
#' @param id A string.
#' @param df A dataframe.
#' @param title A string, the title to use.
#' @returns A Shiny module.
#' @examples
#' plotHist_demo()
#' plotHist_demo_1()
#' plotHist_demo_2()

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

plotHist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bins"), "bins", 20, min = 1, step = 1),
    plotOutput(ns("hist"))
  )
}

#### Server function of the module #### ----------

plotHist_server <- function(id, df, title = reactive("Histogram")) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(title))

  moduleServer(id, function(input, output, session) {
    histPlot <- reactive({
      req(is.numeric(df()))
      main <- paste0(title(), " [", input$bins, "]")
      ggplot_truehist(df(), breaks = input$bins, title = main)
    })
    
    output$hist <- renderPlot({
      histPlot()
      # hist(df(), breaks = input$bins, main = main)
    })
    return(histPlot)
  })
}

#### Demo function of the module #### ----------
plotHist_demo <- function() {
  alldata <- load("./example_data/MS_2.rda")

  d <- unlist(df)
  ui <- fluidPage(plotHist_ui("hist"))
  server <- function(input, output, session) {
    plotHist_server("hist", reactive({
      d
    }))
  }
  shinyApp(ui, server)
}

plotHist_demo_1 <- function() {
  source("dataImport.R")
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        dataImport_ui("datafile", "User data (.csv format)"),
      ),
      mainPanel(
        plotHist_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    data <- dataImport_server("datafile")
    df <- reactive({
      data() %>%
        unlist()
    })

    plotHist_server("hist", df)
  }
  shinyApp(ui, server)
}

plotHist_demo_2 <- function() {
  source("dataImport.R")
  source("selectVar.R")
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        dataImport_ui("datafile", "User data"),
        selectVar_ui("var", "Choose a column"),
      ),
      mainPanel(
        plotHist_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    data <- dataImport_server("datafile")
    var <- selectVar_server("var", data, filter = is.numeric)
    plotHist_server("hist", var)
  }
  shinyApp(ui, server)
}
