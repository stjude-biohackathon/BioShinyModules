#' Plot ggplot histogram
#'
#' @param data A numeric vector of data to use to generate a histogram.
#' @param breaks Numeric. Number of bins. Default is 50
#' @param title Character. Plot title.
#'
#' @return A ggplot histogram with a title
#' @keywords histogram
#' @author Max Qiu, Louis Le Nézet, Alyssa Obermayer, Jared Andrews
#' @import ggplot2
#' @export
ggplot_truehist <- function(data, breaks = 50, title = NULL) {
  data <- as.numeric(data)
  ggplot() +
    aes(data) +
    geom_histogram(aes(data),
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


#' Histogram plot ui module
#'
#' @description R Shiny module UI to generate a histogram plot.
#'
#' @details This module create a histogram plot from a given
#' dataframe.
#' The number of bins is selectable by the user.
#'
#' @param id A string.
#' @returns A Shiny UI.
#' @examplesIf interactive()
#' ui <- shiny::fluidPage(
#'       plotHist_ui("hist")
#' )
#'
#' server <- function(input, output, session) {
#'   speed <- reactive(cars$speed)
#'   plotHist_server("hist", speed)
#' }
#' shiny::shinyApp(ui, server)
#' 
#' @keywords histogram
#' @author Max Qiu, Louis Le Nézet, Alyssa Obermayer, Jared Andrews
#' @importFrom shiny NS tagList numericInput plotOutput
#' @export
plotHist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bins"), "bins", 20, min = 1, step = 1),
    plotOutput(ns("hist"))
  )
}


#' Histogram plot server module
#'
#' @description R Shiny module server to generate an histogram plot.
#'
#' @details This module create a histogram plot from a given
#' dataframe.
#' The number of bins is selectable by the user.
#'
#' @param id A string.
#' @param data A reactive vector of numeric values
#' @param title A reactive string to be used as a title
#' @returns moduleServer containing histogram plot output and server code for generating plot.
#' @examples
#' if (interactive()) {
#'   plotHist_demo()
#' }
#' @keywords histogram
#' @author Max Qiu, Louis Le Nézet, Alyssa Obermayer, Jared Andrews
#' @importFrom shiny is.reactive moduleServer reactive req renderPlot
#' @export plotHist_server
plotHist_server <- function(id, data, title = reactive("Histogram")) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(title))

  moduleServer(id, function(input, output, session) {
    hist_plot <- reactive({
      req(is.numeric(data()))
      main <- paste0(title(), " [", input$bins, " bins]")
      ggplot_truehist(data(), breaks = input$bins, title = main)
    })

    output$hist <- renderPlot({
      hist_plot()
    })
  })
}

#### Demo function of the module #### ----------
#' Histogram plot demo app
#'
#' @description R Shiny module server to generate an histogram plot.
#'
#' @details This module create a histogram plot app.
#' The user can select the dataframe and the variable to use,
#' as well as the number of bins for the histogramm.
#'
#' @returns A shiny app
#' @examples
#' if (interactive()) {
#'   plotHist_demo()
#' }
#' @keywords histogram
#' @importFrom shiny fluidPage mainPanel selectInput sidebarLayout sidebarPanel shinyApp
#' @export plotHist_demo
plotHist_demo <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectVar_ui("var", "Choose a column"),
      ),
      mainPanel(
        plotHist_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    var <- selectVar_server("var", cars, filter = is.numeric)
    plotHist_server("hist", var)
  }
  shinyApp(ui, server)
}
