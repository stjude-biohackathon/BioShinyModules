# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)
# author: Louis Le Nézet (louislenezet@gmail.com)
# author: Alyssa Obermayer (aobermayer4@gmail.com)

#### Library needed #### ----------
usethis::use_package("shiny")
usethis::use_package("ggplot2")

#### Function needed to work #### ----------
#' Plot ggplot histogram
#'
#' @param data A numeric vector.
#' @param breaks Numeric. Number of bins. Default is 50
#' @param title Character. Plot title.
#'
#' @return A ggplot histogram with a title
#' @keywords hplot
#' @export ggplot_truehist
ggplot_truehist <- function(data, breaks = 50, title) {
  data <- as.numeric(data)
  ggplot2::ggplot() +
    ggplot2::aes(data) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(stats::density)),
      bins = breaks,
      fill = "cornflowerblue", color = "gray30"
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      aspect.ratio = 1
    )
}

#### UI function of the module #### ----------
#' Histogram plot ui module
#'
#' @description R Shiny module UI to generate an hitogram plot.
#'
#' @details This module create a histogram plot from a given
#' dataframe.
#' The number of bins is selectable by the user.
#'
#' @param id A string.
#' @returns A Shiny UI.
#' @examples
#' \dontrun{
#'     plotHist_demo()
#' }
#' @keywords hplot
#' @export plotHist_ui
plotHist_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::numericInput(ns("bins"), "bins", 20, min = 1, step = 1),
    shiny::plotOutput(ns("hist"))
  )
}

#### Server function of the module #### ----------
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
#' @returns Histogram plot.
#' @examples
#' \dontrun{
#'     plotHist_demo()
#' }
#' @keywords hplot
#' @export plotHist_server
plotHist_server <- function(id, data, title = shiny::reactive("Histogram")) {
  stopifnot(shiny::is.reactive(data))
  stopifnot(shiny::is.reactive(title))

  shiny::moduleServer(id, function(input, output, session) {
    hist_plot <- shiny::reactive({
      shiny::req(is.numeric(data()))
      main <- paste0(title(), " [", input$bins, "]")
      ggplot_truehist(data(), breaks = input$bins, title = main)
    })

    output$hist <- shiny::renderPlot({
      hist_plot()
    })
    return(hist_plot)
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
#' \dontrun{
#'     plotHist_demo()
#' }
#' @keywords hplot
#' @export plotHist_demo
plotHist_demo <- function() {
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        dataImport_ui("datafile", "User data"),
        selectVar_ui("var", "Choose a column"),
      ),
      shiny::mainPanel(
        plotHist_ui("hist")
      )
    )
  )

  server <- function(input, output, session) {
    data <- dataImport_server("datafile")
    var <- selectVar_server("var", data, filter = is.numeric)
    plotHist_server("hist", var)
  }
  shiny::shinyApp(ui, server)
}
