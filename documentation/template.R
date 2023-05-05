# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: @

# Documentation
#' R Shiny module to generate XXX graph
#' 
#' @param id A string.
#' @param df A dataframe.
#' @param vbl_x The variable name to use in df as abscissa.
#' @param vbl_y The variable name to use in df as ordinate.
#' @returns A Shiny module.
#' @examples
#' test_demo()

#### Library needed #### ----------
# TODO Add here all the library needed for the module (ggplot2, tidyr, ...)
library(ggplot2)
library(shiny)

#### Function needed to work #### ----------
# TODO write in this section the different function use by the module

my_plot <- function(df, x_var, y_var) {
  
  ggplot(df) +
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]]
    ) +
    geom_line() +
    theme_minimal()
}


#### UI function of the module #### ----------
# TODO Add here the UI function of the module

test_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("plot")),
    downloadButton(ns("dnld"), label = "")
  )
  
}

#### Server function of the module #### ----------
# TODO Add here the server function of the module

test_server <- function(id, df, vbl_x, vbl_y) {
  
  moduleServer(id, function(input, output, session) {
    
    plot <- reactive({my_plot(df(), vbl_x, vbl_y)})
    output$plot <- renderPlot({plot()})
    output$dnld <- downloadHandler(
      filename = function() {paste0(vbl_x, "by", vbl_y, '.png')},
      content = function(file) {ggsave(file, plot())}
    )
    
  })
}

#### Demo function of the module #### ----------
# TODO Add here the demo function of the module

test_demo <- function() {
  
  df <- data.frame(day = 1:30, arr_delay = 1:30)
  ui <- fluidPage(test_ui("x"))
  server <- function(input, output, session) {
    test_server("x", reactive({df}), "day", "arr_delay")
  }
  shinyApp(ui, server)
}

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation at the top of the module to match the necessary parameters of the server.
