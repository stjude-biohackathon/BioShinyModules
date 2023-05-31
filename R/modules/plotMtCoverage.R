# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: @KMcC73 (kelly.mccastlain@stjude.org)
# author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate a mitochondrial coverage plot.
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' mtCoverage_demo()

#### Library needed #### ----------
setwd("C:/Users/kmccastl/Desktop/BioHackathon")
library(shiny)
library(ggplot2)
library(colourpicker)
library(dplyr)
library(ggpubr)
library(shinycssloaders)
library(shinyjqui)

#### Function needed to work #### ----------
# Documentation
#' Plot mitochondrial coverage
#'
#' @param df A dataframe.
#' @param threshold value
#' @param mt_bp_value mtDNA bp value
#' @param area_col coverage color
#' @returns A mitochondrial coverage plot.

mitoCov <- function(df, threshold, bp_num, area_col) {
  ggplot(df, aes(x = position, y = mean, ymin = mean - sd, ymax = mean + sd)) +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    geom_area(fill = area_col, alpha = 0.4) +
    geom_hline(yintercept = threshold, linetype = 2) +
    geom_point(aes(x = bp_num), shape = 95, size = 1, color = "red") +
    theme(
      panel.background = element_rect(
        fill = "white", colour = "white", size = 0.5,
        linetype = "solid", color = "white"
      ),
      panel.border = element_blank(),

      # plot.title = element_text(color="black", size=14, face="bold.italic", hjust=0),
      axis.title.x = element_text(color = "black", size = 14, face = "plain"),
      axis.title.y = element_text(color = "black", size = 14, face = "plain"),
      legend.key = element_rect(fill = "white"),
      axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.text.x = element_text(
        face = "bold", color = "black",
        size = 12, angle = 0
      ),
      axis.text.y = element_text(
        face = "bold", color = "black",
        size = 12, angle = 0
      )
    ) +
    xlab("Mitochondria Position (bp)") +
    ylab("Ave coverage") +
    # labs(title = "CM 5primeGEX, scRNAseq PER CELL", subtitle = "Std Dev")+
    scale_x_continuous(breaks = seq(0, 16000, 2000)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    coord_cartesian(ylim = c(0, 600000))
}

#### UI function of the module #### ----------
mtCoverage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      colourInput(ns("area_col"), "Pick coverage color", value = "#69b3a2"),
      numericInput(ns("mt_bp_num"), "mtDNA bp value", value = 3243),
      numericInput(ns("cov_threshold"), "Threshold", value = 100)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(ns("mtCoverage"))), type = 6)
    )
  )
}

#### Server function of the module #### ----------
mtCoverage_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    mitoCov_plot <- reactive({
      mitoCov(df(), input$cov_threshold, input$mt_bp_num, input$area_col)
    })
    output$mtCoverage <- renderPlot({
      mitoCov_plot()
    })
    return(mitoCov_plot)
  })
}

#### Demo function of the module #### ----------
mtCoverage_demo <- function() {
  GEX5 <- read.table(file = "../example_data/GEX5Line.txt", header = T)

  ### descriptive
  grouped <- group_by(GEX5, position)
  df <- summarise(grouped, mean = mean(uniq_cell_umiN), sd = sd(uniq_cell_umiN))

  ui <- fluidPage(
    mtCoverage_ui("mtCoverage")
  )
  server <- function(input, output, session) {
    mtCoverage_server("mtCoverage", reactive({
      df
    }))
  }
  shinyApp(ui, server)
}

# TODO add documentation
