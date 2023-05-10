# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: Yaseswini Neelamraju
# Author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate basic Manhattan plot and highlight certain snps
#'
#' @param id A string.
#' @param df A dataframe containing the snp position and their p-values.
#' @param chr_colors A palette of color for each chromosome.
#' @returns A Shiny module.
#' @examples
#' Manhattan_demo()
#### Library needed #### ----------
library(tidyverse)
library(shiny)
library(MetBrewer)
library(qqman)

#### Function needed to work #### ----------
# Module code for 'selectorUI' and 'selector'
selectorUI <- function(id) {
  ns <- NS(id)
  selectizeInput(inputId = ns('select'),
                 label = "Should we highlight snps",
                 choices = c("Yes", "No"),
                 selected="No")
}

selector <- function(input, output, session) {
  reactive(input$select)
}

#### UI function of the module #### ----------
Manhattan_ui<- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      uiOutput(ns("chr")),
      uiOutput(ns("chrposition")),
      uiOutput(ns("snp")),
      uiOutput(ns("pvalue")),
      selectorUI(ns('id1')),
      fileInput(ns("file_with_snps_to_highlight"), "Upload file with snps to highlight"),
      actionButton(ns("submit_click"), "Generate plot")
    ),
    mainPanel(
      plotOutput(ns("manhattanplot")),
      plotOutput(ns("manhattanplot_with_highlight"))
    )
  )
}

#### Server function of the module #### ----------
Manhattan_server <- function(id, df, chr_colors) {
  
  moduleServer(id, function(input, output, session) {
    
    ## Custom selection based on df
    ns <- NS(id)

    output$chr <- renderUI({
      selectInput(ns("col_chr"), "Select the column that best describes chr", choices = names(df()))
    })
    output$chrposition <- renderUI({
      selectInput(ns("col_chrposition"), "Select the column that best describes chr position", choices = names(df()))
    })
    output$snp <- renderUI({
      selectInput(ns("col_snp"), "Select the column that best describes snp position", choices = names(df()))
    })
    output$pvalue <- renderUI({
      selectInput(ns("col_pvalue"), "Select the column that best descriobes p-value", choices = names(df()))
    })
    
    ## Plotting function
    manhattan_plot <- eventReactive(input$submit_click, {
      manhattan(df(), chr = input$col_chr,
                    bp = input$col_chrposition,
                    snp = input$col_snp,
                    p = input$col_pvalue,
                    col = chr_colors,
                    highlight = snps_to_highlight() )
    })

    snps_to_highlight <- eventReactive(input$submit_click, {
      condition1 <- callModule(selector, 'id1')
      if (condition1() == "Yes"){
        req(input$file_with_snps_to_highlight)
        datTmp <- read_tsv(input$file_with_snps_to_highlight$datapath)[[1]]
        return(datTmp)
      }else{
        return(NULL)
      }
      
    })
    
    output$manhattanplot <- renderPlot({
      manhattan_plot()
    })
    
    return(manhattan_plot)
    
  })
}

#### Demo function of the module #### ----------
Manhattan_demo <- function() {
  chr_colors <- met.brewer(n = 23, name = "Cross")
  df<- gwasResults # Inbuilt dataset in the`qqman` package
  ui <- fluidPage(Manhattan_ui("x"))
  server <- function(input, output, session) {
    Manhattan_server("x", reactive({df}), chr_colors)
  }
  shinyApp(ui, server)
}