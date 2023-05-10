# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: @KMcC73
# author: Louis Le Nézet (louislenezet@gmail.fr)

# Documentation
#' R Shiny module to generate a violin plot per gene
#'
#' @param id A string.
#' @param df_expr Dataframe with the expression information.
#' @param df_meta Dataframe with the meta data information per sample.
#' @returns A Shiny module.
#' @examples
#' GeneViolin_demo()
#### Library needed #### ----------
library(shiny)
library(ggplot2)
library(plotly)
library(colourpicker)
library(dplyr)
library(ggpubr)
library(readxl)
library(viridis)
library(patchwork)
library(cowplot)

#### Function needed to work #### ----------
#' plot Violin
#'
#' @param df_expr Dataframe with the expression information.
#' @param df_meta Dataframe with the meta data information per sample.
#' @param geneSel String defining the gene to look at.
#' @param colSel Factor defining the variable to compare to
#'
#' @return Violin plot for the gene selected for each level of the column selected

GeneViolin <- function(df_expr, df_meta, geneSel, colSel) {
  df_expr_sub <- df_expr[which(df_expr[, 1] == geneSel), ]
  df_expr_sub <- as.data.frame(t(df_expr_sub))
  colnames(df_expr_sub)[1] <- geneSel

  df_expr_sub$PatientID <- rownames(df_expr_sub)
  rownames(df_expr_sub) <- NULL
  df_expr_sub <- df_expr_sub[-1, ]
  df_expr_sub[, 1] <- as.numeric(df_expr_sub[, 1])

  metaMerge <- merge(df_expr_sub, df_meta)
  metaMerge[, colSel] <- as.factor(metaMerge[, colSel])
  # colnames(metaMerge)[c(2,3)] <- c("Gene","MetaColumn")

  # Create plot
  ggplot(metaMerge, aes(metaMerge[, colSel], metaMerge[, geneSel], fill = metaMerge[, colSel])) +
    geom_violin() +
    # geom_jitter(width = 0.3)+
    geom_boxplot(width = 0.1, fill = "white") +
    scale_fill_viridis(discrete = TRUE, option = "plasma", direction = -1, alpha = 0.6) +
    theme(
      panel.background = element_rect(
        fill = "white", colour = "white", linewidth = 0.5,
        linetype = "solid", color = "grey73"
      ),
      panel.border = element_blank(),

      # plot.title = element_text(hjust=0, color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color = "black", size = 14, face = "plain"),
      axis.title.y = element_text(color = "black", size = 14, face = "plain"),

      # legend.key=element_rect(fill='white'),
      legend.position = "none",
      axis.line = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
      axis.text.x = element_text(
        face = "bold", color = "black",
        size = 12, angle = 45, hjust = 1
      ),
      axis.text.y = element_text(
        face = "bold", color = "black",
        size = 12, angle = 0
      )
    ) +
    xlab(colSel) +
    ylab(geneSel)
}


#### UI function of the module #### ----------
GeneViolin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("geneSelected"), "Select Gene:", value = "MYC"),
    uiOutput(ns("metaSelect")),
    plotOutput(ns("Violin"))
  )
}

#### Server function of the module #### ----------
GeneViolin_server <- function(id, df_expr, df_meta) {
  moduleServer(id, function(input, output, session) {
    GeneViolin_plot <- reactive({
      plotGeneViolin(df_expr, df_meta, input$geneSelected, input$metaSelected)
    })

    ns <- NS(id)
    output$metaSelect <- renderUI({
      selectInput(ns("metaSelected"), "Select Column:", choices = colnames(df_meta))
    })

    output$Violin <- renderPlot({
      GeneViolin_plot()
    })
    return(GeneViolin_plot)
  })
}

#### Demo function of the module #### ----------
GeneViolin_demo <- function() {
  df_expr <- read.delim(file = "../example_data/TCGA_CHOL_Expression_PatientID.txt", header = T, check.names = F)
  df_meta <- read.delim(file = "../example_data/TCGA_CHOL_Clinical_PatientID.txt", header = T, check.names = F)

  ui <- fluidPage(
    GeneViolin_ui("Violin")
  )
  server <- function(input, output, session) {
    GeneViolin_server("Violin", df_expr, df_meta)
  }
  shinyApp(ui, server)
}
