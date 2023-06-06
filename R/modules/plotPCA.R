# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Lawryn Kasper (lawryn.kasper@stjude.org)

# Documentation
#' R Shiny module to generate PCA plot with choice of 2D or 3D
#'
#' @param id A string.
#' @param df A dataframe containing the value for each sample (column)
#' for all the features (rows).
#' @param sample_anno A data frame containing the sample informations.
#' @param sample_anno_col The column name of sample category in sample_anno.
#' @returns A Shiny module.
#' @examples
#' plotPCA_demo()
#### Library needed #### ----------
library(shiny)
library(ggplot2)
library(pcaMethods)
library(plotly)
library(RColorBrewer)

#### Function needed to work #### ----------
#' plot PCA 2D
#'
#' @param data A data frame with feature (row) by sample (column).
#' @param sample_anno A data frame containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param title Character. Plot title
#'
#' @return GGplot PCA
ggplotly_2Dpca <- function(df, sample_anno, sample_anno_col, PC1, PC2, pal, title) {
    require(pcaMethods)
    require(ggplot2)

    data <- as.matrix(df)
    class(data) <- "numeric"

    labels <- as.matrix(sample_anno[sample_anno_col])

    pc1 <- pcaMethods::pca(t(data), nPcs = 3, scale = "pareto")
    pc1merged <- merge(cbind(labels, t(data)),
        pcaMethods::scores(pc1),
        by = 0
    )
    p <- ggplot(pc1merged, aes(!!sym(PC1), !!sym(PC2), colour = !!sym(sample_anno_col))) +
        geom_point() +
        scale_color_brewer(palette = pal) +
        stat_ellipse() +
        xlab(paste(PC1, round((pc1@R2[PC1] * 100), digits = 1), "% of the variance")) +
        ylab(paste(PC2, round((pc1@R2[PC2] * 100), digits = 1), "% of the variance")) +
        ggtitle(label = title)
    ggplotly(p)
}

#' Plot PCA 3D
#'
#' @param data A data frame with feature (row) by sample (column).
#' @param sample_anno A data frame containing sample metadata
#' @param sample_anno_col Character. Sample annotation column in `sample_anno`
#' @param title Character. Plot title
#'
#' @return Interactive PCA plot
ggplotly_3Dpca <- function(df, sample_anno, sample_anno_col, pal, title) {
    data <- as.matrix(df)
    class(data) <- "numeric"

    labels <- as.matrix(sample_anno[sample_anno_col])

    pc1 <- pcaMethods::pca(t(data), nPcs = 3, scale = "pareto")
    pc1merged <- merge(cbind(labels, t(data)),
        pcaMethods::scores(pc1),
        by = 0
    )

    plot_ly(pc1merged,
        x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d",
        color = sample_anno$sampleLabel, colors = pal,
        text = sample_anno$sampleName, hoverinfo = "text"
    ) %>%
        layout(title = title, scene = list(
            xaxis = list(title = "PC1"),
            yaxis = list(title = "PC2"),
            zaxis = list(title = "PC3")
        ))
}

palettes <- rownames(RColorBrewer::brewer.pal.info)

#### UI function of the module #### ----------
plotPCA_ui <- function(id) {
    ns <- NS(id)
    tagList(
        selectInput(ns("palette"), "Choose color palette", choices = palettes, selected = "Dark2"),
        textInput(ns("title"), "Title graph", value = ""),
        radioButtons(ns("dim_select"), "Select 2D or 3D",
            choices = c("2D", "3D"), selected = "2D"
        ),
        selectInput(ns("PC1"), "Select first PC",
            choices = list("PC1", "PC2", "PC3"),
            selected = "PC1"
        ),
        selectInput(ns("PC2"), "Select second PC",
            choices = list("PC1", "PC2", "PC3"),
            selected = "PC2"
        ),
        plotlyOutput(ns("plot"))
    )
}

#### Server function of the module #### ----------
plotPCA_server <- function(id, df, sample_anno, sample_anno_col) {
    moduleServer(id, function(input, output, session) {
        stopifnot(is.reactive(df))
        stopifnot(is.reactive(sample_anno))
        stopifnot(is.reactive(sample_anno_col))

        PCA_plot <- reactive({
            if (input$dim_select == "2D") {
                ggplotly_2Dpca(
                    df(), sample_anno(), sample_anno_col(),
                    input$PC1, input$PC2,
                    input$palette, input$title
                )
            } else {
                ggplotly_3Dpca(df(), sample_anno(), sample_anno_col(), input$palette, input$title)
            }
        })

        output$plot <- renderPlotly({
            PCA_plot()
        })

        return(PCA_plot)
    })
}

#### Demo function of the module #### ----------

plotPCA_demo <- function() {
    load("../../data-raw/MS_2.rda")
    df <- df
    sample_anno <- sample_meta
    sample_anno_col <- "sampleLabel"

    ui <- fluidPage(plotPCA_ui("x"))

    server <- function(input, output, session) {
        my_plot <- plotPCA_server("x", reactive({
            df
        }), reactive({
            sample_anno
        }), reactive({
            sample_anno_col
        }))
    }
    shinyApp(ui, server)
}

# to-do: download using ggsave_both()
plotPCA_demo_2 <- function() {
    source("ggsave_both.R")
    load("../../data-raw/MS_2.rda")
    df <- df
    sample_anno <- sample_meta
    sample_anno_col <- "sampleLabel"

    ui <- fluidPage(
        plotPCA_ui("x"),
        ggsaveBoth_ui("savepca")
    )

    server <- function(input, output, session) {
        my_plot <- plotPCA_server("x", reactive({
            df
        }), reactive({
            sample_anno
        }), reactive({
            sample_anno_col
        }))
        ggsaveBoth_server("savepca", my_plot, is_plotly = TRUE)
    }
    shinyApp(ui, server)
}
