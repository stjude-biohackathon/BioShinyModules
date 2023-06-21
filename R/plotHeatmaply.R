# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (ytqiuhaowen@gmail.com)


# Documentation
#' R Shiny module to generate a heatmap/distance/correlation using `heatmaply`
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' plotHeatmaply_demo()
#' plotHeatmaply_demo_2()
#### Library needed #### ----------
usethis::use_package("shiny")
usethis::use_package("heatmaply")
usethis::use_package("RColorBrewer")
usethis::use_package("stats")
usethis::use_package("grDevices")

#### Function needed to work #### ----------
#' plot Heatmap
#'
#' @param df A data frame
#' @param type Character to choose from one of three types: "heatmap",
#' "correlation", "distance".
#' @param direction Character taking the following values: "col" or "row" for
#' correlation or distance types plot.
#' @param select_dist Method used to compute the distance (dissimilarity)
#' between both rows and columns that can take the following values :
#' "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski". To be
#' use with type distance.
#' @param select_corr Correlation analysis model to use that can take the
#' following values : "pearson", "kendall", "spearman" to be use with type
#' correlation.
#' @param select_scale Character indicating if the values should be centered and
#' scaled in either the row direction or the column direction, or none.
#' The default is "none".
#' @param pal Color palette to use. Obtained from
#' `RColorBrewer::brewer.pal.info`
#' @param ... Other parameters to be passed to  `heatmaply()`
#'
#' @return A heatmap plot generated with pheatmap
#' @export plotHeatmaply
plotHeatmaply <- function(df, type, direction = NULL,
                          select_dist = NULL,
                          select_corr = NULL,
                          select_scale = "none",
                          pal,
                          ...) {
  df <- data.frame(df)

  if (type == "heatmap") {
    message(paste(
      "When 'type' == 'heatmap', 'direction', 'select_dist',",
      "'select_corr' are set at NULL. \n "
    ))
    mat <- as.matrix(df)
  } else if (type == "correlation") {
    message(paste(
      "When 'type' == 'correlation', 'select_scale' is set at",
      "'none' and 'select_dist' is set at NULL. \n "
    ))

    if (direction == "row") {
      mat <- stats::cor(t(df), method = select_corr)
      select_scale <- "none"
    } else if (direction == "col") {
      mat <- stats::cor(df, method = select_corr)
      select_scale <- "none"
    }
  } else if (type == "distance") {
    message(paste(
      "When 'type' == 'distance', 'select_scale' is set at 'none',",
      "and 'select_corr' is set at NULL. \n"
    ))

    if (direction == "row") {
      mat <- as.matrix(stats::dist(df, method = select_dist))
      select_scale <- "none"
    } else if (direction == "col") {
      mat <- as.matrix(stats::dist(t(df), method = select_dist))
      select_scale <- "none"
    }
  }


  # select colors for hm and anno
  palettes <- RColorBrewer::brewer.pal.info
  hm_color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = palettes$maxcolors[rownames(palettes) == pal],
    name = pal))(256)

  # draw hm
  heatmaply::heatmaply(mat,
    colors = hm_color,
    scale = select_scale,
    dist_method = NULL,
    ...
  )
}

palettes <- RColorBrewer::brewer.pal.info
hm_palettes <- rownames(palettes[palettes$category == "div", ])
dist_palettes <- rownames(palettes[palettes$category == "seq", ])


#### UI function of the module #### ----------
# TODO add doc
plotHeatmaply_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("plot_type"), "Select plot type",
      choices = c("heatmap", "distance", "correlation"), selected = "heatmap"
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'heatmap'", ns("plot_type")),
      # "input.plot_type == 'heatmap'",
      selectInput(ns("hm_palette"), "Choose color palette",
        choices = hm_palettes, selected = "RdYlBu"
      ),
      selectInput(ns("scale"), "Select scale",
        choices = c("none", "row", "column"), selected = "row"
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'distance'", ns("plot_type")),
      # "input.plot_type == 'distance'",
      selectInput(ns("dist_palette"), "Choose color palette",
        choices = dist_palettes, selected = "YlGnBu"
      ),
      selectInput(ns("dist_direction"), "Select direction",
        choices = c("col", "row"), selected = "col"
      ),
      selectInput(ns("dist"), "Select distance metric",
        choices = c(
          "euclidean", "maximum", "manhattan",
          "canberra", "binary", "minkowski"
        ),
        selected = "euclidean"
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'correlation'", ns("plot_type")),
      # "input.plot_type == 'correlation'",
      selectInput(ns("corr_palette"), "Choose color palette",
        choices = hm_palettes, selected = "RdBu"
      ),
      selectInput(ns("corr_direction"), "Select direction",
        choices = c("col", "row"), selected = "col"
      ),
      selectInput(ns("corr"), "Select correlation method",
        choices = c("pearson", "kendall", "spearman"),
        selected = "pearson"
      )
    ),
    selectInput(ns("select_hclust"), "Select hclust method",
      choices = c(
        "ward.D", "ward.D2", "single", "complete", "average",
        "mcquitty", "median", "centroid"
      ),
      selected = "complete"
    ),
    checkboxInput(ns("showRowDendrogram"), "Show row dendrogram", value = TRUE),
    checkboxInput(ns("showColDendrogram"), "Show col dendrogram", value = TRUE),
    checkboxInput(ns("showRowLabel"), "Show row label", value = TRUE),
    checkboxInput(ns("showColLabel"), "Show col label", value = TRUE),
    sliderInput(ns("fontsize_col"), "Select font size for column label",
      min = 5, max = 15, value = 8
    ),
    sliderInput(ns("fontsize_row"), "Select font size for row label",
      min = 5, max = 15, value = 8
    ),
    textInput(ns("title"), "Title of the graph", value = ""),
    actionButton(ns("click_submit"), label = "Submit"),
    plotlyOutput(ns("plot"))
  )
}

#### Server function of the module #### ----------
# TODO add doc
plotHeatmaply_server <- function(id, df) {
  stopifnot(is.reactive(df))

  moduleServer(id, function(input, output, session) {
    Heatmap_plot <- eventReactive(input$click_submit, {
      scale <- NULL
      direction <- NULL
      corr <- NULL
      dist <- NULL
      pal <- NULL
      if (input$plot_type == "heatmap") {
        scale <- input$scale
        pal <- input$hm_palette
      } else if (input$plot_type == "distance") {
        direction <- input$dist_direction
        dist <- input$dist
        pal <- input$dist_palette
      } else if (input$plot_type == "correlation") {
        direction <- input$corr_direction
        corr <- input$corr
        pal <- input$corr_palette
      } else {
          message("Error")
      }
      plotHeatmaply(df(),
        type = input$plot_type,
        direction = direction,
        select_dist = dist,
        select_corr = corr,
        select_scale = scale,
        pal = pal,
        hclust_method = input$select_hclust,
        show_dendrogram = c(input$showRowDendrogram,input$showColDendrogram),
        showticklabels = c(input$showRowLabel,input$showColLabel),
        fontsize_col = input$fontsize_col,
        fontsize_row = input$fontsize_row,
        main = input$title
      )
    })
    output$plot <- renderPlotly({
      Heatmap_plot()
    })
    return(Heatmap_plot)
  })
}

#### Demo function of the module #### ----------
# TODO add doc
plotHeatmap_demo <- function() {
  load("../data-raw/MS_1.rda")
  df <- ic_bps

  ui <- fluidPage(
    plotHeatmaply_ui("plotHeatmaply")
  )

  server <- function(input, output, session) {
    my_plot <- plotHeatmaply_server(
      "plotHeatmaply",
      reactive({
        df
      })
    )
  }
  shinyApp(ui, server)
}

# TODO add doc
plotHeatmap_demo_2 <- function() {
  load("../data-raw/MS_1.rda")
  source("exportPlot.R")
  df <- ic_bps

  ui <- fluidPage(
    plotHeatmaply_ui("plotHeatmaply"),
    exportPlot_ui("saveheatmap")
  )

  server <- function(input, output, session) {
    my_plot <- plotHeatmaply_server(
      "plotHeatmaply",
      reactive({
        df
      })
    )
    exportPlot_server("saveheatmap", my_plot, is_plotly = TRUE)
  }
  shinyApp(ui, server)
}
