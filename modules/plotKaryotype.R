# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate karyotype graph with cytobands
#'
#' @param id A string.
#' @param df_geno A dataframe containing the genome information in the form chr start end.
#' @param df_cyto A dataframe containing the cytobands or region of interest localisations in the form chr start end + metadata.
#' @returns A Shiny module.
#' @examples
#' Karyotype_demo()
#### Library needed #### ----------
library(ggplot2)
library(shiny)
# BiocManager::install("karyoploteR")
library(karyoploteR)
library(berryFunctions)

#### Function needed to work #### ----------
# The original cytobands plotting functions needed to be adjusted to plot each time the basic genom with round corner
kpAddCytobandsCust <- function(kp, color.table = NULL,
                               color.schema = c("circos", "biovizbase", "only.centromeres"),
                               clipping = TRUE, ...) {
  if (missing(kp)) {
    stop("The parameter 'karyoplot' is required")
  }
  if (!methods::is(kp, "KaryoPlot")) {
    stop("'kp' must be a valid 'KaryoPlot' object")
  }

  mcols(kp$genome) <- data.frame(gieStain = "white", stringsAsFactors = FALSE)

  for (type in c("genome", "cytobands")) {
    cyto <- kp[[type]]
    if (!methods::is(cyto, "GRanges")) {
      stop("'cytobands' must be a GRanges object")
    }
    if (!("gieStain" %in% colnames(mcols(cyto)))) {
      warning("No 'gieStain' column found in cytobands. Using 'gpos50' (gray) for all of them")
      mcols(cyto) <- cbind(mcols(cyto), gieStain = "gpos50")
    }

    cyto <- filterChromosomes(cyto, keep.chr = kp$chromosomes)
    kp$beginKpPlot()
    on.exit(kp$endKpPlot())
    ccf <- kp$coord.change.function
    pp <- kp$plot.params
    mids <- kp$ideogram.mid
    color.table <- getCytobandColors(
      color.table = color.table,
      color.schema = color.schema
    )
    border <- ifelse("border" %in% names(color.table),
      color.table["border"], "black"
    )
    ybottom <- mids(as.character(seqnames(cyto))) - pp$ideogramheight / 2
    ytop <- mids(as.character(seqnames(cyto))) + pp$ideogramheight / 2
    xleft <- ccf(
      x = start(cyto), chr = as.character(seqnames(cyto)),
      data.panel = "ideogram"
    )$x
    xright <- ccf(
      x = end(cyto), chr = as.character(seqnames(cyto)),
      data.panel = "ideogram"
    )$x
    col <- color.table[as.character(cyto$gieStain)]

    if (kp$zoom == TRUE) {
      if (clipping == TRUE) {
        clip.xleft <- ccf(
          x = start(kp$plot.region),
          chr = as.character(seqnames(kp$plot.region)),
          data.panel = "ideogram"
        )$x
        clip.xright <- ccf(
          x = end(kp$plot.region),
          chr = as.character(seqnames(kp$plot.region)),
          data.panel = "ideogram"
        )$x
        clip.ybottom <- ybottom - 10
        clip.ytop <- ytop + 10
        graphics::clip(
          x1 = clip.xleft, x2 = clip.xright,
          y1 = clip.ybottom, y2 = clip.ytop
        )
      }
    }

    df_cyto <- data.frame(
      xleft = xleft, xright = xright,
      ybottom = ybottom, ytop = ytop,
      col = col, border = border
    )

    if (type == "genome") {
      for (i in 1:dim(df_cyto)[1]) {
        berryFunctions::roundedRect(
          xleft = df_cyto$xleft[i], ybottom = df_cyto$ybottom[i],
          xright = df_cyto$xright[i], ytop = df_cyto$ytop[i],
          bothsame = F, rounding = 0.5, corfactor = 30
        )
      }
    } else {
      graphics::rect(
        xleft = xleft, xright = xright, ybottom = ybottom,
        ytop = ytop, col = col, border = border, ...
      )
    }
    invisible(kp)
  }
}


plotKaryotypeShiny <- function(df, chrtoplot, cytobands = NULL, plottype = 1) {
  # Remove NA value from cytobands
  cytobands <- cytobands[!is.na(cytobands$start) & !is.na(cytobands$end) & !is.na(cytobands$chr), c("chr", "start", "end", "gieStain")]
  kp <- plotKaryotype(
    genome = df, chromosomes = chrtoplot,
    cytobands = cytobands, plot.type = plottype, ideogram.plotter = kpAddCytobandsCust
  )
}

#### UI function of the module #### ----------

Karyotype_ui <- function(id) {
  ns <- NS(id)

  tagList(
    plotOutput(ns("plot")),
    selectInput(ns("plottype"), "Choose plot type", choices = seq(1, 7)),
    uiOutput(ns("chrSelection"))
  )
}

#### Server function of the module #### ----------


Karyotype_server <- function(id, df_geno, df_cyto) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$chrSelection <- renderUI({
      chr <- df_geno()$chr
      selectInput(ns("chrSelected"),
        label = "Select chromosome to plot",
        choices = chr, selected = chr[1], multiple = TRUE
      )
    })

    karyotype_plot <- reactive({
      req(input$chrSelected)
      plotKaryotypeShiny(df_geno(), input$chrSelected, df_cyto(), input$plottype)
    })

    output$plot <- renderPlot({
      karyotype_plot()
    })

    return(karyotype_plot)
  })
}

#### Demo function of the module #### ----------

Karyotype_demo <- function() {
  library(plyr)
  df_geno <- read.table("../example_data/genome_list.txt", header = TRUE) # example data
  df_cytobands <- read.delim("../example_data/cytobands.txt", header = TRUE) # example data
  df_cytobands$gieStain <- revalue(df_cytobands$Type, c("MicroSat" = "stalk", "SNP" = "gpos100", "Genes" = "acen"))

  ui <- fluidPage(Karyotype_ui("Karyotype"))
  server <- function(input, output, session) {
    Karyotype_server("Karyotype", reactive({
      df_geno
    }), reactive({
      df_cytobands
    }))
  }
  shinyApp(ui, server)
}
