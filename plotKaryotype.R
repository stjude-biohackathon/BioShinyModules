# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate karyotype graph
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
#BiocManager::install("karyoploteR")
library(karyoploteR)

#### Function needed to work #### ----------
# TODO write in this section the different function use by the module

kpAddCytobandsCust <- function (karyoplot, color.table = NULL, color.schema = c("circos", 
                                                          "biovizbase", "only.centromeres"), clipping = TRUE,
          ...) {
  if (missing(karyoplot))
    stop("The parameter 'karyoplot' is required")
  if (!methods::is(karyoplot, "KaryoPlot"))
    stop("'karyoplot' must be a valid 'KaryoPlot' object")
  if (!is.null(karyoplot$cytobands) && length(karyoplot$cytobands) >
      0) {
    cyto <- karyoplot$cytobands
    print(cyto)
  }
  else {
    cyto <- karyoplot$genome
    mcols(cyto) <- data.frame(name = seqnames(cyto), gieStain = "gpos50",
                              stringsAsFactors = FALSE)
  }
  if (!methods::is(cyto, "GRanges")) 
    stop("'cytobands' must be a GRanges object")
  if (!("gieStain" %in% colnames(mcols(cyto)))) {
    warning("No 'gieStain' column found in cytobands. Using 'gpos50' (gray) for all of them")
    mcols(cyto) <- cbind(mcols(cyto), gieStain = "gpos50")
  }
  cyto <- filterChromosomes(cyto, keep.chr = karyoplot$chromosomes)
  karyoplot$beginKpPlot()
  on.exit(karyoplot$endKpPlot())
  ccf <- karyoplot$coord.change.function
  pp <- karyoplot$plot.params
  mids <- karyoplot$ideogram.mid
  color.table <- getCytobandColors(color.table = color.table, 
                                   color.schema = color.schema)
  border <- ifelse("border" %in% names(color.table), 
                   color.table["border"], "black")
  ybottom <- mids(as.character(seqnames(cyto))) - pp$ideogramheight/2
  ytop <- mids(as.character(seqnames(cyto))) + pp$ideogramheight/2
  xleft <- ccf(x = start(cyto), chr = as.character(seqnames(cyto)), 
               data.panel = "ideogram")$x
  xright <- ccf(x = end(cyto), chr = as.character(seqnames(cyto)), 
                data.panel = "ideogram")$x
  col <- color.table[as.character(cyto$gieStain)]
  if (karyoplot$zoom == TRUE) {
    if (clipping == TRUE) {
      clip.xleft <- ccf(x = start(karyoplot$plot.region), 
                        chr = as.character(seqnames(karyoplot$plot.region)), 
                        data.panel = "ideogram")$x
      clip.xright <- ccf(x = end(karyoplot$plot.region), 
                         chr = as.character(seqnames(karyoplot$plot.region)), 
                         data.panel = "ideogram")$x
      clip.ybottom <- ybottom - 10
      clip.ytop <- ytop + 10
      graphics::clip(x1 = clip.xleft, x2 = clip.xright, 
                     y1 = clip.ybottom, y2 = clip.ytop)
    }
  }
  graphics::rect(xleft = xleft, xright = xright, ybottom = ybottom, 
                 ytop = ytop, col = col, border = border, ...)
  invisible(karyoplot)
}


cytobands = df_cytobands
chrtoplot = 1


plotKaryotypeCust <- function(df, chrtoplot, cytobands=NULL, plottype=1) {
  print(df)
  print(chrtoplot)
  # Remove NA value from cytobands
  cytobands = cytobands[!is.na(cytobands$start) & !is.na(cytobands$end) & !is.na(cytobands$chr),c("chr","start","end","gieStain")]
  kp <- plotKaryotype(genome = df, chromosomes = chrtoplot,
                cytobands = cytobands, plot.type = plottype,ideogram.plotter=kpAddCytobandsCust)
}


#### UI function of the module #### ----------
# TODO Add here the UI function of the module

Karyotype_ui <- function(id) {

  ns <- NS(id)

  tagList(
    plotOutput(ns("plot")),
    selectInput(ns("plottype"),"Choose plot type", choices=seq(1,7)),
    uiOutput(ns("chrSelection"))
  )

}

#### Server function of the module #### ----------
# TODO Add here the server function of the module

Karyotype_server <- function(id, df_geno, df_cyto) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$chrSelection <- renderUI({
      chr <-df_geno()$chr
      selectInput(ns("chrSelected"),
                  label = "Select chromosome to plot",
                  choices = chr, selected = chr[1], multiple = TRUE
      )
    })

    karyotype_plot <- reactive({
      req(input$chrSelected)
      plotKaryotypeCust(df_geno(), input$chrSelected, df_cyto(), input$plottype )
    })

    output$plot <- renderPlot({
      karyotype_plot()
    })

    return(karyotype_plot)
  })
}

#### Demo function of the module #### ----------
# TODO Add here the demo function of the module

Karyotype_demo <- function() {
  library(plyr)
  df_geno <- read.table("./example_data/genome_list.txt", header = TRUE) # example data
  df_cytobands <- read.delim("./example_data/cytobands.txt", header = TRUE) # example data
  df_cytobands$gieStain <- revalue(df_cytobands$Type,c("MicroSat" = "stalk","SNP" = "gpos100","Genes" = "acen"))

  ui <- fluidPage(Karyotype_ui("Karyotype"))
  server <- function(input, output, session) {
    Karyotype_server("Karyotype", reactive({df_geno}), reactive({df_cytobands}))
  }
  shinyApp(ui, server)
}

Karyotype_demo()

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation at the top of the module to match the necessary parameters of the server.