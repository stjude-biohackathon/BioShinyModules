# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Christy LaFlamme (christy.laflamme@stjude.org)
# the plot created is adapated from a script written by Paras Garg

# Documentation
#' R Shiny module to generate volcano plot
#'
#' @param data A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
# TODO Add here all the library needed for the module (ggplot2, tidyr, ...)
library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)

#### Function needed to work #### ----------
#' Differentially methylated region (DMR) plot 
#'
#' @param data A data frame of B-values from methylation array
#' @param coordinates Genomic coordinates to plot (e.g. chr16:17562418-17565190)
#' @param chrCol The name of the column containing the chromosome (e.g. chr16)
#' @param startCol The name of the column containing the starting/beginning CpG coordinate (e.g. 17562418)
#' @param endCol The name of the column containing the stopping/ending CpG coordinate (e.g. 17565190)
#' @param intSamples The names of the samples of interest to highlight (e.g. 203866380012_R06C01, 204873630063_R08C01)
#' @param title The title of the plot (if desired)
#' @return plot
#' @export pdf

#### plotting function of the module #### ----------

plotDMR <- function(data, coordinates, chrCol, startCol, endCol, intSamples, title = ""){
  
  # get the sample names (column names of data matrix satisfying the following regular expression)
  samples <- str_extract(colnames(data), "............_R0.C0.") # recognizes the sentrix ID format
  samples <- samples[!is.na(samples)]
  
  # string split the given coordinates to pull the chromosome, start, and stop information
  coordinates <- gsub(",", "", coordinates) # remove commas
  chr <- strsplit(coordinates, split = ":")[[1]][1]
  start <- strsplit(strsplit(coordinates, split = ":")[[1]][2], split = "-")[[1]][1]
  stop <- strsplit(strsplit(coordinates, split = ":")[[1]][2], split = "-")[[1]][2]
  
  # acquire the row indexes of the data matrix that correspond to the start and stop coordinates
  
  # cut the data to match with chromosome specificity of the given coordinates
  data = data[data[,data[[chrCol]] == chr],]
  
  # this is with the minimum distance from the given coordiantes to the data +1 / -1 probe 
  startPos <- which(abs(data[[startCol]]-as.numeric(start))==min(abs(data[[startCol]]-as.numeric(start)))) - 1
  endPos <- which(abs(data[[endCol]]-as.numeric(stop))==min(abs(data[[endCol]]-as.numeric(stop)))) + 1
  
  # make sure that interest sample list is separated out from commas and remove spaces
  intSamples <- gsub(" ", "", strsplit(intSamples, ",")[[1]])

  # pre-processing of data
  data = data %>% slice(startPos:endPos) %>%
    melt(id.vars = setdiff(colnames(data), samples), variable.name = "sampleID", value.name = "Beta") %>%
    mutate(status = ifelse(sampleID %in% intSamples, "Sample-of-Interest", "normal"))
  
  # create the line plot
  g = ggplot(data =  data, aes_string(x=startCol, y="Beta")) +
    geom_line(aes(group=sampleID,size=status, linetype = status, colour = status)) +
    scale_colour_manual(values = c("Sample-of-Interest" = "red","normal" = "black")) +
    scale_size_manual(values = c("Sample-of-Interest"=0.9, "normal" = 0.2 )) +
    scale_linetype_manual(values = c("Sample-of-Interest"="solid", "normal" = "dashed" )) +
    coord_cartesian(ylim = c(0,1)) +
    xlab(paste0("Genomic Position at ",chr)) +
    ylab("Methylation Value") +
    labs(title = title) +
    theme_bw()
  g
  
}

#### UI function of the module #### ----------
module_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    titlePanel("Interactive DMR plot"),
    textInput(NS(id,"coordinates"), "What genomic coordinates would you like to view? Please use format chrX:start-stop ", value = "chr16:17562418-17565190"),
    textInput(NS(id,"chrCol"), "What is the name of the column with the chromosome information? (e.g. chr16)", value = "CpG_chrm"),
    textInput(NS(id,"startCol"), "What is the name of the column with the start positions? (e.g. 17562418)", value = "CpG_beg"),
    textInput(NS(id,"endCol"), "What is the name of the column with the end positions? (e.g. 17565190)", value = "CpG_end"),
    textInput(NS(id,"intSamples"), "Are there samples of interest to highlight (e.g. 203866380012_R06C01, 204873630063_R08C01)?", value = "203866380012_R06C01, 204873630063_R08C01"),
    textInput(NS(id,"title"), "What is the title of the plot?", value = "DMR plot at XYLT1"),
    verbatimTextOutput(NS(id,"region")),
    plotOutput(NS(id,"plot"))
  )
}

#### Server function of the module #### ----------
module_server <- function(id) {
  moduleServer(id, function(input, output, session){
    output$region <- renderText(paste0("The region you have selected is: ", input$coordinates, " and the samples of interest are: ", input$intSamples))
    
    output$plot <- renderPlot({
      plotDMR(data, input$coordinates, input$chrCol, input$startCol, input$endCol, input$intSamples, title = input$title)
    })
  })
}

#### Demo function of the module #### ----------
data <- fread("./autosomes.beta.txt.sorted") # example data

DMRplot <- function() {
  ui <- fluidPage(
    module_ui("dmrplot")
  )
  server <- function(input, output, session) {
    module_server("dmrplot")
  }
  shinyApp(ui, server)  
}

DMRplot()


