
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly) #install.packages("plotly")
library(colourpicker) #install.packages("colourpicker")
library(ggpubr)
library(readxl)
library(viridis)
library(patchwork)
library(interactivePlotModule)
####----Input Files----####

## Volcano File
df <- read.delim("L29_vitro_Control_vs_knockdown_diff.txt")
df <- mutate(df, log_pval = -log10(df$P.Value))
df <- mutate(df, log_adjpval = -log10(df$adj.P.Val))

#Mito File
GEX5<- read.table(file="GEX5Line.txt", header=T)
###descriptive
grouped <- group_by(GEX5, position)
combo<-summarise(grouped, mean=mean(uniq_cell_umiN), sd=sd(uniq_cell_umiN))


#Shiny App
ui <-
navbarPage("Shiny Module Demos",
           tabPanel(
             "Volcano",
             fluidPage(
               mainPanel(
                 module_volcano_ui("volcano")
               )
             )
           ),
           tabPanel(
             "Mito Coverage",
             fluidPage(
               mainPanel(
                 module_mito_ui("mtCoverage")
               )
             )
           )
)

server <- function(input, output, session) {
  module_volcano_server("volcano")
  module_mito_server("mtCoverage")
}
shinyApp(ui, server)

