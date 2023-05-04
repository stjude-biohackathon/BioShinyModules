# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Lawryn Kasper (lawryn.kasper@stjude.org)

# Documentation
# R Shiny module to generate a scatterplot with hover text
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(colourpicker)

library(shiny)

df <- read.delim("L29_vitro_Control_vs_knockdown_diff.txt") # read in file - this needs to be generalized to allow user to read in any file
df <- mutate(df, log_pval = -log10(df$P.Value))
df <- mutate(df, log_adjpval = -log10(df$adj.P.Val))



scatter_ui <- function(id) {
  tagList(
  colourInput(NS(id,"dotcol"),"Pick point color", value = "black"),
  selectInput(NS(id,"x_columns"), "Select column to plot on x axis", choices=colnames(df), selected = "logFC"),
  selectInput(NS(id,"y_columns"), "Select column to plot on y axis", choices=colnames(df), selected = "log_pval"),
  textInput(NS(id,"title"), "Enter title text", ""),
  plotlyOutput(NS(id,"scatter"))
)
  }




scatter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$scatter <- renderPlotly({
      
      ggplot(df, aes(x=df[,input$x_columns], y=df[,input$y_columns], text=df[,1])) + geom_point(col=input$dotcol) + 
        ggtitle(input$title) + xlab(input$x_columns) + ylab(input$y_columns)
    
    })
  })
}


ScatterApp <- function() {
  ui <- fluidPage(
    scatter_ui("scatter")
  )
  server <- function(input, output, session) {
    scatter_server("scatter")
  }
  shinyApp(ui, server)  
}

ScatterApp()
