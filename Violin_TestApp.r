wd = ("\\\\ad.uws.edu.au/dfshare/HomeBNK$/90951007/My Documents")
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly) #install.packages("plotly")
library(colourpicker) #install.packages("colourpicker")

df <- read.delim('L29_vitro_Control_vs_knockdown_diff.txt')
df <- mutate(df, log_pval = -log10(df$P.Value))
df <- mutate(df, log_adjpval = -log10(df$adj.P.Val))


module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    colourInput(NS(id,"dotcol"),"Pick point color", value = "#ED6262"),
    radioButtons(NS(id,"stat"),"Choose statistic for plot", choices=c("-Log10 P value" = "log_pval","-Log10 adjusted P value" = "log_adjpval"), selected = "log_pval"),
    plotOutput(NS(id,"violin"))
  )
}

module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$violin <- renderPlot({
      ggplot(df, aes(x=logFC, y=df[,input$stat])) + geom_violin(col=input$dotcol)
    })
  })
}


ViolinApp <- function() {
  ui <- fluidPage (
    module_ui("violin")
  )
  server <- function(input, output, session) {
    module_server("violin")
  }
  shinyApp(ui, server)  
}

ViolinApp()
