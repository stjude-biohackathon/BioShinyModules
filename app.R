library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly) #install.packages("plotly")
library(colourpicker) #install.packages("colourpicker")
library(ggpubr)
library(readxl)
library(viridis)
library(patchwork)



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


####----Volcano Functions----####

module_volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    colourInput(NS(id,"dotcol"),"Pick point color", value = "black"),
    radioButtons(NS(id,"stat"),"Choose statistic for plot",
                 choices=c("-Log10 P value" = "log_pval","-Log10 adjusted P value" = "log_adjpval"),
                 selected = "log_pval"),
    plotOutput(NS(id,"volcano"))
    
  )
}

module_volcano_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$volcano <- renderPlot({
      ggplot(df, aes(x=logFC, y=df[,input$stat])) + geom_point(col=input$dotcol)
    })
  })
}


####----Mitochondrial Control Functions----####

module_mito_ui <- function(id) {
  ns <- NS(id)
  tagList(
    colourInput(NS(id,"curve_area"),"Pick coverage color", value = "#69B3A2"),
    numericInput(NS(id,"bp_num"), "mtDNA bp value", value = 3243),
    numericInput(NS(id,"cov_threshold"),"Threshold", value = 100),
    plotOutput(NS(id,"mtCoverage"))
  )
}
module_mito_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$mtCoverage <- renderPlot({
      ggplot(combo, aes(x=position, y=mean, ymin=mean-sd, ymax=mean+sd)) +
        geom_line() + geom_ribbon(alpha=0.2) +
        geom_area(fill= input$curve_area, alpha=0.4) +
        geom_hline(yintercept=input$cov_threshold,linetype=2)+
        geom_point(aes(x=input$bp_num), shape = 95, size = 1, color = "red")+
        theme(
          panel.background = element_rect(fill="white", colour="white", size=0.5,
                                          linetype="solid", color="white"),
          panel.border = element_blank(),
          #plot.title = element_text(color="black", size=14, face="bold.italic", hjust=0),
          axis.title.x = element_text(color="black", size=14, face="plain"),
          axis.title.y = element_text(color="black", size=14, face="plain"),
          legend.key=element_rect(fill='white'),
          axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"),
          axis.text.x = element_text(face="bold", color="black",
                                     size=12, angle=0),
          axis.text.y = element_text(face="bold", color="black",
                                     size=12, angle=0))+
        xlab("Mitochondria Position (bp)") +
        ylab("Ave coverage") +
        #labs(title = "CM 5primeGEX, scRNAseq PER CELL", subtitle = "Std Dev")+
        scale_x_continuous(breaks=seq(0,16000,2000))+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        coord_cartesian(ylim = c(0,600000))
    })
  })
}





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
