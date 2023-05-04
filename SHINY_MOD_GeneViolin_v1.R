setwd("C:/Users/kmccastl/Desktop/BioHackathon")
library(shiny)
library(ggplot2)
#library(tidyverse)
library(plotly)
library(colourpicker)
library(dplyr)
library(ggpubr)
library(readxl)
library(viridis)
library(patchwork)
library(cowplot) 

#file1
expr<- read.delim(file="TCGA_CHOL_Expression_PatientID.txt", header=T, check.names=F) 
meta<- read.delim(file="TCGA_CHOL_Clinical_PatientID.txt", header=T, check.names=F) 
GeneList <- expr[,1]
ColumnList <- colnames(meta)


module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(NS(id,"geneSelect"),"Type Gene:", value="MYC"),
    selectInput(NS(id,"metaSelect"),"Select Column:", choices = ColumnList),
    plotOutput(NS(id,"Violin"))
   )
}

module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$Violin <- renderPlot({
      
      GeneSel <- toupper(input$geneSelect)
      ColSel <- input$metaSelect
      expr_sub <- expr[which(expr[,1] == GeneSel),]
      expr_sub <- as.data.frame(t(expr_sub))
      colnames(expr_sub)[1] <- GeneSel
      expr_sub$PatientID <- rownames(expr_sub)
      rownames(expr_sub) <- NULL
      expr_sub <- expr_sub[-1,]
      expr_sub[,1] <- as.numeric(expr_sub[,1])
      metaMerge <- merge(expr_sub,meta)
      metaMerge[,ColSel] <- as.factor(metaMerge[,ColSel])
      #colnames(metaMerge)[c(2,3)] <- c("Gene","MetaColumn")
      
      ggplot(metaMerge,aes(metaMerge[,ColSel],metaMerge[,GeneSel],fill=metaMerge[,ColSel])) +
        geom_violin()+
        #geom_jitter(width = 0.3)+
        geom_boxplot(width=0.1,fill = "white")+
        scale_fill_viridis(discrete = TRUE, option = "plasma",direction=-1,alpha=0.6)+
        theme(
          panel.background = element_rect(fill="white", colour="white", linewidth=0.5, 
                                          linetype="solid", color="grey73"),
          panel.border = element_blank(),
          
          #plot.title = element_text(hjust=0, color="black", size=14, face="bold.italic"),
          axis.title.x = element_text(color="black", size=14, face="plain"),
          axis.title.y = element_text(color="black", size=14, face="plain"),
          
          #legend.key=element_rect(fill='white'),
          legend.position="none",
          
          axis.line = element_line(linewidth = 0.5, linetype = "solid",colour = "black"),
          axis.text.x = element_text(face="bold", color="black", 
                                     size=12, angle=45, hjust=1),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=12, angle=0))+
        xlab(input$metaSelect)+ylab(input$geneSelect)

    })
  })
}

GeneViolinApp <- function() {
  ui <- fluidPage(
    module_ui("Violin")
  )
  server <- function(input, output, session) {
    module_server("Violin")
  }
  shinyApp(ui, server)  
}

GeneViolinApp()



