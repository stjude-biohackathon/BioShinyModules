# Author : Yaseswini Neelamraju 
## Functionalities : 
## Draw basic manhattan plot 
## Highlight certain snps 

library( tidyverse )
library( shiny )
library( MetBrewer )
library( qqman )

chr_colors = met.brewer( n = 23 , name = "Cross" )

inputData = gwasResults # Inbuilt dataset in the`qqman` package



manhattanPlotUI <- fluidPage(

    sidebarPanel(
      #selectInput( NS( id , "GWAS_inputFile"), "Enter the GWAS file" ),
      selectInput( "col_chr" , "Select the column that best describes chr"  , choices = names(inputData) ),
      selectInput( "col_chrposition" , "Select the column that best describes chr position" , choices = names(inputData) ),
      selectInput( "col_snp" , "Select the column that best describes snp position" , choices = names(inputData) ) ,
      selectInput( "col_pvalue" , "Select the column that best descriobes p-value" , choices = names(inputData) ),
      radioButtons( "highlight_snp" , label = "Should we highlight snps" , choices = c("Yes","No") ),
      conditionalPanel( condition = "input.highlight_snp == 'Yes'",
                        fileInput( "file_with_snps_to_highlight" , "Upload file with snps to highlight"  ) ),
      
      actionButton( "submit_click" , "Generate plot" )
    ),
    mainPanel(
      plotOutput( "manhattanplot" ),
      plotOutput( "manhattanplot_with_highlight")
    )
)

manhattanPlotServer <- function( input , output , session){
  
                                manhattanplot_obj <- eventReactive( input$submit_click , {
                                                                                        
                                                                        manhattan( inputData , 
                                                                                   chr = input$col_chr , 
                                                                                   bp = input$col_chrposition , 
                                                                                   snp = input$col_snp , 
                                                                                   p = input$col_pvalue ,
                                                                                   col = chr_colors)
                                } )
                                
                                snps_to_highlight <- eventReactive( input$submit_click ,{ 
                                  
                                                                req(input$file_with_snps_to_highlight)
                                                                datTmp <- read_tsv(input$file_with_snps_to_highlight$datapath)
                                                                return(datTmp)
                                }
                                )
                                
                                output$manhattanplot <- renderPlot({ manhattanplot_obj() })
                                
                                output$manhattanplot_with_highlight = renderPlot({
                                                                                      if( input$highlight_snp == 'Yes' ){
                                                                                        
                                                                                        snps_to_highlight_vec = snps_to_highlight()[[1]]
                                                                                        manhattan( inputData , 
                                                                                                   chr = input$col_chr , 
                                                                                                   bp = input$col_chrposition , 
                                                                                                   snp = input$col_snp , 
                                                                                                   p = input$col_pvalue ,
                                                                                                   col = chr_colors , 
                                                                                                   highlight = snps_to_highlight_vec 
                                                                                              
                                                                                                   )
                                                                                        
                                                                                      }
                                })
          
}
                      

shinyApp( manhattanPlotUI, manhattanPlotServer )  

