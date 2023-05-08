## Shiny app for DNA methylation analysis ## 
## Author : Yaseswini Neelamraju
## Example data was already upload by Christy LaFlamme

library( data.table )
library( GenomicRanges )
library( tidyverse )
library( ggthemes )
library( wesanderson )
library( MetBrewer )
options(shiny.maxRequestSize=200*1024^2)
options(stringsAsFactors = F)
colorChoices = c( met.brewer(name="Derain") )

DNAmethylationUI <- fluidPage(
                          
                              sidebarPanel(
                                 ## Read the methylation data file 
                                fileInput( "methInFile" , "Upload the input file" , accept = c(".gz",".tsv") ), 
                                uiOutput("methData_cols_notreqd"),
  
                                ## Dropdown menu that lists columns from the phenotype file for the user to select appropriate column defining the sampleIDs 
                                fileInput( "phenoInFile" , "Upload phenotype file" ),
                                uiOutput("pheno_sampleIDcol_ui"),
                                uiOutput("pheno_colorBy_chooseBroad_ui" ),
                                uiOutput("ui1"),
                
                                ## Enter the region of interest
                                textInput( "RegionLocus" , "Enter the locus of interest(Ex:chr1:10-100)" ),
                                textInput( "allowFlankbp" , "How many bps can the locus be extended" , placeholder = 10 ) ,
                                
                                actionButton( "submit_click" , "Find Regions" ) 
                              ),

                                mainPanel(
                                          tableOutput("view"),
                                          plotOutput( "methLinePlot" ),
                                          plotOutput( "methBoxPlot" )
                                          ## Plot output 
                                          #plotOutput( linePlot_meth ),
                                          #plotOutput( boxPlot_meth ) 
                                )
                                          
                              )

DNAmethylationServer <- function( input , output , session ){
                          
                                      methData = reactive({
                                                                req(input$methInFile)
                                                                datTmp <- read_tsv(input$methInFile$datapath) 
                                                                return(datTmp)
                                                              })
                                      
                                      phenoData = reactive({
                                                                req(input$phenoInFile)
                                                                datTmp <- read_tsv(input$phenoInFile$datapath) 
                                                                return(datTmp)
                                                                
                                                              })
                                      ## Automatically populates the column names identified in the phenotype file and methylation files 
                                      output$pheno_sampleIDcol_ui = renderUI({
                                                            selectInput( "pheno_sampleIDcol" , "Select the column that denote sample id" , choices = names(phenoData()) )
                                       })
                                      
                                      output$pheno_colorBy_chooseBroad_ui = renderUI({  
                                        
                                        selectInput( "pheno_colorBy_broadCategory" , 
                                                     label = "How do you want to color the points?" , 
                                                     choices = c("IndividualSamples","Specific Column from the phenotype" ))
                                      })
                                      
                                      
                                      output$ui1 <- renderUI({
                                        
                                        phenoData_local = phenoData()
                                        phenoData_colnames = colnames(phenoData_local)
                                        sampleID_column_local = input$pheno_sampleIDcol
                                        sampleIDs = phenoData_local[, sampleID_column_local ]
                                        
                                                                    if (input$pheno_colorBy_broadCategory == 'IndividualSamples'){
                                                                      selectInput('samplesToGroup', 'Select a group of samples', choices = sampleIDs , multiple=TRUE)
                                                                    } else if ( input$pheno_colorBy_broadCategory == "Specific Column from the phenotype" ){
                                                                      selectInput('pheno_colorBy', 'Choose a column to represent', choices = phenoData_colnames )
                                                                    }
                                      })
                                      
                                      ## Compute overlaps ## 
                                      get_overlaps <- eventReactive( input$submit_click , {
                                        
                                                      CpG_gr = makeGRangesFromDataFrame( methData() %>% dplyr::rename( "chr" = "CpG_chrm" , "start" = "CpG_beg", "end" = "CpG_end" ) , keep.extra.columns = TRUE )
                                                      SearchRegion = setNames( data.frame( t( data.frame( "SearchRegion" = unlist( strsplit( as.character( input$RegionLocus ) , split = ":|-" ) ) ) ) ) , c("chr","start","end") )
                                                      SearchRegion$start = as.numeric(SearchRegion$start) - as.numeric( input$allowFlankbp )
                                                      SearchRegion$end = as.numeric(SearchRegion$end) + as.numeric( input$allowFlankbp )
                                                      SearchRegion_gr = makeGRangesFromDataFrame( SearchRegion )
                                                      
                                                      ## Overlap 
                                                      OverlappingRegionCoord = as.data.frame( findOverlaps( CpG_gr , SearchRegion_gr ) )
                                                      
                                                      OverlappingRegions_CpGinformation = as.data.frame( CpG_gr [  OverlappingRegionCoord$queryHits  ] )
                                                      
                                                      return(OverlappingRegions_CpGinformation)                 
                                                    })
                                      
                                      output$view = renderTable({
                                                                        get_overlaps_output = get_overlaps()
                                                                        if( nrow(get_overlaps_output) == 0 ){
                                                                          return( "No overlaps found" )
                                                                        } 
                                                                        else{
                                                                          get_overlaps() %>% head(n=3)
                                                                        }
                                                               })
                                      
                                      get_plotData <- eventReactive( input$submit_click , {
                                        
                                                OverlappingRegions_CpGinformation = get_overlaps()                  
                                                
                                                if( nrow(OverlappingRegions_CpGinformation) > 0  ){
                                                  
                                                  phenoData_local = phenoData()
                                                  phenoData_filtered = phenoData_local %>% dplyr::select( Sample_ID , input$pheno_colorBy ) %>% dplyr::rename( "SampleID" = "Sample_ID" )
                                                  
                                                  if( input$pheno_colorBy_broadCategory == "Specific Column from the phenotype" ){
                                                    
                                                    methPlotData = OverlappingRegions_CpGinformation %>% 
                                                      pivot_longer( cols = -c("seqnames","start","end","width","strand","probeID") , names_to = "SampleID" , values_to = "BetaValues") %>%
                                                      mutate( SampleID = gsub("^X","",SampleID) ) %>%
                                                      left_join( phenoData_filtered ) %>%
                                                      mutate( colorGroup = !!rlang::sym(input$pheno_colorBy) ) %>%
                                                      mutate( chrBase = paste0( seqnames , ":" , start , "-" , end ) )
                                                  }else if( input$pheno_colorBy_broadCategory == "IndividualSamples" ){
                                                    methPlotData = OverlappingRegions_CpGinformation %>% 
                                                      pivot_longer( cols = -c("seqnames","start","end","width","strand","probeID") , names_to = "SampleID" , values_to = "BetaValues") %>%
                                                      mutate( SampleID = gsub("^X","",SampleID) ) %>%
                                                      mutate( colorGroup = if_else( SampleID %in% input$samplesToGroup , "Sample-of-Interest" , "Others" ) ) %>%
                                                      mutate( chrBase = paste0( seqnames , ":" , start , "-" , end ) )
                                                  }
                                                  return(methPlotData)
                                                  
                                                }
                                          }
                                      )
                                          
                                      
                                      output$methLinePlot = renderPlot({
                                                  plotData = get_plotData()
  
                                                if( nrow(plotData) > 0 ){
                                                 
                                                  p = ggplot( plotData , aes( x = chrBase , y = BetaValues  ) ) + 
                                                    geom_line(aes(group = SampleID,  linetype = colorGroup , colour = colorGroup ) ) +
                                                    scale_colour_manual(values = setNames( colorChoices[ 1:length(unique(plotData$colorGroup))] , c("Sample-of-Interest","Others") ) )+
                                                    scale_linetype_manual(values = setNames( c("solid","dashed") , c("Sample-of-Interest","Others") ) ) +
                                                    coord_cartesian(ylim = c(0,1)) + 
                                                    theme_base() +
                                                    labs( x = "" , y = "Beta values" , title = input$Region_Locus ) + 
                                                    theme( legend.position = "top", legend.title = element_blank() , axis.text.x = element_text( angle = 45 , vjust = 1 , hjust = 1 ) )
                                                  
                                                  return(p)
                                                 }

                                                })
                                                
                                      output$methBoxPlot = renderPlot({
                                                  
                                                  plotData = get_plotData()
                                                  
                                                  if( nrow(plotData) > 0 ){
                                                  
                                                    p = ggplot( plotData , aes( x = chrBase , y = BetaValues , fill = colorGroup  ) ) + 
                                                              geom_boxplot() +
                                                              scale_fill_manual(values = setNames( colorChoices[ 1:length(unique(plotData$colorGroup))] , unique(plotData$colorGroup) ) ) +
                                                              coord_cartesian(ylim = c(0,1)) + 
                                                              theme_base() +
                                                              labs( x = "" , y = "Beta values" , title = input$Region_Locus ) + 
                                                              theme( legend.position = "top", legend.title = element_blank() , axis.text.x = element_text( angle = 45 , vjust = 1 , hjust = 1 ) )
                                                            
                                                    return(p)        
                                                  }
                                                }) 
                                        
}

shinyApp( ui = DNAmethylationUI , server = DNAmethylationServer )



