# Author : Yaseswini Neelamraju
## User provides the table 
## User has the flexibility to choose columns that specify time and event
## User has the flexibility to choose KM or Cox proportional hazard 
## If KM plot is choosen --
##      the user can choose the data column of choice to split the data ( Not implemented yet ; default is median now )
##      Once the data column is choosen , user has options to choose if midpoint or quartile ( Not implemented yet ) 
## If Cox is choosen -- ( not implemented yet )
##      User has the choice to select multiple columns to run survival analysis 

library( shiny )
library( tidyverse )
library( ggpubr )
library( ggthemes )
library( survminer )
library( survival )

inputData_local = lung
colorChoices = c( met.brewer(name="Derain") )

survivalUI <- fluidPage(
                            sidebarPanel( 
                              
                                    # fileInput( "inputFileSurvival" , "Upload input file for survival" ) ,
                                    selectInput( "time_var" , "Choose a column that has time variable" , choices = colnames(inputData) ),
                                    selectInput( "event_var" , "Choose a column that has event variable" , choices = colnames(inputData) ),
                                    selectInput( "select_surv_model" , "Choose a model" , choices = c("KM","Cox-proportional") ),
                            
                                    selectInput( "group_var" , "Choose a group variable" , choices = colnames(inputData) ),
                                    conditionalPanel(
                                                      condition = "input.select_surv_model == 'KM'",
                                                       selectInput( "select_group_var_type" , 
                                                                     "Choose the type of the variable" , 
                                                                    choices = c("categorical","continuous") ) ),
                                    actionButton( "click_submit", label = "Run survival" ),
                                                                    
                            ),
                                    #
                            mainPanel(
                                      tableOutput( "survDat" ),
                                      plotOutput( "survPlot" )
                            )
                            
                                    
                          
                        )
survivalServer <- function( input , output , session ){
                                    
                                    ## if input file is choosen 
                                    #inputData <- reactive({
                                    #                          req(input$inputFileSurvival)
                                    #                          datTmp <- read_tsv(input$methInFile$datapath) 
                                    #                         return(datTmp)
                                    #                     })
                                    
                                    ## `survDat` return the dataframe useful to plot survival curves 
                                    survDat = eventReactive( input$click_submit , {
                                                            if( input$select_surv_model == "KM" ){
                                                               
                                                               #inputData_local = inputData()
                                                               
                                                               datTmp = inputData_local %>% dplyr::select( input$time_var , input$event_var , input$group_var ) %>% data.frame 
                                                                datTmp[ , "time_for_surv" ] = as.numeric( datTmp[ , input$time_var ] )
                                                                datTmp[ , "event_for_surv" ] = as.numeric( datTmp[ , input$event_var ] )
                                                                if( input$select_group_var_type == "continuous" ){
                                                                      
                                                                      datTmp$numeric_val = datTmp[ ,input$group_var ]
                                                                      datTmp$surv_group = ifelse(datTmp$numeric_val >= median(datTmp$numeric_val), ">Median", "<Median" )
                                                                      return(datTmp)
                                                                      
                                                                }else if( input$select_group_var_type == "categorical" ){
                                                                    
                                                                    datTmp$surv_group = factor( datTmp[ , input$group_var ] )
                                                                    return(datTmp)
                                                                }

                                                            }
                                                              
                                    })
                                    
                                    output$survTable = renderTable({ 
                                                                       survDat() %>% head  
                                                                  })
                                    
                                    output$survPlot = renderPlot({
                                                                        survData_local = survDat() 
                                                                        
                                                                        if( input$select_surv_model == "KM" ){
                                      
                                                                        sfit = survfit( Surv( time_for_surv , event_for_surv  ) ~ surv_group , data = survData_local )
                                                                        names(sfit$strata) = gsub("surv_group=","",names(sfit$strata) )
                                                                        #surv_colors = setNames( colorChoices[ length(unique(survData_local$surv_group)) ] , unique(survData_local$surv_group) )
                                                                        
                                                                        p = ggsurvplot(  sfit, 
                                                                                     data = survData_local,
                                                                                     risk.table = TRUE,
                                                                                     legend.title = input$group_var,
                                                                                     ggtheme = theme_classic(),
                                                                                     palette = "nejm"
                                                                                   )
                                                                        p$plot = p$plot + theme( axis.text = element_text( size = 15 ) , 
                                                                                                 axis.title = element_text( size = 15 ) , 
                                                                                                 legend.text = element_text( size = 15 ) , 
                                                                                                 legend.title = element_text( size = 15 ) )
                                                                        p$table = p$table + theme( axis.text = element_text( size = 15 ) , 
                                                                                                   axis.title = element_text( size = 15 ) , 
                                                                                                   legend.text = element_text( size = 15 ) , 
                                                                                                   legend.title = element_text( size = 15 ) )
                                                                        return(p)
                                                                       }
  
                                                          })

                                    }     
                                      

shinyApp(ui=survivalUI,server=survivalServer)

