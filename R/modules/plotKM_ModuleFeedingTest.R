# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Alyssa Obermayer (alyssa.obermayer@moffitt.org)
# author: Yaseswini Neelamraju ()

#### Library needed #### ----------
usethis::use_package("shiny")
usethis::use_package("dplyr")
usethis::use_package("ggpubr")
usethis::use_package("ggthemes")
usethis::use_package("survminer")
usethis::use_package("survival")
usethis::use_package("shinyjqui")
usethis::use_package("gtsummary")
usethis::use_package("ggsci")
usethis::use_package("RColorBrewer")
usethis::use_package("ggrepel")


#library("shiny")
#library("dplyr")
#library("ggpubr")
#library("ggthemes")
#library("survminer")
#library("survival")
#library("shinyjqui")
#library("gtsummary")
#library("ggsci")
#library("RColorBrewer")
#library("ggrepel")


source("D:/R/KIDS23-Team13/R/modules/exportPlot.R")

#### UI function of the module #### ----------
#' PlotKM ui
#'
#' @description R Shiny module UI to display Kaplan Meyer Curves
#'
#' @details This module allows the user to plot Kaplan Meier curves.
#' The input data currently supported is a data frame that includes
#' survival time and event columns as well as clinical features.
#' The UI will ask the user to select the columns that represent the
#' survival time and event columns, as well as the features they 
#' want to stratify the plot with. The user must press the 
#' "Run Survival" button to display the plot.
#'
#' @param id A string.
#' @param data A data frame
#' @returns A Shiny UI.
#' @examples
#' \donttest{
#' plotKM_demo()
#' }
#' @keywords plot
#' @export plotKM_ui
plotKM_Input_ui <- function(id, data, orientation = 2) {
  ns <- shiny::NS(id)
  ## As column
  if (orientation == 2) {
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6,
                      shiny::selectInput(ns("time_var"), "Time column:", choices = colnames(data))
        ),
        shiny::column(6,
                      shiny::selectInput(ns("event_var"), "Event column:", choices = colnames(data))
        )
      ),
      shiny::fluidRow(
        shiny::column(6,
                      shiny::selectInput(ns("group_var"), "Feature column:", choices = colnames(data))
        ),
        shiny::column(3,
                      shiny::radioButtons(ns("select_group_var_type"),"",choices = c("Categorical","Continuous"))
        ),
        shiny::column(3,
                      shiny::checkboxGroupInput(ns("SurvPlotAddOns"),"",choices = c("Medain Survival Line","Confidence Interval","P.Value"), inline = FALSE)
        )
      ),
      shiny::actionButton(ns("click_submit"), label = "Run survival")
      )
    ## As Row
  } else if (orientation == 1) {
    shiny::tagList(
      shiny::fluidRow(
        shiny::fluidRow(
          shiny::column(6,
                        shiny::selectInput(ns("time_var"), "Time column:", choices = colnames(data))
          ),
          shiny::column(6,
                        shiny::selectInput(ns("event_var"), "Event column:", choices = colnames(data))
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
                        shiny::selectInput(ns("group_var"), "Feature column:", choices = colnames(data))
          ),
          shiny::column(3,
                        shiny::radioButtons(ns("select_group_var_type"),"",choices = c("Categorical","Continuous"))
          ),
          shiny::column(3,
                        shiny::checkboxGroupInput(ns("SurvPlotAddOns"),"",choices = c("Medain Survival Line","Confidence Interval","P.Value"), inline = FALSE, selected = "P.Value")
          )
        ),
        shiny::actionButton(ns("click_submit"), label = "Run survival")
      )
    )
  } 
}

plotKM_Input_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    return(
      list(
        time_var = reactive({input$time_var}),
        event_var = reactive({input$event_var}),
        group_var = reactive({input$group_var}),
        select_group_var_type = reactive({input$select_group_var_type}),
        SurvPlotAddOns = reactive({input$SurvPlotAddOns}),
        click_submit = reactive({input$click_submit})
      )
    )
    
  })
}

plotKM_Plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjqui::jqui_resizable(shiny::plotOutput(ns("survPlot")))
  )
}


plotKM_Plot_server <- function(id, data, plotKM_Input) {
  
  moduleServer(id, function(input, output, session) {
    
    Submit_react <- reactive({ plotKM_Input$click_submit() })
    
    ## `survDat` return the dataframe useful to plot survival curves 
    survDat <- shiny::eventReactive(Submit_react(), {
      
      #inputData_local = inputData()
      
      timeVar <- plotKM_Input$time_var()
      eventVar <- plotKM_Input$event_var()
      groupVar <- plotKM_Input$group_var()
      groupVarType <- plotKM_Input$select_group_var_type()
      
      #timeVar <- input$time_var
      #eventVar <- input$event_var
      #groupVar <- input$group_var
      #groupVarType <- input$select_group_var_type
      
      datTmp <- data %>% dplyr::select(timeVar, eventVar, groupVar) %>% data.frame 
      datTmp[,timeVar] <- as.numeric(datTmp[,timeVar])
      datTmp[,eventVar] <- as.numeric(datTmp[,eventVar])
      if (groupVarType == "Continuous") {
        datTmp[,groupVar] <- as.numeric(datTmp[,groupVar])
        CutPoint <- input$ContCutPoint
        #if (CutPoint == "Median") {
        datTmp[,groupVar] <- ifelse(datTmp[,groupVar] >= median(datTmp[,groupVar], na.rm = TRUE), "Above_Median", "Below_Median")
        #} else if (CutPoint == "Tertile") {
        #  tert <- quantile(datTmp[,groupVar],c(0:3/3), na.rm = TRUE)
        #  datTmp[,groupVar] <- with(datTmp, cut(datTmp[,groupVar], tert, include.lowest = T, labels = c("Low", "Medium", "High")))
        #} else if (CutPoint == "Quartile") {
        #  quart <- quantile(datTmp[,groupVar], na.rm = TRUE)
        #  datTmp[,groupVar] <- with(datTmp, cut(datTmp[,groupVar], quart, include.lowest = T, labels = c("Low", "Medium_Low", "Medium_High", "High")))
        #}
        
      } else if (groupVarType == "Categorical") {
        datTmp[,groupVar] <- factor(datTmp[,groupVar])
      }
      
      datTmp
      
    })
    
    survPlot_reactVal <- shiny::eventReactive(Submit_react(), {
      
      survData_local <- survDat()
      
      timeVar <- plotKM_Input$time_var()
      eventVar <- plotKM_Input$event_var()
      groupVar <- plotKM_Input$group_var()
      
      #timeVar <- input$time_var
      #eventVar <- input$event_var
      #groupVar <- input$group_var
      
      form <- paste("survival::Surv(",timeVar,",",eventVar,") ~ ",groupVar,sep = "")
      form2 <- as.formula(form)
      sfit <- eval(substitute(survival::survfit(form2,data = survData_local, type="kaplan-meier")))
      names(sfit$strata) <- gsub(paste0(groupVar,"="),"",names(sfit$strata))
      sfit
      
    })
    
    SurvPlotVariables <- shiny::eventReactive(Submit_react(), {
      
      timeVar <- plotKM_Input$time_var()
      eventVar <- plotKM_Input$event_var()
      groupVar <- plotKM_Input$group_var()
      groupVarType <- plotKM_Input$select_group_var_type()
      
      #timeVar <- input$time_var
      #eventVar <- input$event_var
      #groupVar <- input$group_var
      #groupVarType <- input$select_group_var_type
      
      list(timeVar = timeVar, eventVar = eventVar, groupVar = groupVar, groupVarType = groupVarType)
      
    })
    
    survPlot_react <- shiny::reactive({
      
      sfit <- survPlot_reactVal()
      survData_local <- survDat()
      SurvPlotVars <- SurvPlotVariables()
      timeVar <- plotKM_Input$time_var()
      eventVar <- plotKM_Input$event_var()
      groupVar <- plotKM_Input$group_var()
      groupVarType <- plotKM_Input$select_group_var_type()
      AddOns <- plotKM_Input$SurvPlotAddOns()
      #AddOns <- input$SurvPlotAddOns
      #ColorPal <- input$ColorPalSelect
      #plotTitle <- input$SurvPlotTitleInput
      if ("Medain Survival Line" %in% AddOns) {
        ShowMedianLine <- "hv"
      } else { ShowMedianLine <- "none" }
      if ("Confidence Interval" %in% AddOns) {
        ShowCI <- TRUE
      } else { ShowCI <- FALSE }
      if ("P.Value" %in% AddOns) {
        ShowPval <- TRUE
      } else { ShowPval <- FALSE }
      
      #if (plotTitle == "") {
      #  if (groupVarType == "Continuous") {
      #    cutP <- input$ContCutPoint
      #    plotTitle <- paste0("Survival Curve Featuring ",groupVar," at ",cutP," Over Time")
      #  } else {
      #    plotTitle <- paste0("Survival Curve Featuring ",groupVar," Over Time")
      #  }
      #}
      
      p <- survminer::ggsurvplot(sfit,
                                 data = survData_local,
                                 #title = plotTitle,
                                 risk.table = TRUE,
                                 legend.title = groupVar,
                                 ggtheme = theme_classic(),
                                 surv.median.line = ShowMedianLine,
                                 conf.int = ShowCI,
                                 pval=ShowPval,
                                 risk.table.height = 0.20
                                 #palette = ColorPal
      )
      if (ShowMedianLine != "none") {
        MedSurvItem <- p[["plot"]][["layers"]][length(p[["plot"]][["layers"]])]
        MedSurvItem_df <- MedSurvItem[[1]][["data"]]
        MedSurvItem_df <- MedSurvItem_df[order(MedSurvItem_df[,1]),]
        MedSurvItem_df <- MedSurvItem_df %>%
          mutate(label = round(MedSurvItem_df[,1]))
        rownames(MedSurvItem_df) <- 1:nrow(MedSurvItem_df)
        if (nrow(MedSurvItem_df) > 1) {
          p$plot <- p$plot +
            geom_label_repel(data = MedSurvItem_df, aes(x = x1, y = y1, label = label, size = 4), label.size = NA, show.legend = FALSE)
        }
      }
      p$plot <- p$plot + theme(axis.text = element_text(size = 15), 
                               axis.title = element_text(size = 15), 
                               legend.text = element_text(size = 15), 
                               legend.title = element_text(size = 15))
      p$table <- p$table + theme(axis.text = element_text(size = 15), 
                                 axis.title = element_text(size = 15), 
                                 legend.text = element_text(size = 15), 
                                 legend.title = element_text(size = 15))
      p
      
    })
    
    survPlotdnld_react <- reactive({
      
      p <- survPlot_react()
      p$plot
      
    })
    
    output$survPlot <- shiny::renderPlot({
      
      p <- survPlot_react()
      p
      
    })
    
    return(survPlotdnld_react)
    
  })
}



plotKM_demo <- function() {
  ExampleLungData <- lung
  ui <- shiny::fluidPage(
    #sidebarPanel(
    #  plotKM_Input_ui("KMplot_Input", ExampleLungData, 2)
    #),
    mainPanel(
      plotKM_Input_ui("KMplot_Input", ExampleLungData, 1),
      plotKM_Plot_ui("KMplot_Plot"),
      exportPlot_ui("saveSurv")
    )
  )
  
  server <- function(input, output, session) {
    
    plotKMinput <- plotKM_Input_server("KMplot_Input")
    plotKMplot <- plotKM_Plot_server("KMplot_Plot", ExampleLungData, plotKMinput)
    exportPlot_server("saveSurv",plotKMplot)
    
  }
  
  shiny::shinyApp(ui, server)
}
