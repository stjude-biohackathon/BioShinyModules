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

source("exportPlot.R")

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

plotKM_ui <- function(id, data) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::sidebarPanel(
        shiny::p(),
        shiny::h4("Data Parameters"),
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
                        shiny::uiOutput(ns("rendContCutPoint"))
                        )
        ),
        shiny::actionButton(ns("click_submit"), label = "Run survival"),
        shiny::hr(),
        shiny::h4("Figure Parameters"),
        fluidRow(
          column(4,
                 shiny::checkboxGroupInput(ns("SurvPlotAddOns"),"",choices = c("Medain Survival Line","Confidence Interval","P.Value"), inline = FALSE)
                 ),
          column(8,
                 shiny::textInput(ns("SurvPlotTitleInput"), "Plot Title:", value = ""),
                 shiny::uiOutput(ns("rendColorPalSelect"))
                 )
          ),
      ),
      shiny::mainPanel(
        fluidRow(
          column(8,
                 shinyjqui::jqui_resizable(shiny::plotOutput(ns("survPlot"), height = "500px")),
                 exportPlot_ui(ns("saveSurv"))
                 #shiny::uiOutput(ns("rendSurvPlotDnldButton"))
                 ),
          column(4,
                 shiny::tableOutput(ns("CoxHRSummary")),
                 shiny::verbatimTextOutput(ns("CoxPHSummary"))
                 )
        ),
        shiny::hr(),
        div(DT::dataTableOutput(ns("survTable")), style = "font-size:12px")
      )
    )
}


#### Server function of the module #### ----------
#' PlotKM server
#'
#' @description R Shiny module server to display Kaplan Meyer Curves
#'
#' @details This module allows the user to plot Kaplan Meier curves.
#' The input data currently supported is a data frame that includes
#' survival time and event columns as well as clinical features.
#'
#' @param id A string.
#' @param data A data frame
#' @returns A Shiny server.
#' @examples
#' \donttest{
#' plotKM_demo()
#' }
#' @keywords plot
#' @export plotKM_server

plotKM_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- shiny::NS(id)
    
    output$rendContCutPoint <- shiny::renderUI({
      
      if (input$select_group_var_type == "Continuous") {
        
        shiny::radioButtons(ns("ContCutPoint"),"",choices = c("Median","Tertile","Quartile"))
        
      }
      
    })
    
    shiny::observeEvent(input$click_submit, {
      
      output$rendSurvPlotDnldButton <- shiny::renderUI({
        
        downloadButton(ns("DownloadButton"))
        
      })
      
    })
    
    output$rendColorPalSelect <- renderUI({
      
      RColBrew <- rownames(RColorBrewer::brewer.pal.info)
      RColBrew_descrete <- c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
      RColBrewCBTF <- RColorBrewer::brewer.pal.info$colorblind
      names(RColBrewCBTF) <- RColBrew
      RColBrewCBTF <- RColBrewCBTF[RColBrew_descrete]
      RColBrew_nCB <- as.list(names(RColBrewCBTF[which(RColBrewCBTF == FALSE)]))
      names(RColBrew_nCB) <- names(RColBrewCBTF[which(RColBrewCBTF == FALSE)])
      RColBrew_CB <- as.list(names(RColBrewCBTF[which(RColBrewCBTF == TRUE)]))
      names(RColBrew_CB) <- names(RColBrewCBTF[which(RColBrewCBTF == TRUE)])
      
      ggsci_names <- as.list(c("NPG","AAAS","NEJM","Lancet","JAMA","JCO","UCSCGB","D3","LocusZoom","IGV","COSMIC","UChicago",
                               "Star Trek","Tron Legacy","Futurama","Rick and Morty","The Simpsons","Flat UI","Frontiers"))
      names(ggsci_names) <- unlist(ggsci_names)
      
      ColorList <- list(`Colorblind Friendly` = RColBrew_CB,
                        `Non Colorblind Friendly or Unknown` = c(RColBrew_nCB,ggsci_names))
      selectInput(ns("ColorPalSelect"),"Color Palette:", choices = ColorList)
      
    })
    
    ## `survDat` return the dataframe useful to plot survival curves 
    survDat <- shiny::eventReactive(input$click_submit, {
      
        #inputData_local = inputData()
        timeVar <- input$time_var
        eventVar <- input$event_var
        groupVar <- input$group_var
        groupVarType <- input$select_group_var_type
        
        datTmp <- data %>% dplyr::select(timeVar, eventVar, groupVar) %>% data.frame 
        datTmp[,timeVar] <- as.numeric(datTmp[,timeVar])
        datTmp[,eventVar] <- as.numeric(datTmp[,eventVar])
        if (input$select_group_var_type == "Continuous") {
          datTmp[,groupVar] <- as.numeric(datTmp[,groupVar])
          CutPoint <- input$ContCutPoint
          if (CutPoint == "Median") {
            datTmp[,groupVar] <- ifelse(datTmp[,groupVar] >= median(datTmp[,groupVar], na.rm = TRUE), "Above_Median", "Below_Median")
          } else if (CutPoint == "Tertile") {
            tert <- quantile(datTmp[,groupVar],c(0:3/3), na.rm = TRUE)
            datTmp[,groupVar] <- with(datTmp, cut(datTmp[,groupVar], tert, include.lowest = T, labels = c("Low", "Medium", "High")))
          } else if (CutPoint == "Quartile") {
            quart <- quantile(datTmp[,groupVar], na.rm = TRUE)
            datTmp[,groupVar] <- with(datTmp, cut(datTmp[,groupVar], quart, include.lowest = T, labels = c("Low", "Medium_Low", "Medium_High", "High")))
          }
          
        } else if (input$select_group_var_type == "Categorical") {
          datTmp[,groupVar] <- factor(datTmp[,groupVar])
        }
        
        datTmp
      
    })
    
    SurvDataTab_react <- shiny::eventReactive(input$click_submit, {
      
      SurvData <- survDat()
      groupVar <- input$group_var
      timeVar <- input$time_var
      eventVar <- input$event_var
      SurvTab <- survival::coxph(as.formula(paste("survival::Surv(",timeVar,",",eventVar,") ~ ",groupVar,sep = "")), data = SurvData)
      
    })
    
    CoxHRSummary_react <- shiny::eventReactive(input$click_submit, {
      
      SurvTab <- SurvDataTab_react()
      groupVar <- input$group_var
      SurvTab2 <- SurvTab %>% 
        gtsummary::tbl_regression(exp = TRUE) %>%
        as_gt()
      
      SurvTabDF <- as.data.frame(SurvTab2)
      
      SurvTabDF <- SurvTabDF %>%
        dplyr::select(label,estimate,ci,p.value)
      colnames(SurvTabDF) <- c("Characteristic","Hazard Ratio","95% Confidence Interval","P.Value")
      
      SurvTabDF
      
    })
    
    output$CoxHRSummary <- shiny::renderTable({
      
      df <- CoxHRSummary_react()
      df 
      
    })
    
    CoxPHSummary_react <- shiny::eventReactive(input$click_submit, {
      
      SurvTab <- SurvDataTab_react()
      groupVar <- input$group_var
      SurvTabPH <- capture.output(summary(SurvTab))
      SurvTabZPH <- capture.output(cox.zph(SurvTab))
      
      con_line <- grep("^Concordance=",SurvTabPH,value = T)
      lik_line <- grep("^Likelihood ratio test=",SurvTabPH,value = T)
      wal_line <- grep("^Wald test",SurvTabPH,value = T)
      sco_line <- grep("^Score ",SurvTabPH,value = T)
      
      text <- paste("CoxH Summary:",con_line,lik_line,wal_line,sco_line,"","Proportional Hazards assumption:",
                    SurvTabZPH[1],SurvTabZPH[2],SurvTabZPH[3],sep = "\n")
      cat(text)
      
    })
    
    output$CoxPHSummary <- shiny::renderPrint({
      
      text <- CoxPHSummary_react()
      text
      
    })
    
    survTable_react <- shiny::eventReactive(input$click_submit, {
      
      df <- survDat()
      timeVar <- input$time_var
      eventVar <- input$event_var
      groupVar <- input$group_var
      df2 <- data %>%
        relocate(timeVar,eventVar,groupVar)
      DT::datatable(df2,
                    options = list(lengthMenu = c(10, 20, 100, 1000),
                                   pageLength = 10,
                                   scrollX = T),
                    rownames = T)
      
    })
    
    
    output$survTable <- DT::renderDataTable({ 
      
      tab <- survTable_react()
      tab
      
    })
    
    survPlot_reactVal <- shiny::eventReactive(input$click_submit, {
      
      survData_local <- survDat()
      timeVar <- input$time_var
      eventVar <- input$event_var
      groupVar <- input$group_var
      
      form <- paste("survival::Surv(",timeVar,",",eventVar,") ~ ",groupVar,sep = "")
      form2 <- as.formula(form)
      sfit <- eval(substitute(survival::survfit(form2,data = survData_local, type="kaplan-meier")))
      names(sfit$strata) <- gsub(paste0(groupVar,"="),"",names(sfit$strata))
      #survPlot_reactVal(sfit)
      sfit
      
    })
    
    SurvPlotVariables <- shiny::eventReactive(input$click_submit, {
      
      timeVar <- input$time_var
      eventVar <- input$event_var
      groupVar <- input$group_var
      groupVarType <- input$select_group_var_type
      
      list(timeVar = timeVar, eventVar = eventVar, groupVar = groupVar, groupVarType = groupVarType)
      
    })
    
    #survPlot_reacVal <- reactiveVal()
    
    survPlot_react <- shiny::reactive({
      
      sfit <- survPlot_reactVal()
      survData_local <- survDat()
      SurvPlotVars <- SurvPlotVariables()
      timeVar <- SurvPlotVars[["timeVar"]]
      eventVar <- SurvPlotVars[["eventVar"]]
      groupVar <- SurvPlotVars[["groupVar"]]
      groupVarType <- SurvPlotVars[["groupVarType"]]
      AddOns <- input$SurvPlotAddOns
      ColorPal <- input$ColorPalSelect
      plotTitle <- input$SurvPlotTitleInput
      if ("Medain Survival Line" %in% AddOns) {
        ShowMedianLine <- "hv"
      } else { ShowMedianLine <- "none" }
      if ("Confidence Interval" %in% AddOns) {
        ShowCI <- TRUE
      } else { ShowCI <- FALSE }
      if ("P.Value" %in% AddOns) {
        ShowPval <- TRUE
      } else { ShowPval <- FALSE }
      
      if (plotTitle == "") {
        if (groupVarType == "Continuous") {
          cutP <- input$ContCutPoint
          plotTitle <- paste0("Survival Curve Featuring ",groupVar," at ",cutP," Over Time")
        } else {
          plotTitle <- paste0("Survival Curve Featuring ",groupVar," Over Time")
        }
      }
      
      p <- survminer::ggsurvplot(sfit,
                                 data = survData_local,
                                 title = plotTitle,
                                 risk.table = TRUE,
                                 legend.title = groupVar,
                                 ggtheme = theme_classic(),
                                 surv.median.line = ShowMedianLine,
                                 conf.int = ShowCI,
                                 pval=ShowPval,
                                 risk.table.height = 0.20,
                                 palette = ColorPal
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
    
    exportPlot_server("saveSurv",survPlotdnld_react)
      
  })
  
}



#### Demo function of the module #### ----------
#' Data import demo
#'
#' @description R Shiny module demo to import data files
#'
#' @details This module allow to import multiple type of data.
#' The file type currently supported are csv, txt, xls, xslx, rda.
#' This demo allow the user to select a dataframe from a file and render it.
#'
#' @returns A Shiny App.
#' @examples
#' \dontrun{
#' dataImport_demo()
#' }
#' @keywords plot
#' @export dataImport_demo
plotKM_demo <- function() {
  ExampleLungData <- lung
  ui <- shiny::fluidPage(
    plotKM_ui("KMplot", ExampleLungData)
  )
  
  server <- function(input, output, session) {
    plotKM_server("KMplot", ExampleLungData)
  }
  shiny::shinyApp(ui, server)
}

