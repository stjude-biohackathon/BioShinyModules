# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author : Yaseswini Neelamraju
# modified by Max Qiu (ytqiuhaowen@gmail.com)

# Documentation
#' R Shiny module to generate a survival analysis plot
#'
#' @param id A string.
#' @param df A dataframe.
#' @param time_var Character
#' @param event_var Character
#' @param group_var Character
#' @param group_var_type
#' @returns A Shiny module.
#' @examples
#' survival_demo()
#' survival_demo_1()
#' survival_demo_2()
#### Library needed #### ----------
library(shiny)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(survminer)
library(survival)

#### Function needed to work #### ----------


#### UI function of the module #### ----------

survival_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # selectInput( NS(id, "select_surv_model"), "Choose a model" , choices = c("KM","Cox-proportional") ),
    # conditionalPanel(
    #         condition = "input.select_surv_model == 'KM'",
    #         selectInput( NS(id, "select_group_var_type"),
    #                      "Choose the type of the variable" ,
    #                      choices = c("categorical","continuous") ) ),

    # dataTableOutput( NS(id, "table") ),
    # verbatimTextOutput( NS(id, "test") ),
    plotOutput(ns("survPlot"))
  )
}



#### Server function of the module #### ----------

survival_server <- function(id, df, time_var, event_var, group_var, group_var_type) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(time_var))
  stopifnot(is.reactive(event_var))
  stopifnot(is.reactive(group_var))
  stopifnot(is.reactive(group_var_type))

  moduleServer(id, function(input, output, session) {
    datTmp <- reactive({
      dat <- df() %>%
        dplyr::select(time_var(), event_var(), group_var()) %>%
        data.frame()

      dat[, "time_for_surv"] <- as.numeric(dat[, time_var()])
      dat[, "event_for_surv"] <- as.numeric(dat[, event_var()])

      if (group_var_type() == "continuous") {
        dat$numeric_val <- dat[, group_var()]
        dat$surv_group <- ifelse(dat$numeric_val >= median(dat$numeric_val), ">Median", "<Median")
        return(dat)
      } else if (group_var_type() == "categorical") {
        print(group_var())
        print(dat)
        dat$surv_group <- factor(dat[, group_var()])
        return(dat)
      }

      return(dat)
    })

    # output$table = renderDataTable({
    #         datTmp() %>% head()
    # })

    sfit <- reactive({
      sfitTmp <- survfit(Surv(time_for_surv, event_for_surv) ~ surv_group, data = datTmp())
      names(sfitTmp[["strata"]]) <- gsub("surv_group=", "", names(sfitTmp[["strata"]]))
      return(sfitTmp)
    })

    # output$test = renderPrint({
    #         surv_summary(sfit(), data = datTmp()) %>%
    #                 head()
    # })

    surv_Plot <- reactive({
      p <- survminer::ggsurvplot(sfit(),
        data = datTmp(),
        risk.table = TRUE,
        legend.title = group_var(),
        ggtheme = theme_classic(),
        palette = "nejm"
      )

      p$plot <- p$plot + theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
      )
      p$table <- p$table + theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
      )
      return(p)
    })

    output$survPlot <- renderPlot({
      surv_Plot()$plot
    })
    return(surv_Plot)
  })
}

#### Demo function of the module #### ----------

survival_demo <- function() {
  df <- read.delim("../example_data/InputData_for_Survival_v1.txt")
  time_var <- "time"
  event_var <- "status"
  group_var <- "sex"
  group_var_type <- "categorical"

  ui <- fluidPage(survival_ui("x"))
  server <- function(input, output, session) {
    survival_server(
      "x", reactive({
        df
      }), reactive({
        time_var
      }), reactive({
        event_var
      }),
      reactive({
        group_var
      }), reactive({
        group_var_type
      })
    )
  }
  shinyApp(ui, server)
}

survival_demo_1 <- function() {
  source("dataImport.R")

  ui <- fluidPage(
    dataImport_ui("datafile", "User data (csv, xls, xlsx format)"),
    selectInput("time_var", "Choose a column that has time variable", choices = NULL),
    selectInput("event_var", "Choose a column that has event variable", choices = NULL),
    selectInput("group_var", "Choose a group variable", choices = NULL),
    selectInput("group_var_type", "Choose the type of the variable",
      choices = c("categorical", "continuous")
    ),
    survival_ui("x")
  )
  server <- function(input, output, session) {
    df <- dataImport_server("datafile")

    observe({
      df_colnames <- colnames(df())

      updateSelectInput(
        inputId = "time_var",
        choices = df_colnames
      )

      updateSelectInput(
        inputId = "event_var",
        choices = df_colnames
      )

      updateSelectInput(
        inputId = "group_var",
        choices = df_colnames
      )
    })

    survival_server(
      "x", df, reactive({
        input$time_var
      }), reactive({
        input$event_var
      }),
      reactive({
        input$group_var
      }), reactive({
        input$group_var_type
      })
    )
  }
  shinyApp(ui, server)
}

survival_demo_2 <- function() {
  source("dataImport.R")
  source("ggsave_Both.R")

  ui <- fluidPage(
    dataImport_ui("datafile", "User data (csv, xls, xlsx format)"),
    selectInput("time_var", "Choose a column that has time variable", choices = NULL),
    selectInput("event_var", "Choose a column that has event variable", choices = NULL),
    selectInput("group_var", "Choose a group variable", choices = NULL),
    selectInput("group_var_type", "Choose the type of the variable",
      choices = c("categorical", "continuous")
    ),
    survival_ui("x"),
    ggsaveBoth_ui("saveplot")
  )
  server <- function(input, output, session) {
    df <- dataImport_server("datafile")

    observe({
      df_colnames <- colnames(df())

      updateSelectInput(
        inputId = "time_var",
        choices = df_colnames
      )

      updateSelectInput(
        inputId = "event_var",
        choices = df_colnames
      )

      updateSelectInput(
        inputId = "group_var",
        choices = df_colnames
      )
    })

    my_plot <- survival_server(
      "x", df, reactive({
        input$time_var
      }), reactive({
        input$event_var
      }),
      reactive({
        input$group_var
      }), reactive({
        input$group_var_type
      })
    )

    saveplot <- reactive({
      my_plot()[["plot"]]
    })

    ggsaveBoth_server("saveplot", saveplot)
  }
  shinyApp(ui, server)
}

# TODO update documentation
