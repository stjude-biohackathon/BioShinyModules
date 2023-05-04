library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly) #install.packages("plotly")
library(colourpicker) #install.packages("colourpicker")
library(ggpubr)
library(readxl)
library(viridis)
library(patchwork)
library(shinycssloaders)
library(shinyjqui)
library(pheatmap)
library(EnhancedVolcano)

####----Input Files----####

## Volcano File
df_vol <- read.delim("L29_vitro_Control_vs_knockdown_diff.txt")
df_vol <- mutate(df_vol, log_pval = -log10(df_vol$P.Value))
df_vol <- mutate(df_vol, log_adjpval = -log10(df_vol$adj.P.Val))

stat <- read.delim("L29_vitro_Control_vs_knockdown_diff.txt")

res = stat
feature_col = "gene"
padj_col = "adj.P.Val"
log2fc_col = "logFC"

volGenes <- df_vol[,1]


## Mito File
GEX5<- read.table(file="GEX5Line.txt", header=T)
###descriptive
grouped <- group_by(GEX5, position)
combo<-summarise(grouped, mean=mean(uniq_cell_umiN), sd=sd(uniq_cell_umiN))

## Violin File
expr<- read.delim(file="TCGA_CHOL_Expression_PatientID.txt", header=T, check.names=F) 
meta<- read.delim(file="TCGA_CHOL_Clinical_PatientID.txt", header=T, check.names=F) 
GeneList <- expr[,1]
ColumnList <- colnames(meta)


## Heatmap
load("MS_2.rda")
df <- df
sample_anno <- sample_meta
sample_anno_col <- "sampleLabel"
feature_anno <- feature_meta
feature_anno_col <- "featureName"
## Histogram
d <- unlist(df)



####----Volcano----####

module_volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      colourInput(NS(id,"dotcol"),"Pick point color", value = "black"),
      radioButtons(NS(id,"stat"),"Choose statistic for plot",
                   choices=c("-Log10 P value" = "log_pval","-Log10 adjusted P value" = "log_adjpval"),
                   selected = "log_pval")
    ),
    mainPanel(
      plotOutput(NS(id,"volcano"))
    )
  )
}

module_volcano_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$volcano <- renderPlot({
      ggplot(df_vol, aes(x=logFC, y=df_vol[,input$stat])) + geom_point(col=input$dotcol)
    })
  })
}

####----Volcano - New----####

##--Plot Volcano--##

plotVolcano <- function(res, feature_col = NULL, padj_col = NULL, log2fc_col = NULL,
                        fdr = 0.05, log2fc = 1) {
  
  require(dplyr)
  require(ggplot2)
  
  # identify column names for feature_col, padj and log2fc
  if (!is.null(feature_col)) feature_col <- feature_col else feature_col <- "variable"
  if (!is.null(padj_col)) padj_col <- padj_col else padj_col <- "padj"
  if (!is.null(log2fc_col)) log2fc_col <- log2fc_col else log2fc_col <- "FC(log2)"
  
  # data wrangling
  res <- res %>%
    mutate(`-log10padj` = -log10(!!sym(padj_col)))
  
  df_filt <- res %>%
    mutate(`-log10padj` = -log10(!!sym(padj_col))) %>%
    dplyr::filter(!!sym(padj_col) < fdr) %>%
    mutate(status = case_when(!!sym(log2fc_col) < - log2fc ~ "Down",
                              !!sym(log2fc_col) > log2fc ~ "Up") )
  
  
  df_filt_up <- df_filt %>% dplyr::filter(status == "Up")
  df_filt_down <- df_filt %>% dplyr::filter(status == "Down")
  up_no <- nrow(dplyr::filter(df_filt, status == "Up"))
  down_no <- nrow(dplyr::filter(df_filt, status == "Down"))
  
  ### truncate feature names if too long
  df_filt <- df_filt %>%
    mutate(across(!!sym(feature_col), ~ stringr::str_trunc(., width = 15, ellipsis = "")))
  ### only add annotation to top 10 highlight
  up_top10 <- df_filt_up %>%
    arrange(!!sym(padj_col)) %>%
    dplyr::slice(1:10)
  down_top10 <- df_filt_down %>%
    arrange(!!sym(padj_col)) %>%
    dplyr::slice(1:10)
  
  # plot range parameter
  volcano_xlim <- max(na.omit(abs(res[log2fc_col])))
  volcano_ylim <- max(-log10(na.omit(res[padj_col])))
  
  # draw plot
  feature_col <- sym(feature_col)
  log2fc_col <- sym(log2fc_col)
  ggplot(res, aes(!!log2fc_col, `-log10padj`)) +
    geom_point(alpha = 0.4, size = 1.5, colour = "grey50", na.rm = TRUE) +
    scale_x_continuous(limits = c(-volcano_xlim, volcano_xlim)) +
    scale_y_continuous(limits = c(0, volcano_ylim)) +
    
    ### highlight up/down variables
    geom_point(
      data = df_filt_down, shape = 21, alpha = 0.6,
      size = 1.5, fill = "blue", colour = "blue", na.rm = TRUE
    ) +
    geom_point(
      data = df_filt_up, shape = 21, alpha = 0.6,
      size = 1.5, fill = "red", colour = "red", na.rm = TRUE
    ) +
    ### add annotation to up/down variables
    geom_point(
      data = up_top10, shape = 21, fill = "red",
      colour = "black", size = 2, na.rm = TRUE
    ) +
    ggrepel::geom_text_repel(
      data = up_top10, aes(label = !!feature_col), na.rm = TRUE,
      size = 4, max.overlaps = 20
    ) +
    geom_point(
      data = down_top10, shape = 21, fill = "blue",
      colour = "black", size = 2, na.rm = TRUE
    ) +
    ggrepel::geom_text_repel(
      data = down_top10, aes(label = !!feature_col), na.rm = TRUE,
      size = 4, max.overlaps = 20
    ) +
    ### add a frame to the plot, theme_bw
    theme_bw(base_size = 14) +
    ### add labels for x and y, and title to show highlight criteria
    labs(
      x = "log2-fold change",
      y = "-log 10 (padj)",
      title = paste("Volcano plot (", sprintf("FDR: %.2f, log2FC: %.1f", fdr, log2fc), ")", sep = ""),
      subtitle = sprintf("Up %i Down %i", up_no, down_no)
    )
}

volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #sidebarPanel(
    #  #fluidRow(
    #  #  column(6,
    #  #         numericInput("PvlCut","P. Val Cutoff")
    #  #         ),
    #  #  column(6,
    #  #         numericInput("LFCCut","Log FC Cutoff")
    #  #         )
    #  #)
    #),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id, "plot"))), type = 6),
      downloadButton(NS(id, "dnld"), label = "")
    )
  )
}

volcano_server <- function(id, res, feature_col, padj_col, log2fc_col ) {
  
  moduleServer(id, function(input, output, session) {
    
    stopifnot(is.reactive(res))
    stopifnot(is.reactive(feature_col))
    stopifnot(is.reactive(padj_col))
    stopifnot(is.reactive(log2fc_col))
    
    plot <- reactive({
      plotVolcano(res(), feature_col(), padj_col(), log2fc_col())
      
      
    })
    output$plot <- renderPlot({plot()})
    output$dnld <- downloadHandler(
      filename = function() {paste0("volcano", '.png')},
      content = function(file) {ggsave(file, plot())}
    )
    
  })
}


####----Mito Control----####

module_mito_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      colourInput(NS(id,"curve_area"),"Pick coverage color", value = "#69B3A2"),
      numericInput(NS(id,"bp_num"), "mtDNA bp value", value = 3243),
      numericInput(NS(id,"cov_threshold"),"Threshold", value = 100)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id,"mtCoverage"))), type = 6)
    )
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


####----Violin----####

violin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      textInput(NS(id,"geneSelect"),"Type Gene:", value="MYC"),
      selectInput(NS(id,"metaSelect"),"Select Column:", choices = ColumnList)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id,"Violin"))), type = 6)
    )
  )
}

violin_server <- function(id) {
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
          panel.background = element_rect(fill="white", colour="white", size=0.5, 
                                          linetype="solid", color="grey73"),
          panel.border = element_blank(),
          
          #plot.title = element_text(hjust=0, color="black", size=14, face="bold.italic"),
          axis.title.x = element_text(color="black", size=14, face="plain"),
          axis.title.y = element_text(color="black", size=14, face="plain"),
          
          #legend.key=element_rect(fill='white'),
          legend.position="none",
          
          axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"),
          axis.text.x = element_text(face="bold", color="black", 
                                     size=12, angle=45, hjust=1),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=12, angle=0))+
        xlab(input$metaSelect)+ylab(input$geneSelect)
      
    })
  })
}

####----Heatmap----####

plotHeatmap <- function(df, sample_anno, sample_anno_col,
                        feature_anno, feature_anno_col,
                        rowname_switch = TRUE, colname_switch = TRUE, main = "Heatmap") {
  require(pheatmap)
  require(dplyr)
  
  # select annotation columns for sample meta and feature meta
  sample_anno <- sample_anno[sample_anno_col] # data.frame
  feature_anno <- feature_anno[, feature_anno_col] # character vector
  
  
  # draw hm
  pheatmap(
    df,
    annotation_col = sample_anno,
    labels_row = feature_anno,
    annotation_names_col = FALSE,
    angle_col = 45,
    scale = "row",
    show_rownames = rowname_switch,
    show_colnames = colname_switch,
    # cluster_rows = T,cluster_cols = T,
    # cellwidth = 30.0,
    # fontsize_row = 8,
    # fontsize_col = 8,
    # treeheight_col = 25,
    # treeheight_row = 25,
    # fontsize_number = 6,
    main = main
  )
}

heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #sidebarPanel(
    #  
    #),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(ns("plot"))), type = 6),
      downloadButton(ns("dnld"), label = "")
    )
  )
}

heatmap_server <- function(id, df, sample_anno, sample_anno_col,
                           feature_anno, feature_anno_col,
                           rowname_switch = TRUE, colname_switch = TRUE) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(sample_anno))
  stopifnot(is.reactive(sample_anno_col))
  stopifnot(is.reactive(feature_anno))
  stopifnot(is.reactive(feature_anno_col))
  
  moduleServer(id, function(input, output, session) {
    plot <- reactive({
      
      plotHeatmap(df(), sample_anno(), sample_anno_col(),
                  feature_anno(), feature_anno_col(),
                  rowname_switch = TRUE, colname_switch = TRUE,
                  main = paste("Heatmap: ", dim(df())[1], "features", dim(df())[2], "samples")
      )
    })
    output$plot <- renderPlot({
      plot()
    })
    output$dnld <- downloadHandler(
      filename = function() {
        paste0("heatmap", ".png")
      },
      content = function(file) {
        ggsave(file, plot(), width = 18, height = 10)
      }
    )
  })
}


####----Histogram----####

ggplot_truehist <- function(data, breaks = 50, title) {
  data <- as.numeric(data)
  ggplot() +
    aes(data) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = breaks,
                   fill = "cornflowerblue", color = "gray30"
    ) +
    labs(title = title) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    )
}

histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      numericInput(ns("bins"), "bins", 20, min = 1, step = 1)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(ns("hist"))), type = 6),
      downloadButton(ns("dnld"), label = "")
    )
    
  )
}

histogram_server <- function(id, df, title = reactive("Histogram")) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    output$hist <- renderPlot({
      req(is.numeric(df()))
      main <- paste0(title(), " [", input$bins, "]")
      ggplot_truehist(df(), breaks = input$bins, title = main)
      # hist(df(), breaks = input$bins, main = main)
    })
    
    output$dnld <- downloadHandler(
      filename = function() {
        paste0("histogram", ".png")
      },
      content = function(file) {
        ggsave(file, plot(), width = 18, height = 10)
      }
    )
  })
}


####----UI and Server----####

ui <- 
  navbarPage("Shiny Module Demos",
             tabPanel("Volcano",
                      fluidPage(
                        mainPanel(
                          module_volcano_ui("volcano")
                        )
                      )
             ),
             tabPanel("Volcano New",
                      fluidPage(
                        mainPanel(
                          volcano_ui("x")
                        )
                      )
             ),
             tabPanel("Mito Coverage",
                      fluidPage(
                        mainPanel(
                          module_mito_ui("mtCoverage")
                        )
                      )
             ),
             tabPanel("Violin Plot",
                      fluidPage(
                        mainPanel(
                          violin_ui("Violin")
                        )
                      )
             ),
             tabPanel("Heatmap",
                      fluidPage(
                        mainPanel(
                          heatmap_ui("heatmap")
                        )
                      )
             ),
             tabPanel("Histogram",
                      fluidPage(
                        mainPanel(
                          histogram_ui("hist")
                        )
                      )
             )
  )

server <- function(input, output, session) {
  module_volcano_server("volcano")
  volcano_server("x", reactive({res}), reactive({feature_col}),
                 reactive({padj_col}), reactive({log2fc_col}))
  module_mito_server("mtCoverage")
  violin_server("Violin")
  heatmap_server(
    "heatmap", reactive({df}), reactive({sample_anno}), reactive({sample_anno_col}),
    reactive({feature_anno}), reactive({feature_anno_col}))
  histogram_server("hist", reactive({d}))
  
}



shinyApp(ui, server)  


