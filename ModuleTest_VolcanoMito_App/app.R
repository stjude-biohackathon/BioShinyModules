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
library(data.table)
library(reshape2)
library(stringr)
library(pcaMethods)
library(RColorBrewer)
library(Seurat)
library(ggthemes)
library(survminer)
library(survival)
library(MetBrewer)
library(GenomicRanges)
library(wesanderson)

####----Input Files----####

## Volcano File
L29_Vitro_Diff <- read.delim("L29_vitro_Control_vs_knockdown_diff.txt")
L29_Vitro_Diff <- mutate(L29_Vitro_Diff, log_pval = -log10(L29_Vitro_Diff$P.Value))
L29_Vitro_Diff <- mutate(L29_Vitro_Diff, log_adjpval = -log10(L29_Vitro_Diff$adj.P.Val))
volGenes <- L29_Vitro_Diff[,1]


## Mito File
GEX5<- read.table(file="GEX5Line.txt", header=T)
###descriptive
grouped <- group_by(GEX5, position)
combo<-summarise(grouped, mean=mean(uniq_cell_umiN), sd=sd(uniq_cell_umiN))

## Violin File
df_expr <- read.delim(file = "TCGA_CHOL_Expression_PatientID.txt", header = T, check.names = F)
df_meta <- read.delim(file = "TCGA_CHOL_Clinical_PatientID.txt", header = T, check.names = F) 

## Heatmap
load("MS_2.rda")
heatmap_df <- df
pca_df <- df
dist_df <- dist(t(df))
rm(df)
sample_anno <- sample_meta
sample_anno_col <- "sampleLabel"
feature_anno <- feature_meta
feature_anno_col <- "featureName"

## Histogram
histogram_df <- unlist(heatmap_df)

## DRM Data
DMR_data <- fread("autosomes.beta.txt.sorted.chr16.txt")

## Methylation Array
options(shiny.maxRequestSize=200*1024^2)
options(stringsAsFactors = F)
colorChoices = c(met.brewer(name="Derain"))

## R color Brewer palettes
palettes <- rownames(RColorBrewer::brewer.pal.info)

## sc data
sc_df <- readRDS("pbmc3k.rds")

## Survival Data
inputData_local = lung
colorChoices = c(met.brewer(name="Derain"))

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

module_volcano_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$volcano <- renderPlot({
      ggplot(data, aes(x=logFC, y=data[,input$stat])) + geom_point(col=input$dotcol)
    })
  })
}


####----Volcano - New----####

##--Plot Volcano--##

plotVolcano <- function(res, feature_col = NULL, padj_col = NULL, log2fc_col = NULL,
                        fdr = 0.05, log2fc = 1, TopHits = 10) {
  
  require(dplyr)
  require(ggplot2)
  
  # identify column names for feature_col, padj and log2fc
  if (!is.null(feature_col)) feature_col <- feature_col else feature_col <- "variable"
  if (!is.null(padj_col)) padj_col <- padj_col else padj_col <- "padj"
  if (!is.null(log2fc_col)) log2fc_col <- log2fc_col else log2fc_col <- "FC(log2)"
  if (!is.null(fdr)) fdr <- fdr else fdr <- 0.05
  if (!is.null(log2fc)) log2fc <- log2fc else log2fc <- 1
  if (!is.null(TopHits)) TopHits <- TopHits else TopHits <- 10
  
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
    dplyr::slice(1:TopHits)
  down_top10 <- df_filt_down %>%
    arrange(!!sym(padj_col)) %>%
    dplyr::slice(1:TopHits)
  
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
    sidebarPanel(
      fluidRow(
        column(4,
               numericInput(NS(id,"PvlCut"),"P. Val Cutoff", value = 0.05)
               ),
        column(4,
               numericInput(NS(id,"LFCCut"),"Log FC Cutoff", value = 1)
               ),
        column(4,
               numericInput(NS(id,"TopHits"),"Top Hits", value = 10)
        )
      )
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id, "plot"))), type = 6),
      downloadButton(NS(id, "dnld"), label = "")
    )
  )
}

volcano_server <- function(id, res, feature_col, padj_col, log2fc_col) {
  moduleServer(id, function(input, output, session) {
    
    
    plot <- reactive({
      plotVolcano(res, feature_col, padj_col, log2fc_col, input$PvlCut, input$LFCCut, input$TopHits)
    })
    output$plot <- renderPlot({plot()})
    output$dnld <- downloadHandler(
      filename = function() {paste0("volcano", '.png')},
      content = function(file) {ggsave(file, plot())}
    )
    
  })
}

####----Volcano Enhanced----####

volcanoEnh_ui <- function(id, GeneChoices) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      radioButtons(NS(id,"stat"), "Choose statistic for plot", 
                   choices=c("-Log10 P value" = "P.Value",
                             "-Log10 adjusted P value" = "adj.P.Val"), 
                   selected = "P.Value", inline = T),
      fluidRow(
        column(6,
               numericInput(NS(id,"Pvalue"), label = "Specify P-value", value = 0.05)
        ),
        column(6, 
               numericInput(NS(id,"LogFC"), label = "Specify LogFC", value = 2)
        )
      ),
      selectInput(NS(id,"VolGenesSel"), "Select Genes:", choices = GeneChoices, multiple = T)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id, "volcanoEnh"), height = "550px", width = "800px")), type = 6),
      downloadButton(NS(id,"dnld"), label = "")
    )
  )
}

volcanoEnh_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    plot <- reactive({
      
      df <- data
      
      if (length(input$VolGenesSel) > 0) {
        Features_Select <- input$VolGenesSel
      } else Features_Select <- NULL
      
      VolPlot <- EnhancedVolcano(df,
                                 lab = df[,1],
                                 x = "logFC",
                                 y = input$stat,
                                 selectLab = Features_Select,
                                 title = "Differential Expression",
                                 subtitle = NULL,
                                 FCcutoff = input$LogFC,
                                 pCutoff = input$Pvalue,
                                 legendPosition = "right",
                                 legendLabSize = 12,
                                 legendIconSize = 5.0,
                                 drawConnectors = TRUE,
                                 widthConnectors = 0.75)
      VolPlot
      
    })
    
    output$volcanoEnh <- renderPlot(plot())
    output$dnld <- downloadHandler(
      filename = function() {paste0("volcano", '.png')},
      content = function(file) {ggsave(file, plot())})
    
  })
  
}

####----Mito Coverage----####

mitoCov_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      colourInput(NS(id,"curve_area"),"Pick coverage color", value = "#69b3a2"),
      numericInput(NS(id,"bp_num"), "mtDNA bp value", value = 3243),
      numericInput(NS(id,"cov_threshold"),"Threshold", value = 100)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id,"mtCoverage"))), type = 6)
    )
  )
}

mitoCov_server <- function(id, data) {
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


####----DMR Plot----####

plotDMR <- function(data, coordinates, chrCol, startCol, endCol, intSamples, title = "") {
  # get the sample names (column names of data matrix satisfying the following regular expression)
  samples <- str_extract(colnames(data), "............_R0.C0.") # recognizes the sentrix ID format
  samples <- samples[!is.na(samples)]
  
  # string split the given coordinates to pull the chromosome, start, and stop information
  coordinates <- gsub(",", "", coordinates) # remove commas
  chr <- strsplit(coordinates, split = ":")[[1]][1]
  start <- strsplit(strsplit(coordinates, split = ":")[[1]][2], split = "-")[[1]][1]
  stop <- strsplit(strsplit(coordinates, split = ":")[[1]][2], split = "-")[[1]][2]
  
  # acquire the row indexes of the data matrix that correspond to the start and stop coordinates
  
  # cut the data to match with chromosome specificity of the given coordinates
  data <- data[data[, data[[chrCol]] == chr], ]
  
  # this is with the minimum distance from the given coordiantes to the data +1 / -1 probe
  startPos <- which(abs(data[[startCol]] - as.numeric(start)) == min(abs(data[[startCol]] - as.numeric(start)))) - 1
  endPos <- which(abs(data[[endCol]] - as.numeric(stop)) == min(abs(data[[endCol]] - as.numeric(stop)))) + 1
  
  # make sure that interest sample list is separated out from commas and remove spaces
  intSamples <- gsub(" ", "", strsplit(intSamples, ",")[[1]])
  
  # pre-processing of data
  data <- data %>%
    slice(startPos:endPos) %>%
    melt(id.vars = setdiff(colnames(data), samples), variable.name = "sampleID", value.name = "Beta") %>%
    mutate(status = ifelse(sampleID %in% intSamples, "Sample-of-Interest", "normal"))
  
  # create the line plot
  g <- ggplot(data = data, aes_string(x = startCol, y = "Beta")) +
    geom_line(aes(group = sampleID, size = status, linetype = status, colour = status)) +
    scale_colour_manual(values = c("Sample-of-Interest" = "red", "normal" = "black")) +
    scale_size_manual(values = c("Sample-of-Interest" = 0.9, "normal" = 0.2)) +
    scale_linetype_manual(values = c("Sample-of-Interest" = "solid", "normal" = "dashed")) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab(paste0("Genomic Position at ", chr)) +
    ylab("Methylation Value") +
    labs(title = title) +
    theme_bw()
  g
}

plotDMR_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      textInput(NS(id, "coordinates"), "What genomic coordinates would you like to view? Please use format chrX:start-stop ", value = "chr16:17562418-17565190"),
      textInput(NS(id, "chrCol"), "What is the name of the column with the chromosome information? (e.g. chr16)", value = "CpG_chrm"),
      textInput(NS(id, "startCol"), "What is the name of the column with the start positions? (e.g. 17562418)", value = "CpG_beg"),
      textInput(NS(id, "endCol"), "What is the name of the column with the end positions? (e.g. 17565190)", value = "CpG_end"),
      textInput(NS(id, "intSamples"), "Are there samples of interest to highlight (e.g. 203866380012_R06C01, 204873630063_R08C01)?", value = "203866380012_R06C01, 204873630063_R08C01"),
      textInput(NS(id, "title"), "What is the title of the plot?", value = "DMR plot at XYLT1"),
    ),
    mainPanel(
      titlePanel("Interactive DMR plot"),
      verbatimTextOutput(NS(id, "region")),
      p(),
      withSpinner(jqui_resizable(plotOutput(NS(id, "plot"))), type = 6)
    )
  )
}

plotDMR_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$region <- renderText(paste0("The region you have selected is: ", input$coordinates, " and the samples of interest are: ", input$intSamples))
    
    DMR_plot <- reactive({
      plotDMR(data, input$coordinates, input$chrCol, input$startCol, input$endCol, input$intSamples, title = input$title)
    })
    
    output$plot <- renderPlot({
      DMR_plot()
    })
    
  })
}

####----Methylation Array----####

MethArray_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      ## Read the methylation data file 
      fileInput(ns("methInFile") , "Upload the input file" , accept = c(".gz",".tsv") ), 
      uiOutput(ns("methData_cols_notreqd")),
      
      ## Dropdown menu that lists columns from the phenotype file for the user to select appropriate column defining the sampleIDs 
      fileInput(ns("phenoInFile") , "Upload phenotype file" ),
      uiOutput(ns("pheno_sampleIDcol_ui")),
      uiOutput(ns("pheno_colorBy_chooseBroad_ui" )),
      uiOutput(ns("ui1")),
      
      ## Enter the region of interest
      textInput(ns("RegionLocus") , "Enter the locus of interest(Ex:chr1:10-100)", value = "chr16:17562418-17565190"),
      textInput(ns("allowFlankbp") , "How many bps can the locus be extended" , value = 10 ) ,
      
      actionButton(ns("submit_click") , "Find Regions" )
      ),
    mainPanel(
      plotOutput(ns("methLinePlot" )),
      plotOutput(ns("methBoxPlot" )),
      tableOutput(ns("view"))
      ## Plot output 
      #plotOutput( linePlot_meth ),
      #plotOutput( boxPlot_meth )
      )
    )
}

MethArray_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
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
    ns <- NS(id)
    ## Automatically populates the column names identified in the phenotype file and methylation files 
    output$pheno_sampleIDcol_ui = renderUI({
      selectInput(ns("pheno_sampleIDcol") , "Select the column that denote sample id" , choices = names(phenoData()) )
    })
    
    output$pheno_colorBy_chooseBroad_ui = renderUI({  
      
      selectInput(ns("pheno_colorBy_broadCategory") , 
                   label = "How do you want to color the points?" , 
                   choices = c("IndividualSamples","Specific Column from the phenotype" ))
    })
    
    
    output$ui1 <- renderUI({
      
      phenoData_local = phenoData()
      phenoData_colnames = colnames(phenoData_local)
      sampleID_column_local = input$pheno_sampleIDcol
      sampleIDs = phenoData_local[, sampleID_column_local ]
      
      if (input$pheno_colorBy_broadCategory == 'IndividualSamples'){
        selectInput(ns('samplesToGroup'), 'Select a group of samples', choices = sampleIDs , multiple=TRUE)
      } else if ( input$pheno_colorBy_broadCategory == "Specific Column from the phenotype" ){
        selectInput(ns('pheno_colorBy'), 'Choose a column to represent', choices = phenoData_colnames )
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
      if( nrow(OverlappingRegions_CpGinformation) > 0 ){
        return( OverlappingRegions_CpGinformation )
      }else{
        return( "No overlaps found" )
      }                      
    })
    
    
    get_plotData <- eventReactive( input$submit_click , {
      
      OverlappingRegions_CpGinformation = get_overlaps()                  
      
      if( OverlappingRegions_CpGinformation != "No overlaps found" ){
        
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
      
      
    })
    
    output$view = renderTable({
      if( get_overlaps() != "No overlaps found" ){
        get_plotData() %>% head(n=3)
      } 
      else{
        return( "No overlaps found" )
      }
    })
    
    
    output$methLinePlot = renderPlot({
      plotData = get_plotData()
      
      if( get_overlaps() != "No overlaps found" ){
        
        if( input$pheno_colorBy_broadCategory == "Specific Column from the phenotype"  ){
          return(NULL)
        }else{
          
          print(plotData$colorGroup)
          print(plotData)
          
          p = ggplot( plotData , aes( x = chrBase , y = BetaValues  ) ) + 
            geom_line(aes(group = SampleID,  linetype = colorGroup , colour = colorGroup ) ) +
            scale_colour_manual(values = setNames( colorChoices[ 1:length(unique(plotData$colorGroup))] , c("Sample-of-Interest","Others") ) ) +
            scale_linetype_manual(values = setNames( c("solid","dashed") , c("Sample-of-Interest","Others") ) ) +
            coord_cartesian(ylim = c(0,1)) + 
            theme_base() +
            labs( x = "" , y = "Beta values" , title = input$Region_Locus ) + 
            theme( legend.position = "top", legend.title = element_blank() , axis.text.x = element_text( angle = 45 , vjust = 1 , hjust = 1 ) )
          
          return(p) }
      }
    })
    
    output$methBoxPlot = renderPlot({
      
      plotData = get_plotData()
      
      if( get_overlaps() != "No overlaps found" ){
        
        ggplot( plotData , aes( x = chrBase , y = BetaValues , fill = colorGroup  ) ) + 
          geom_boxplot() +
          scale_fill_manual(values = setNames( colorChoices[ 1:length(unique(plotData$colorGroup))] , unique(plotData$colorGroup) ) ) +
          coord_cartesian(ylim = c(0,1)) + 
          theme_base() +
          labs( x = "" , y = "Beta values" , title = input$Region_Locus ) + 
          theme( legend.position = "top", legend.title = element_blank() , axis.text.x = element_text( angle = 45 , vjust = 1 , hjust = 1 ) )
        
      }                                         
    })
    
  })
  
}


####----Heatmap----####

plotHeatmap <- function(df, sample_anno, sample_anno_col,
                        feature_anno, feature_anno_col,
                        rowname_switch = TRUE, colname_switch = TRUE, main = "Heatmap") {
  
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

plotHeatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      fluidRow(
        column(6,
               checkboxInput(NS(id,"ShowRowNames"),"Show Row Names", value = T)
               ),
        column(6,
               checkboxInput(NS(id,"ShowColNames"),"Show Column Names", value = T)
        )
      )
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(ns("plot"))), type = 6)
    )
  )
}

plotHeatmap_server <- function(id, df, sample_anno, sample_anno_col,
                               feature_anno, feature_anno_col) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(sample_anno))
  stopifnot(is.reactive(sample_anno_col))
  stopifnot(is.reactive(feature_anno))
  stopifnot(is.reactive(feature_anno_col))
  
  moduleServer(id, function(input, output, session) {
    Heatmap_plot <- reactive({
      plotHeatmap(df(), sample_anno(), sample_anno_col(),
                  feature_anno(), feature_anno_col(),
                  rowname_switch = input$ShowRowNames, colname_switch = input$ShowColNames,
                  main = paste("Heatmap: ", dim(df())[1], "features", dim(df())[2], "samples")
      )
    })
    output$plot <- renderPlot({
      Heatmap_plot()
    })
    #return(Heatmap_plot)
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

plotHist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      numericInput(NS(id,"bins"), "bins", 20, min = 1, step = 1)
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(NS(id,"hist"))), type = 6)
    )
  )
}

plotHist_server <- function(id, df, title = reactive("Histogram")) {
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(title))
  
  moduleServer(id, function(input, output, session) {
    hist_plot <- reactive({
      req(is.numeric(df()))
      main <- paste0(title(), " [", input$bins, "]")
      ggplot_truehist(df(), breaks = input$bins, title = main)
    })
    
    output$hist <- renderPlot({
      hist_plot()
    })
  })
}


####----Gene Violin----####

plotGeneViolin <- function(df_expr, df_meta, geneSel, colSel) {
  df_expr_sub <- df_expr[which(df_expr[, 1] == geneSel), ]
  df_expr_sub <- as.data.frame(t(df_expr_sub))
  colnames(df_expr_sub)[1] <- geneSel
  
  df_expr_sub$PatientID <- rownames(df_expr_sub)
  rownames(df_expr_sub) <- NULL
  df_expr_sub <- df_expr_sub[-1, ]
  df_expr_sub[, 1] <- as.numeric(df_expr_sub[, 1])
  
  metaMerge <- merge(df_expr_sub, df_meta)
  metaMerge[, colSel] <- as.factor(metaMerge[, colSel])
  # colnames(metaMerge)[c(2,3)] <- c("Gene","MetaColumn")
  
  # Create plot
  ggplot(metaMerge, aes(metaMerge[, colSel], metaMerge[, geneSel], fill = metaMerge[, colSel])) +
    geom_violin() +
    # geom_jitter(width = 0.3)+
    geom_boxplot(width = 0.1, fill = "white") +
    scale_fill_viridis(discrete = TRUE, option = "plasma", direction = -1, alpha = 0.6) +
    theme(
      panel.background = element_rect(
        fill = "white", colour = "white",
        #linewidth = 0.5,  ## GGplot version issue, argument not used
        size = 0.5,
        linetype = "solid", color = "grey73"
      ),
      panel.border = element_blank(),
      
      # plot.title = element_text(hjust=0, color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color = "black", size = 14, face = "plain"),
      axis.title.y = element_text(color = "black", size = 14, face = "plain"),
      
      # legend.key=element_rect(fill='white'),
      legend.position = "none",
      axis.line = element_line(linetype = "solid",
                               #linewidth = 0.5,   ## GGplot version issue, argument not used
                               size = 0.5, 
                               colour = "black"),
      axis.text.x = element_text(
        face = "bold", color = "black",
        size = 12, angle = 45, hjust = 1
      ),
      axis.text.y = element_text(
        face = "bold", color = "black",
        size = 12, angle = 0
      )
    ) +
    xlab(colSel) +
    ylab(geneSel)
}

GeneViolin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      textInput(ns("geneSelected"), "Select Gene:", value = "MYC"),
      uiOutput(ns("metaSelect"))
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotOutput(ns("Violin"))), type = 6)
    )
    
  )
}

GeneViolin_server <- function(id, df_expr, df_meta) {
  moduleServer(id, function(input, output, session) {
    GeneViolin_plot <- reactive({
      plotGeneViolin(df_expr, df_meta, input$geneSelected, input$metaSelected)
    })
    
    ns <- NS(id)
    output$metaSelect <- renderUI({
      selectInput(ns("metaSelected"), "Select Column:", choices = colnames(df_meta))
    })
    
    output$Violin <- renderPlot({
      GeneViolin_plot()
    })
    #return(GeneViolin_plot)
  })
}


####----PCA Plot----####

ggplot_pca <- function(df, sample_anno, sample_anno_col, title = NULL) {
  require(pcaMethods)
  require(ggplot2)
  
  data <- as.matrix(df)
  class(data) <- "numeric"
  
  labels <- as.matrix(sample_anno[sample_anno_col])
  
  pc1 <- pcaMethods::pca(t(data), scale = "pareto")
  pc1merged <- merge(cbind(labels, t(data)),
                     pcaMethods::scores(pc1),
                     by = 0
  )
  ggplot(pc1merged, aes(PC1, PC2, colour = !!sym(sample_anno_col))) +
    geom_point() +
    stat_ellipse() +
    xlab(paste("PC1", round((pc1@R2[1] * 100), digits = 1), "% of the variance")) +
    ylab(paste("PC2", round((pc1@R2[2] * 100), digits = 1), "% of the variance")) +
    ggtitle(label = title)
}

plotPCA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(jqui_resizable(plotOutput(ns("plot"))), type = 6)
  )
}

plotPCA_server <- function(id, df, sample_anno, sample_anno_col) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(df))
    stopifnot(is.reactive(sample_anno))
    stopifnot(is.reactive(sample_anno_col))
    
    PCA_plot <- reactive({
      ggplot_pca(df(), sample_anno(), sample_anno_col())
    })
    output$plot <- renderPlot({
      PCA_plot()
    })
    #return(PCA_plot)
  })
}

####----PCA Palette----####

ggplotly_3Dpca <- function(df, sample_anno, sample_anno_col, pal, title) {
  data <- as.matrix(df)
  class(data) <- "numeric"
  
  labels <- as.matrix(sample_anno[sample_anno_col])
  
  pc1 <- pcaMethods::pca(t(data), nPcs = 3, scale = "pareto")
  pc1merged <- merge(cbind(labels, t(data)),
                     pcaMethods::scores(pc1),
                     by = 0
  )
  
  plot_ly(pc1merged,
          x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d",
          color = sample_anno$sampleLabel, colors = pal,
          text = sample_anno$sampleName, hoverinfo = "text"
  ) %>%
    layout(title = title, scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ))
}

plot3DPCA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(ns("palette"), "Choose color palette", choices = palettes, selected = "Dark2"),
      textInput(ns("title"), "Title graph", value = ""),
    ),
    mainPanel(
      withSpinner(jqui_resizable(plotlyOutput(ns("plot"))), type = 6)
    )
  )
}

plot3DPCA_server <- function(id, df, sample_anno, sample_anno_col) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(df))
    stopifnot(is.reactive(sample_anno))
    stopifnot(is.reactive(sample_anno_col))
    
    PCA3D_plot <- reactive({
      ggplotly_3Dpca(df(), sample_anno(), sample_anno_col(), input$palette, input$title)
    })
    output$plot <- renderPlotly({
      PCA3D_plot()
    })
    #return(PCA3D_plot)
  })
}


####----Dist Plot----####

plotDist <- function(d, sample_anno, sample_anno_col,
                     rowname_switch = TRUE, colname_switch = TRUE) {
  # select annotation columns for sample meta and feature meta
  sample_anno <- sample_anno[sample_anno_col] # data.frame
  
  # check distance matrix
  if (class(d) != "dist") {
    stop("Provided `d` is not a `dist` object.")
  }
  if (length(d) != sum(1:(nrow(sample_anno) - 1))) {
    stop("Provided `d` is not the corrected length based on provided `sample_anno`.")
  }
  
  # plot
  pheatmap(
    as.matrix(d),
    clustering_distance_rows = d,
    clustering_distance_cols = d,
    annotation_col = sample_anno,
    annotation_row = sample_anno,
    annotation_names_col = FALSE,
    # display_numbers = TRUE,
    angle_col = 45,
    scale = "none",
    show_rownames = rowname_switch,
    show_colnames = colname_switch,
    # cluster_rows = T,cluster_cols = T,
    # cellwidth = 30.0,
    # fontsize_row = 8,
    # fontsize_col = 8,
    # treeheight_col = 25,
    # treeheight_row = 25,
    # fontsize_number = 6,
    main = paste(
      "Sample-to-sample distance matrix: ",
      nrow(sample_anno), "samples"
    )
  )
  if (isTRUE(save)) {
    ggsave(TITLE,
           path = folder,
           device = "png",
           plot = p3, dpi = 300, width = 17, height = 10
    )
  }
}

plotDist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(jqui_resizable(plotOutput(ns("plot"))), type = 6)
  )
}

plotDist_server <- function(id, d, sample_anno, sample_anno_col,
                            rowname_switch = TRUE, colname_switch = TRUE) {
  moduleServer(id, function(input, output, session) {
    Dist_plot <- reactive({
      plotDist(d(), sample_anno(), sample_anno_col(),
               rowname_switch = TRUE, colname_switch = TRUE
      )
    })
    output$plot <- renderPlot({
      Dist_plot()
    })
    #return(Dist_plot)
  })
}

####----KM Plot----####

plotKM_ui <- function(id, data) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(ns("time_var"), "Choose a column that has time variable", choices = colnames(data)),
      selectInput(ns("event_var"), "Choose a column that has event variable", choices = colnames(data)),
      selectInput(ns("select_surv_model"), "Choose a model" , choices = c("KM","Cox-proportional")),
      selectInput(ns("group_var"), "Choose a group variable", choices = colnames(data)),
      selectInput(ns("select_group_var_type"),"Choose the type of the variable", choices = c("categorical","continuous")),
      #conditionalPanel(
      #  condition = "input.select_surv_model == 'KM'",
      #  selectInput("select_group_var_type", "Choose the type of the variable",  ## can add when Cox Regression is working
      #  choices = c("categorical","continuous")))
      actionButton(ns("click_submit"), label = "Run survival")
    ),
    mainPanel(
      jqui_resizable(plotOutput(ns("survPlot"))),
      tableOutput(ns("survTable"))
    )
  )
}

plotKM_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
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
        timeVar <- input$time_var
        eventVar <- input$event_var
        groupVar <- input$group_var
        groupVarType <- input$select_group_var_type
        
        datTmp = data %>% dplyr::select( timeVar , eventVar , groupVar ) %>% data.frame 
        datTmp[ , "time_for_surv" ] = as.numeric( datTmp[ , timeVar ] )
        datTmp[ , "event_for_surv" ] = as.numeric( datTmp[ , eventVar ] )
        if( input$select_group_var_type == "continuous" ){
          
          datTmp$numeric_val = datTmp[ ,groupVar ]
          datTmp$surv_group = ifelse(datTmp$numeric_val >= median(datTmp$numeric_val), ">Median", "<Median" )
          #return(datTmp)
          
        }else if( input$select_group_var_type == "categorical" ){
          
          datTmp$surv_group = factor( datTmp[ , groupVar ] )
          #return(datTmp)
        }
        
        datTmp
        
      }
      
    })
    
    output$survTable = renderTable({ 
      survDat() %>% head  
    })
    
    output$survPlot = renderPlot({
      survData_local = survDat() 
      groupVar <- input$group_var
      
      #if( input$select_surv_model == "KM" ){
        
        sfit = survfit( Surv( time_for_surv , event_for_surv  ) ~ surv_group , data = survData_local )
        names(sfit$strata) = gsub("surv_group=","",names(sfit$strata) )
        #surv_colors = setNames( colorChoices[ length(unique(survData_local$surv_group)) ] , unique(survData_local$surv_group) )
        
        p = ggsurvplot(  sfit, 
                         data = survData_local,
                         risk.table = TRUE,
                         legend.title = groupVar,
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
        #return(p)
        p
      #}
      
    })
    
  })
  
}

#####----scUMAP----####

process_df <- function(df, resolution=0.5) {
  df <- NormalizeData(df)
  df <- FindVariableFeatures(df, selection.method = "vst", nfeatures = 3000)
  df <- ScaleData(df, verbose = FALSE)
  df <- RunPCA(df, npcs = 15, verbose = FALSE)
  df <- RunUMAP(df, reduction = "pca", dims = 1:15)
  df <- FindNeighbors(df, reduction = "pca", dims = 1:15)
  df <- RunTSNE(df)
  df <- FindClusters(df, resolution = resolution)
  return(df)
}

scUMAP_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      numericInput(ns("parameter"), "Select Resolution", value = 0.5)
    ),
    mainPanel(
      #withSpinner(jqui_resizable(plotOutput(ns("UMAP"))), type = 6)
      jqui_resizable(plotOutput(ns("UMAP")))
    )
  )
}

scUMAP_server <- function(id, df) {
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    scUMAP_plot <- reactive({
      # Processing data
      df_processed <- process_df(df(), input$parameter)
      
      # UMAP
      DimPlot(df_processed, reduction = "umap", cols = ggsci::pal_igv()(50))
    })
    output$UMAP <- renderPlot({
      scUMAP_plot()
    })
    #return(scUMAP_plot)
  })
}

####----UI and Server----####

#"#5050FFFF" "#CE3D32FF" "#749B58FF" "#F0E685FF" "#466983FF" "#BA6338FF"
#"#5DB1DDFF" "#802268FF" "#6BD76BFF" "#D595A7FF" "#924822FF" "#837B8DFF"
#"#C75127FF" "#D58F5CFF" "#7A65A5FF" "#E4AF69FF" "#3B1B53FF" "#CDDEB7FF"
#"#612A79FF" "#AE1F63FF" "#E7C76FFF" "#5A655EFF" "#CC9900FF" "#99CC00FF"
#"#A9A9A9FF" "#CC9900FF" "#99CC00FF" "#33CC00FF" "#00CC33FF" "#00CC99FF"
#"#0099CCFF" "#0A47FFFF" "#4775FFFF" "#FFC20AFF" "#FFD147FF" "#990033FF"
#"#991A00FF" "#996600FF" "#809900FF" "#339900FF" "#00991AFF" "#009966FF"
#"#008099FF" "#003399FF" "#1A0099FF" "#660099FF" "#990080FF" "#D60047FF"
#"#FF1463FF" "#00D68FFF"

ui <- 
  navbarPage("Shiny Module Demos",
             tags$style(HTML("
                             .navbar-default .navbar-nav > li > a[data-value='Volcano'] {background-color: #CC9900FF;  color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Volcano 2'] {background-color: #1F77B4FF;   color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Enhanced Volcano'] {background-color: #FF7F0EFF;  color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Mito Coverage'] {background-color: #2CA02CFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='DMR Plot'] {background-color: #8C564BFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Methylation Array'] {background-color: #9467BDFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Heatmap'] {background-color: #CC9900FF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Histogram'] {background-color: #1F77B4FF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Gene Violin'] {background-color: #FF7F0EFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='PCA'] {background-color: #2CA02CFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='PCA Palette'] {background-color: #8C564BFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='Dist Plot'] {background-color: #9467BDFF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='KM Survival Plot'] {background-color: #CC9900FF; color:white}
                             .navbar-default .navbar-nav > li > a[data-value='scUMAP'] {background-color: #1F77B4FF; color:white}
                             .navbar-default .navbar-nav > li[class=active]    > a {background-color: #7F7F7FFF; color:white}
                             ")),
             tabPanel("Volcano",
                      fluidPage(
                        mainPanel(
                          module_volcano_ui("volcano")
                        )
                      )
             ),
             tabPanel("Volcano 2",
                      fluidPage(
                        mainPanel(
                          volcano_ui("x")
                        )
                      )
             ),
             tabPanel("Enhanced Volcano",
                      fluidPage(
                        mainPanel(
                          volcanoEnh_ui("VolcanoEnh", GeneChoices = volGenes)
                        )
                      )
             ),
             tabPanel("Mito Coverage",
                      fluidPage(
                        mainPanel(
                          mitoCov_ui("mtCoverage")
                        )
                      )
             ),
             tabPanel("DMR Plot",
                      fluidPage(
                        mainPanel(
                          plotDMR_ui("DMRmethPlot")
                        )
                      )
             ),
             tabPanel("Methylation Array",
                      fluidPage(
                        mainPanel(
                          MethArray_ui("MethArray")
                        )
                      )
             ),
             tabPanel("Heatmap",
                      fluidPage(
                        mainPanel(
                          plotHeatmap_ui("plotHeatmap")
                        )
                      )
             ),
             tabPanel("Histogram",
                      fluidPage(
                        mainPanel(
                          plotHist_ui("hist")
                        )
                      )
             ),
             tabPanel("Gene Violin",
                      fluidPage(
                        mainPanel(
                          GeneViolin_ui("Violin")
                        )
                      )
             ),
             tabPanel("PCA",
                      fluidPage(
                        mainPanel(
                          plotPCA_ui("pca")
                        )
                      )
             ),
             tabPanel("PCA Palette",
                      fluidPage(
                        mainPanel(
                          plot3DPCA_ui("pcaPal")
                        )
                      )
             ),
             tabPanel("Dist Plot",
                      fluidPage(
                        mainPanel(
                          plotDist_ui("dist")
                        )
                      )
             ),
             tabPanel("KM Survival Plot",
                      fluidPage(
                        mainPanel(
                          plotKM_ui("kmPlot",inputData_local)
                        )
                      )
             ),
             tabPanel("scUMAP",
                      fluidPage(
                        mainPanel(
                          scUMAP_ui("scUMAP")
                        )
                      )
             )
  )


server <- function(input, output, session) {
  module_volcano_server("volcano", data = L29_Vitro_Diff)
  volcano_server("x", L29_Vitro_Diff, feature_col = "gene",
                 padj_col = "adj.P.Val", log2fc_col = "logFC")
  volcanoEnh_server("VolcanoEnh",data = L29_Vitro_Diff)
  mitoCov_server("mtCoverage",combo)
  plotDMR_server("DMRmethPlot",DMR_data)
  plotHeatmap_server("plotHeatmap", reactive({heatmap_df}), reactive({sample_anno}), reactive({sample_anno_col}),
                     reactive({feature_anno}), reactive({feature_anno_col}))
  plotHist_server("hist", reactive({histogram_df}))
  GeneViolin_server("Violin", df_expr, df_meta)
  plotPCA_server("pca", reactive({pca_df}), reactive({sample_anno}),reactive({sample_anno_col}))
  plot3DPCA_server("pcaPal", reactive({pca_df}), reactive({sample_anno}),reactive({sample_anno_col}))
  plotDist_server("dist", reactive({dist_df}), reactive({sample_anno}), reactive({sample_anno_col}))
  plotKM_server("kmPlot",inputData_local)
  scUMAP_server("scUMAP",reactive({sc_df}))
  MethArray_server("MethArray")
}


shinyApp(ui, server)


