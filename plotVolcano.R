# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# author: Max Qiu (maxqiu@unl.edu)

# Documentation
#' R Shiny module to generate volcano plot
#'
#' @param id A string.
#' @param df A dataframe.
#' @returns A Shiny module.
#' @examples
#' module.demo()

#### Library needed #### ----------
# TODO Add here all the library needed for the module (ggplot2, tidyr, ...)
library(ggplot2)
library(shiny)

#### Function needed to work #### ----------
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


#### UI function of the module #### ----------
# TODO Add here the UI function of the module

volcano_ui <- function(id) {

        fluidRow(
                column(11, plotOutput(NS(id, "plot"))),
                column( 1, downloadButton(NS(id, "dnld"), label = ""))
        )

}

#### Server function of the module #### ----------
# TODO Add here the server function of the module

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

#### Demo functionof the module #### ----------
# TODO Add here the demo function of the module

stat <- read.delim("./example_data/L29_vitro_Control_vs_knockdown_diff.txt")


volcano_demo <- function() {

        res = stat
        feature_col = "gene"
        padj_col = "adj.P.Val"
        log2fc_col = "logFC"

        ui <- fluidPage(volcano_ui("x"))
        server <- function(input, output, session) {
                volcano_server("x", reactive({res}), reactive({feature_col}),
                               reactive({padj_col}), reactive({log2fc_col}))
        }
        shinyApp(ui, server)
}

# TODO list for this template
# TODO rename all the function as modulename_function
# TODO add minimal data for testing
# TODO update documentation
