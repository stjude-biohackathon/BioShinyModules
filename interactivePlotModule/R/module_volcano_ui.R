####----Volcano Functions----####

#' Module Volcano ui
#'
#' @param id parameter
#'
#' @return volcano plot
#' @export
#'
#' @examples
#' module_volcano_ui
#' TO DO need examples here
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
      ggplot(df, aes(x=logFC, y=df[,input$stat])) + geom_point(col=input$dotcol)
    })
  })
}
