chartPage <- function() {
  tabPanel(
    "Chart",
    value="chartPage",
    plotlyOutput("animatedChart")
  )
}