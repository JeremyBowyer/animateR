library(shiny)
library(shinyalert) # https://github.com/daattali/shinyalert
library(shinythemes)
library(plotly)
library(dplyr)
library(XLConnect)
library(DT)

source("global.R", local=TRUE)
source("ui-modules/ui-options.R", local=TRUE)
source("ui-modules/ui-chart.R", local=TRUE)

shinyUI(
  navbarPage(
    title = "animateR",
    theme = shinytheme("sandstone"),
    fluid = TRUE,
    id = "mainTabset",
    optionsPage(),
    chartPage()
  )
)
