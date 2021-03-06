library(shiny)
library(shinyalert) # https://github.com/daattali/shinyalert
library(plotly)
library(dplyr)
library(reshape2)
library(quantmod)
library(XLConnect)
library(DT)
library(lubridate)
library(chron)
library(htmlwidgets)
options(shiny.deprecation.messages=FALSE)
options(stringsAsFactors = FALSE)

source("global.R", local=TRUE)
source("server-modules/filters.R", local=TRUE)
source("server-modules/transformations.R", local=TRUE)
source("server-modules/download-custom-data.R", local=TRUE)
source("server-modules/uploaded-data.R", local=TRUE)
source("server-modules/methods.R", local=TRUE)
source("server-modules/aggregation.R", local=TRUE)
source("server-modules/data-preview.R", local=TRUE)
source("server-modules/conditions.R",local=TRUE)
source("server-modules/animated-chart.R",local=TRUE)

# Define Functions
source("https://raw.githubusercontent.com/JeremyBowyer/Quintile-Function/master/Quintile_Function.R")

shinyServer(function(input, output, session) {
  

  ###########
  # Methods #
  ###########
  loadMethods(input, output, session, vals)
  
  ##################
  # Event Handlers #
  ##################
  # File Uploaded
  observeUploadedData(input, output, session, vals)
  
  ###########################
  # Option Panel Conditions #
  ###########################
  loadConditions(input, output, session, vals)
  
  ##################
  # Filter Section #
  ##################
  # Add filter Buttons
  observeAddFilter(input, output, session, vals)
  # Apply Filters Button
  observeApplyFilters(input, output, session, vals)
  
  #############################
  # Aggregate by Date Section #
  #############################
  # Aggregate Data Button
  observeAggregateData(input, output, session, vals)
  
  ############################
  # Clear Filter & Clear Agg #
  ############################
  observeClearFilters(input, output, session, vals)
  observeClearAgg(input, output, session, vals)
  
  ##########################
  # Transformation Section #
  ##########################
  # Add Transformation Button
  observeAddTransformation(input, output, session, vals)
  # Create Transformations Button
  observeCreateTransformations(input, output, session, vals)
  # Clear Transformations Button
  observeClearTransformations(input, output, session, vals)
  
  #####################
  # Various Mechanics #
  #####################
  observeDownloadCustomData(input, output, session, vals)
  
  #######################
  # Data Preview Screen #
  #######################
  dataPreview(input, output, session, vals)
  
  #########
  # Chart #
  #########
  animatedChart(input, output, session, vals)
  observeDrawChart(input, output, session, vals)
  
})