optionsPage <- function() {
  
  tabPanel(
    "Options",
    value = "options",
    sidebarLayout(
      sidebarPanel(
        useShinyalert(),
        fileInput(
          "csvfile",
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
            )
          ),
        tags$hr(),
        tabsetPanel(
          tabPanel(
            "Column Selection",
            tags$div(
              h3("Column Selection"),
              selectInput("idCol", "Select ID column", choices=list("", "Please upload data first.")),
              selectInput("yCol", "Select Y-axis column", choices=list("", "Please upload data first."), selected=""),
              selectInput("xCol", "Select X-axis column", choices=list("", "Please upload data first."), selected=""),
              selectInput("categoryCol", "Select Color column", choices=list("", "Please upload data first.")),
              selectInput("sizeCol", "Select Size column", choices=list("", "Please upload data first.")),
              selectInput("framesCol", "Select Frames column", choices=list("", "Please upload data first.")),
              style="padding: 5px 20px 20px 20px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
              )
            ),
          tabPanel(
            "Chart Options",
            tags$div(
              h3("Chart Options"),
              numericInput("frameDur", "Frame Duration", value=1000, min=0, step=1),
              numericInput("transitionDur", "Transition Duration", value=1000, min=0, step=1),
              selectInput("easingFunc", "Select Easing Function", choices=easingFuncList),
              style="padding: 5px 20px 20px 20px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
              )
          ),
          tabPanel(
            "Change Data",
            tabsetPanel(
              tabPanel(
                "Date Aggregation",
                tags$div(
                  h3("Date Aggregation"),
                  conditionalPanel(
                    condition = "output.aggCheck",
                    actionLink("aggClear", "Undo Aggregation", style="color: #f12828;"),
                    tags$br()
                  ),
                  selectInput("dateAggDateCol", "Select Date column", choices=list("", "Please upload data first.")),
                  textInput("dateAggDateColFormat", "Format dates are in (check 'Data Preview' tab)", "%m/%d/%Y"),
                  selectInput("groupByCols", "Select Category column(s) (optional)", multiple = TRUE, choices=list("", "Please upload data first.")),
                  selectInput("aggregationLevel", "Aggregation Level (your data must be more granular than selected level)", choices = aggregationLevelList),
                  selectInput("aggregationFunc", "Aggregation Function", choices = aggregationFuncList),
                  conditionalPanel(
                    condition = "!output.aggCheck",
                    actionButton("aggregateData", "Aggregate Data")
                  ),
                  tags$br(),
                  style="padding: 5px 20px 20px 20px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
                )
              ),
              tabPanel(
                "Filters",
                tags$div(
                  h3("Filters"),
                  tags$p("Warning: Filtering by one column will be applied to entire dataset, and will affect all subsequent analysis, including Metric Dive tab."),
                  conditionalPanel(
                    condition = "output.filtersCheck",
                    actionButton("applyFilters", "Apply Filters", icon("filter"), style="padding: 5px 10px 5px 10px;"),
                    tags$br(),
                    actionLink("filterClear", "Clear All Filters", style="color: #f12828;"),
                    tags$hr()
                  ),
                  tags$div(
                    selectInput("filterSelected", label = NULL, width = "275px", choices = filterList),
                    style= "display:inline-block; vertical-align: top;"
                  ),
                  tags$div(actionButton("addFilter", "Add Filter", style="padding:4px;font-size: 75%;"),style="display:inline-block; vertical-align: top;"),
                  tags$div(id="filters"),
                  id="filter-container", style="padding: 5px 20px 20px 20px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
                ),
                tags$hr()
              ),
              tabPanel(
                "Transformations",
                tags$div(
                  h3("Metric Transformations"),
                  conditionalPanel(
                    condition = "output.transformationsCheck",
                    actionButton("applyTransformations", "Create Transformations", icon("recycle"), style="padding: 5px 10px 5px 10px;"),
                    tags$br(),
                    actionLink("transformationsClear", "Clear All Transformations", style="color: #f12828;")
                  ),
                  tags$div(
                    selectInput(
                      "transformationselected",
                      label = NULL,
                      width = "275px",
                      choices = transformationList),
                    style= "display:inline-block; vertical-align: top;"
                  ),
                  tags$div(
                    actionButton(
                      "addTransformation",
                      "Add Transformation",
                      style="padding:4px;font-size: 75%;"
                    ),
                    style="display:inline-block; vertical-align: top;"
                  ),
                  tags$div(id="transformations"),
                  id="transformation-container",
                  style="padding: 5px 20px 20px 20px; background: #e4dfd6; border: 1px solid #b5b3b0; margin: 10px 0 0 0; border-radius: 5px;"
                ),
                tags$hr()
              )
            )
          )
          ),
        tags$hr(),
        actionButton("drawChart", "Draw Chart", style="color: #fff; background-color: rgb(2, 140, 7); border: solid 1px #005a03;"),
        tags$head(
          tags$script(HTML('Shiny.addCustomMessageHandler("conditionalFormatting", function(message) { eval(message.value); });')),
          tags$script(src="loading_screen.js"),
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
          )
        ),
      mainPanel(
        plotlyOutput("animatedChart")
        )
      )
    )
}