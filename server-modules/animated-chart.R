animatedChart <- function(input, output, session, vals) {
  
  output$animatedChart <- renderPlotly({
  
    if(input$xCol == "" || input$yCol == "" || input$categoryCol == "") {
      return(NULL)
    }
    
    df <- vals$datadf

    xForm <- as.formula(paste0("~",input$xCol))
    yForm <- as.formula(paste0("~",input$yCol))
    sizeForm <- as.formula(paste0("~",input$sizeCol))
    
    colorCol <- if(input$categoryCol != "") input$categoryCol else input$xCol
    colorForm <- as.formula(paste0("~", colorCol))
    
    frameForm <- as.formula(paste0("~", input$framesCol))
    
    plot_ly(data=df, x=xForm, y=yForm, size=sizeForm, color=colorForm, frame=frameForm, type = 'scatter', mode = 'markers')
  
  })

}