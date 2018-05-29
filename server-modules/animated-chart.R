animatedChart <- function(input, output, session, vals) {
  
  output$animatedChart <- renderPlotly({
  
    if(input$xCol == "" || input$yCol == "" || input$categoryCol == "") {
      return(NULL)
    }
    
    df <- vals$datadf

    xform <- as.formula(paste0("~",input$xCol))
    yform <- as.formula(paste0("~",input$yCol))
    sizeform <- as.formula(paste0("~",input$sizeCol))
    
    colorcol <- if(input$categoryCol != "") input$categoryCol else input$xCol
    colorForm <- as.formula(paste0("~", colorcol))
    
    frameForm <- as.formula(paste0("~", input$framesCol))
    
    plot_ly(data=df, x=xform, y=yform, size=sizeForm, color=colorForm, frame=frameForm, type = 'scatter', mode = 'markers')
  
  })

}