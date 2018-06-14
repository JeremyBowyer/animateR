animatedChart <- function(input, output, session, vals) {
  
  output$animatedChart <- renderPlotly({
  
    input$drawChart
    
    isolate({
      if(input$xCol == "" || input$yCol == "" || input$categoryCol == "") {
        return(NULL)
      }
    
      # Gather Inputs
      xCol = input$xCol
      yCol = input$yCol
      idCol = input$idCol
      sizeCol = input$sizeCol
      categoryCol = input$categoryCol
      framesCol = input$framesCol
      frameDur = input$frameDur
      transitionDur = input$transitionDur
      easingFunc = input$easingFunc
      
      # Subset uploaded data to ignore NAs in relevant columns
      df <- vals$datadf[, unique(c(xCol, yCol, idCol, sizeCol, categoryCol, framesCol))]
      df <- df[!is.na(df[,framesCol]) & !is.na(df[, idCol]), ]

      # Create skeleton dataframe
      frames <- unique(as.character(df[,framesCol]))
      ids <- unique(as.character(df[,idCol]))
      
      skeleton <- merge(frames, ids, all=TRUE)
      names(skeleton) <- c(framesCol, idCol)

      # Add data to skeleton dataframe
      skeleton <- merge(skeleton, df, all.x=TRUE)
      skeleton <- skeleton[order(skeleton[,idCol], skeleton[,framesCol]), ]

      # Create Chart
      x <- skeleton[[xCol]]
      y <- skeleton[[yCol]]
      id <- skeleton[[idCol]]
      size <- skeleton[[sizeCol]]
      color <- if(categoryCol != "") skeleton[[categoryCol]] else skeleton[[xCol]]
      frame <- skeleton[[framesCol]]
      
      p <-
        ggplot(skeleton, aes(x=x,y=y, color=color, size=size, frame=frame)) +
        geom_point() + theme_bw()

      p <- ggplotly(p)
      p <- p %>% animation_opts(frame=frameDur, transition=transitionDur, easing=easingFunc, redraw=FALSE, mode="next")
      p
    })
    
  })

}