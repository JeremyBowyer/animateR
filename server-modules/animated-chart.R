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

      # Create Opacity column
      skeleton$opacity <- 0
      skeleton[!rowSums(is.na(skeleton[, 3:4])), "opacity"] <- 1
      
      # Fill NAs
      fillNAs <- function(x) {
        x <- na.locf(x, fromLast=TRUE, na.rm=FALSE)
        x <- na.locf(x, fromLast=FALSE, na.rm=FALSE)
        return(x)
      }
      
      for(col in c(xCol, yCol, sizeCol, categoryCol)) {
        skeleton[[col]] <- unlist(aggregate(skeleton[[col]], by=list(rev(skeleton[[idCol]])), FUN=fillNAs, simplify=FALSE)[["x"]])
      }

      # Create Chart
      xForm <- as.formula(paste0("~",input$xCol))
      yForm <- as.formula(paste0("~",input$yCol))
      xyFormula <- as.formula(paste0(yCol,"~",xCol))
      idForm <- as.formula(paste0("~",input$idCol))
      sizeForm <- as.formula(paste0("~",input$sizeCol))
      colorForm <- if(input$categoryCol != "") as.formula(paste0("~", input$categoryCol)) else as.formula(paste0("~", input$xCol))
      frameForm <- as.formula(paste0("~", input$framesCol))
    
      
      p <- plot_ly(data=skeleton, x=xForm, y=yForm, size=sizeForm, color=colorForm, text=colorForm, frame=frameForm, type = 'scatter', mode = 'markers')
      # p <- 
      #   ggplot(skeleton, aes(x=skeleton[[input$xCol]],y=input$yCol, color = input$categoryCol)) +
      #   geom_point(aes(frame = input$framesCol)) + theme_bw()
      # 
      # p <- ggplotly(p)
      p <- p %>% animation_opts(frame=input$frameDur, transition=input$transitionDur, easing=input$easingFunc, redraw=FALSE, mode="next")
      p
    })
    
  })

}