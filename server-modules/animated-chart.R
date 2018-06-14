animatedChart <- function(input, output, session, vals) {
  
  output$animatedChart <- renderPlotly({
  
    input$drawChart
    
    isolate({
      if(input$xCol == "" || input$yCol == "" || input$categoryCol == "") {
        return(NULL)
      }
      
      # Gather Inputs #
      # Columns
      xCol = input$xCol
      yCol = input$yCol
      idCol = input$idCol
      sizeCol = input$sizeCol
      categoryCol = input$categoryCol
      framesCol = input$framesCol
      # Motion Options
      frameDur = input$frameDur
      transitionDur = input$transitionDur
      easingFunc = input$easingFunc
      # Aesthetic Options
      minSize = input$minSize
      maxSize = input$maxSize
      
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
      
      # Labels
      xLabel = if(input$xLabel == "") xCol else input$xLabel
      yLabel = if(input$yLabel == "") yCol else input$yLabel
      colorLabel = if(input$colorLabel == "") categoryCol else input$colorLabel
      colorLabel = paste0("Color: ", colorLabel)
      sizeLabel = if(input$sizeLabel == "") sizeCol else input$sizeLabel
      sizeLabel = paste0("Size: ", sizeLabel)
      prefixLabel = paste0(input$frameLabel, " ")
      title = input$titleLabel
      subtitle = input$subtitleLabel 
      caption = input$captionLabel 
      
      p <-
        ggplot(skeleton, aes(x=x,y=y, color=color, size=size, frame=frame)) +
        geom_point() +
        scale_size(range=c(minSize, maxSize)) +
        labs(x=xLabel,
             y=yLabel,
             size=sizeLabel,
             title=title,
             subtitle=subtitle,
             caption=caption) +
        theme_bw()

      p <- ggplotly(p)
      p <- p %>% animation_opts(frame=frameDur, transition=transitionDur, easing=easingFunc, redraw=FALSE, mode="next")
      p <- p %>% animation_slider(
        currentvalue = list(
          prefix=prefixLabel
          )
        )
      p
    })
    
  })

}

observeDrawChart <- function(input, output, session, vals) {
  
  observeEvent(input$drawChart, {
    
    if(input$xCol == "" || input$yCol == "" || input$categoryCol == "") {
      shinyalert(
        title = "",
        text = "Please select all required columns",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3E3F3A",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return(NULL)
    }
    
    updateTabsetPanel(session, "mainTabset", selected="chartPage")
  })
  
}