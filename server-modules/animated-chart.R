animatedChart <- function(input, output, session, vals) {
  
  themeGenerator <- function(theme, fontSize, font) {
    
    if (theme == "") return(theme_classic(fontSize, font))
    
    switch(theme,
           grey = { return(theme_grey(fontSize, font)) },
           bw = { return(theme_bw(fontSize, font)) },
           linedraw = { return(theme_linedraw(fontSize, font)) },
           light = { return(theme_light(fontSize, font)) },
           dark = { return(theme_dark(fontSize, font)) },
           minimal = { return(theme_minimal(fontSize, font)) },
           classic = { return(theme_classic(fontSize, font)) },
           void = { return(theme_void(fontSize, font)) }
         )
    
    
  }
  
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
      user_theme <- themeGenerator(input$theme, input$fontSize, input$fontFamily)

      # Layout
      width = if(input$chartWidth == "") 1920 else as.numeric(input$chartWidth)
      height = if(input$chartHeight == "") 900 else as.numeric(input$chartHeight)
      legendPosition = input$legendPosition
      
      # Subset uploaded data to ignore NAs in relevant columns
      df <- vals$datadf[, unique(c(xCol, yCol, idCol, sizeCol, categoryCol, framesCol))]
      df <- df[!is.na(df[,framesCol]) & !is.na(df[, idCol]), ]

      # Create skeleton dataframe
      frames <- unique(df[,framesCol])
      ids <- unique(df[,idCol])
      
      skeleton <- merge(frames, ids, all=TRUE)
      names(skeleton) <- c(framesCol, idCol)
      
      # Add data to skeleton dataframe
      skeleton <- merge(skeleton, df, all.x=TRUE)
      skeleton <- skeleton[order(skeleton[,idCol], skeleton[,framesCol]), ]
      
      # If frame is date column, format it properly
      if(input$dateCheckBox) {
        dateFormat <- input$dateColFormat
            
        if(length(grep("%d", dateFormat)) == 0){
          fullDateFormat <- paste0('%d-', dateFormat)
          skeleton[,framesCol] <- paste0("1-", as.character(skeleton[, framesCol]))
        } else {
          fullDateFormat <- dateFormat
        }
        skeleton[,framesCol] <- as.Date(as.character(skeleton[, framesCol]), format = fullDateFormat)
      }

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
      colorLabelCol = if(input$colorLabel == "") categoryCol else input$colorLabel
      colorLabel = paste0("Color: ", colorLabelCol)
      sizeLabelCol = if(input$sizeLabel == "") sizeCol else input$sizeLabel
      sizeLabel = paste0("Size: ", sizeLabelCol)
      prefixLabel = paste0(input$frameLabel, "Frame: ")
      title = input$titleLabel
      subtitle = input$subtitleLabel 
      caption = input$captionLabel 
      
      # Tooltip
      tooltip = paste0('<span style="font-weight: 600; font-size: 15px; text-align: center;">', id, "</span><br>",
                       '<span style="font-weight: 600;">', xLabel, ":</span> ", x, "<br>",
                       '<span style="font-weight: 600;">', yLabel, ":</span> ", y, "<br>",
                       '<span style="font-weight: 600;">', sizeLabelCol, ":</span> ", size, "<br>",
                       '<span style="font-weight: 600;">', colorLabelCol, ":</span> ", color, "<br>",
                       '<span style="font-weight: 600;">', prefixLabel, "</span>", frame
                       )
      
      p <-
        ggplot(skeleton, aes(x=x,y=y, color=color, size=size, frame=frame, text=tooltip)) +
        geom_point() +
        scale_size(range=c(minSize, maxSize)) +
        labs(x=xLabel,
             y=yLabel,
             size=sizeLabel,
             #color=colorLabel,
             title=title,
             subtitle=subtitle,
             caption=caption) +
        theme(legend.position=legendPosition) + 
        user_theme

      p <- ggplotly(p, width=width, height=height, tooltip="text")
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

