# dataset drop-down options 
output$datasetNameCtrl <- renderUI({
  opts <- rawDatasetNames()
  isolate({
    initDf <- c(uploadedDfName(), getInitialArg('initialDf'))[1]
    selectInput("datasetName", "Dataset", choices=opts, initDf)
  })
})

# reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})

# upon-manual-submit button
output$submitCtrl <- renderUI({
  bsButton("submit", label="Submit", icon=icon("refresh"), type="action", block=TRUE)
})

output$plotLog <- renderText({
  paste(reactVals$log, collapse='<hr>')
})

output$evalConsoleBtn <- renderUI({
  bsButton("evalConsoleBtn", label="Evaluate", icon=icon("play-circle-o"), type="action")
})

output$consoleCtrl <- renderText({
  input$evalConsoleBtn  # dependency
  
  isolate({
      console_input <- gsub('(?<=[^<])<-', '<<-', input$console, perl = T)
      res <- isolate(eval(parse(text = console_input)))
      tryCatch({
        cat(res, fill = T)  # check renderText will be able to process it with inner cat()
        res
    }, error = function(e) {
      if (grepl("cannot be handled by 'cat'", e$message)) {
        if (is.list(res)) capture.output(str(res)) else '[Success]'
      } else {
        stop(e$message)
      }
    })
  })
})
