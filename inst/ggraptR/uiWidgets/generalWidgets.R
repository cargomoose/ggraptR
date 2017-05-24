# dataset drop-down options 
output$datasetNameCtrl <- renderUI({
  selectInput("datasetName", "Dataset", choices=rawDatasetNames())
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
  input$evalConsoleBtn  # dependancy
  
  isolate({
      console_input <- gsub('(?<=[^<])<-', '<<-', input$console, perl = T)
      res <- capture.output(eval(parse(text = console_input)))
      paste(tryCatch({
        cat(res, fill = T)  # check renderText will be able to process it with inner cat()
        res
    }, error = function(e) {
      if (grepl("cannot be handled by 'cat'", e$message)) {
        if (is.list(res)) str(res) else '[Success]'
      } else {
        stop(e$message)
      }
    }), collapse = '\n')
  })
})
