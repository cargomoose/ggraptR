# dataset drop-down options 
output$datasetNameCtrl <- renderUI({
  selectInput("datasetName", "Dataset", choices=rawDatasetNames())
})

output$datasetOptionsCtrl <- renderUI({
  bsButton("datasetOptionsBtn", label=NULL, type="action", icon=icon("cog"))
})

# reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})

# upon-manual-submit button
output$submitCtrl <- renderUI({
  bsButton("submit", label="Submit", icon=icon("play-circle-o"), type="action", block=TRUE)
})

output$pTypesWarnBtnCtrl <- renderUI({
  if (is.null(plotTypesWarn())) return()
  bsButton("pTypesWarnBtn", label=NULL, icon=icon("warning"), type="action",
           style="warning")
})

output$pTypesWarnModalMessageCtrl <- renderText({
  plotTypesWarn()
})

#### code tab ####
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
    
    tryCatch({
      res <- capture.output(eval(parse(text = console_input)))
    }, error = function(e) {
      if (grepl('cannot change value of locked binding for', e$message)) {
        e$message <- "Did not find assigned value in .GlobalEnv"
      }
      stop(e$message)
    })
    
    paste(tryCatch({
      cat(res, fill = T)  # check renderText will be able to process it with inner cat()
      res
    }, error = function(e) {
      browser()
      if (grepl("cannot be handled by 'cat'", e$message)) {
        if (is.list(res)) str(res) else '[Success]'
      } else {
        stop(e$message)
      }
    }), collapse = '\n')
  })
})
