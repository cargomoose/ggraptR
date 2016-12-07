# dataset drop-down options 
output$datasetCtrl <- renderUI({
  isolate({
    initDf <- sys.frame(1)$initialDf
    stopifnot(is.null(initDf) || initDf %in% rawDatasetNames())
    selectInput("dataset", "Dataset", choices=rawDatasetNames(), initDf)
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
