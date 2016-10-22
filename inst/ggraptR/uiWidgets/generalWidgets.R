## dataset drop-down options 
output$datasetCtrl <- renderUI({
  isolate({
    stopifnot(state$initialDf %in% rawDatasetNames())
    selectInput("dataset", "Dataset", choices=rawDatasetNames(), selected=state$initialDf)
  })
})

## reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})

## upon-manual-submit button
output$submitCtrl <- renderUI({
  bsButton("submit", label="Submit", icon=icon("refresh"), type="action", block=TRUE)
})

output$plotLog <- renderText({
  paste(log$plot, collapse='<hr>')
})
