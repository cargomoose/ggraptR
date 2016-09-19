## dataset drop-down options 
output$datasetCtrl <- renderUI({
  # select diamonds by default
  selectedDataFrame <- 
    if (state$gDefaultDataFrame != "" && state$gDefaultDataFrame %in% rawDatasetNames()) 
    # variable set by the ggraptR("x") instantiation parameter (see ggraptR.R)
      state$gDefaultDataFrame else "diamonds"
  
  selectInput("dataset", "Choose a dataset:", 
              choices=rawDatasetNames(),
              selected=selectedDataFrame)
})

## reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})


## upon-manual-submit button
output$submitCtrl <- renderUI({
  shinyBS::bsButton("submit", label="Submit", icon=icon("refresh"), type="action", 
                    block=TRUE)
})

output$plotLog <- renderText({
  paste(log$plot, collapse='<hr>')
})
