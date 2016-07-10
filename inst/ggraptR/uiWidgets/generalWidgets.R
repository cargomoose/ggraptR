## dataset drop-down options 
output$datasetCtrl <- renderUI({
  #for now we will select diamonds by default
  selectedDataFrame = "diamonds"
  if ((exists(gDefaultDataFrame)) && gDefaultDataFrame != "" && gDefaultDataFrame %in% rawDatasetNames())
  {
    #variable set by the ggraptR("x") instantiation parameter (see ggraptR.R)
    selectedDataFrame = gDefaultDataFrame
  }
  selectInput("dataset", "Choose a dataset:",
              selected = selectedDataFrame,
              choices = rawDatasetNames())
})

## reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})


## upon-manual-submit button
output$submitCtrl <- renderUI({
  shinyBS::bsButton("submit", label="Submit", icon=icon("refresh"), type = "action", block=TRUE)
})