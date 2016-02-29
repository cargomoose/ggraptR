## dataset drop-down options 
output$datasetCtrl <- renderUI({
  selectInput("dataset", "Choose a dataset:", 
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