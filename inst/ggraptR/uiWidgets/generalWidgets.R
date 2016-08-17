## dataset drop-down options 
output$datasetCtrl <- renderUI({
    if (gDefaultDataFrame != "" && gDefaultDataFrame %in% rawDatasetNames())
    {
      selectedDataFrame <- gDefaultDataFrame
    }
    else
    {
      # select diamonds by default
      selectedDataFrame <- "diamonds"
    }
    # variable set by the ggraptR("x") instantiation parameter (see ggraptR.R)
  
  selectInput("dataset", "Choose a dataset:", 
              choices = rawDatasetNames(),
              selected<-selectedDataFrame)
})

## reactive  option
output$reactiveCtrl <- renderUI({
  checkboxInput("reactive", label="Enable reactivity", value=TRUE)
})


## upon-manual-submit button
output$submitCtrl <- renderUI({
  shinyBS::bsButton("submit", label="Submit", icon=icon("refresh"), type = "action", 
                    block=TRUE)
})

output$plotLog <- renderText({
  paste(log$plot, collapse='<hr>')
})
