colnamesOpts <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    names(dataset)
  }
})

colOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  if (is.null(input$plotType)) return()

  res <- if (input$plotType %in% c('scatter', 'pairs')) {
    c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
    c('None', factorVars(), varsUniqValsCntLOE6)
  }
  
  res
})

fillOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  fillOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  fillOpts      
})

facetOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  facetOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  facetOpts
})

sizeOpts <- reactive({
  c('None', numericVars())
})

shapeOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  #c('None', setdiff(varsUniqValsCntLOE6, numericVars()))
  c('None', varsUniqValsCntLOE6)
})

histMaxBinWidth <- reactive({
  dataset <- dataset()
  if (!is.null(dataset) && !is.null(input$x) && input$x %in% colnames(dataset)) {
	maxBinWidth <- round(diff(range(dataset[[input$x]], na.rm=TRUE)))
	maxBinWidth
  }
})

## additional aggregation options reactive
plotAddAggByOpts <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
	  setdiff(origVars(), plotSemiAutoAggByBase())
  }
})
