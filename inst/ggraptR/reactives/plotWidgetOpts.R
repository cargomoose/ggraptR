#### widget options
## dataset colnames options reactive
colnamesOpts <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    names(dataset)
  }
})

## color options reactive
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

## fill options reactive
fillOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  fillOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  fillOpts      
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  facetOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  facetOpts
})

## size options reactive
sizeOpts <- reactive({
  c('None', numericVars())
})

## shape options reactive
shapeOpts <- reactive({
  dataset <- dataset()
  if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  #c('None', setdiff(varsUniqValsCntLOE6, numericVars()))
  c('None', varsUniqValsCntLOE6)
})

## histogram max bin width reactive
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
