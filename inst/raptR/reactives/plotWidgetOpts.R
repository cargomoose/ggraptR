#### widget options
## x options reactive
xOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## y options reactive
yOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  names(dataset)
})

## color options reactive
colOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  if (is.null(input$plotType)) return()
  
  if (input$plotType=='scatter') {
    colOpts <- c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    #colOpts <- c('None', factorVars())
    varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
    colOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  }
  
  colOpts
})

## fill options reactive
fillOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  fillOpts <- c('None', factorVars(), varsUniqValsCntLOE6)
  fillOpts      
})

## facet options reactive
facetOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
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
  dataset <- dataset(); if (is.null(dataset)) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset, 6)
  #vars <- setdiff(varsUniqValsCntLOE6, numericVars())
  #c('None', vars)
  c('None', varsUniqValsCntLOE6)
})

## histogram max bin width reactive
histMaxBinWidth <- reactive({
  if (is.null(input$x)) return()
  dataset <- dataset(); if (is.null(dataset)) return()
  if (!(input$x %in% colnames(dataset))) return()
  maxBinWidth <- round(diff(range(dataset[[input$x]], na.rm=TRUE)))
  maxBinWidth 
})

## additional aggregation options reactive
plotAddAggByOpts <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  setdiff(origVars(), plotSemiAutoAggByBase())
})
