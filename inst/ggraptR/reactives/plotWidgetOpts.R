colnamesOpts <- reactive({
  names(dataset())
})


colOpts <- reactive({
  if (is.null(dataset()) || is.null(input$plotType)) return()
  if (input$plotType %in% c('scatter', 'pairs')) {
    c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset(), 6)
    c('None', factorVars(), varsUniqValsCntLOE6)
  }
})

fillOpts <- reactive({
  if (is.null(dataset())) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset(), 6)
  c('None', factorVars(), varsUniqValsCntLOE6)
})

facetOpts <- reactive({
  if (is.null(dataset())) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset(), 6)
  c('None', factorVars(), varsUniqValsCntLOE6)
})

sizeOpts <- reactive({
  c('None', numericVars())
})

shapeOpts <- reactive({
  if (is.null(dataset())) return()
  varsUniqValsCntLOE6 <- getVarNamesUniqValsCntLOEN(dataset(), 6)
  c('None', varsUniqValsCntLOE6)
})

histMaxBinWidth <- reactive({
  dataset <- dataset()
  if (!is.null(dataset) && !is.null(input$x) && input$x %in% colnames(dataset)) {
  	round(diff(range(dataset[[input$x]], na.rm=TRUE)))
  }
})

## additional aggregation options reactive
plotAddAggByOpts <- reactive({
  if (is.null(dataset())) return()
	setdiff(origVars(), plotSemiAutoAggByBase())
})
