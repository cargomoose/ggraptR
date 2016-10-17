colnamesOpts <- reactive({
  names(dataset())
})

xOpts <- reactive({
  if (is.null(dataset()) || is.null(plotType())) return()
  if (plotType() %in% c('violin', 'box', 'bar')) categoricalVars() else numericVars()
})

yOpts <- reactive({
  if (is.null(dataset()) || is.null(plotType()) || 
      (displayXCond() && is.null(x()))) return()
  setdiff(numericVars(), if (displayXCond()) x())
})

colOpts <- reactive({
  if (is.null(dataset()) || is.null(input$plotType)) return()
  if (input$plotType %in% c('scatter', 'pairs')) {
    c('None', names(dataset()))
  } else if (input$plotType %in% c('line', 'path')) {
    c('None', categoricalVars())
  }
})

fillOpts <- reactive({
  if (is.null(dataset())) return()
  c('None', categoricalVars())
})

facetOpts <- reactive({
  if (is.null(dataset())) return()
  c('None', categoricalVars())
})

sizeOpts <- reactive({
  c('None', numericVars())
})

shapeOpts <- reactive({
  if (is.null(dataset())) return()
  c('None', varsUniqValsCntLOEN())
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
