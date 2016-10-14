colnamesOpts <- reactive({
  names(dataset())
})

xOpts <- reactive({
  if (is.null(dataset()) || is.null(plotType()) || is.null(colnamesOpts())) return()
  if (plotType() %in% c('violin', 'box', 'bar')) return(categoricalVars())
  if (plotType() %in% c('histogram', 'density')) 
    numericVars() else colnamesOpts()
})

yOpts <- reactive({
  if (is.null(dataset()) || is.null(plotType()) || is.null(colnamesOpts())) return()
  setdiff(if (plotType() %in% c('violin', 'box', 'bar'))
    numericVars() else colnamesOpts(), 
    if (isolate(displayXCond())) isolate(xOpts())[1])
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
