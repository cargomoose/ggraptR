colnamesOpts <- reactive({
  names(dataset())
})

xOpts <- reactive({
  pType <- plotType()
  if (anyNull(isolate(dataset()), pType)) return()
  if (pType %in% c('violin', 'box', 'bar')) categoricalVars() else numericVars()
})

yOpts <- reactive({
  x <- x()  # the only reactive trigger. y options must be different from selected x value
  # so it does not make sense to render y before x is ready
  isolate({
    if (is.null(plotType()) || (displayXCond() && is.null(x))) return()
    setdiff(numericVars(), if (displayXCond()) x) })
})

colOpts <- reactive({
  if (is.null(dataset()) || is.null(plotType())) return()
  if (plotType() %in% c('scatter', 'pairs')) {
    c('None', names(dataset()))
  } else if (plotType() %in% c('line', 'path')) {
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
