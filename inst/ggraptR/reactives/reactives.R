#### variables for rawDataset() -- probably not very useful

## original variables
origVars <- reactive({
  dataset <- rawDataset()
  if (!is.null(dataset)) {
    colnames(dataset)
  }
})

## original factor variables
origFactorVars <- reactive({
  dataset <- rawDataset()
  if (!is.null(dataset)) {
    getFactorVarNames(dataset)
  }
})

## original numeric variables
origNumericVars <- reactive({
  dataset <- rawDataset()
  if (!is.null(dataset)) {
    getNumericVarNames(dataset)
  }
})



#### variables for dataset() -- raw or manually aggregated dataset

## processed dataset factor variables
factorVars <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    getFactorVarNames(dataset)
  }
})

## processed dataset numeric variables
numericVars <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    getNumericVarNames(dataset)
  }
})

## processed dataset variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    n <- input$nUniqValsCntThres
    if (!is.null(n)) {
      getVarNamesUniqValsCntLOEN(dataset, n)
    }
  }
})



#### variables for finalDF()

## number of rows
nrows <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset)) {
    nrow(dataset)
  }
})

finalDFVars <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset)) {
    colnames(dataset)
  }
})

finalDFFactorVars <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset)) {
    getFactorVarNames(dataset)
  }
})

finalDFNumericVars <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset)) {
    getNumericVarNames(dataset)
  }
})

# xRange <- reactive({
#   dataset <- finalDF(); if (is.null(dataset)) return()
#   if (is.null(input$x)) return()
#   if (input$x %in% finalDFNumericVars())
#     range(dataset[input$x], na.rm=TRUE)
# })

## work-around for round error in sliderInput (for consistency w/ yRange())
xRange <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(input$x)) {
    if (input$x %in% finalDFNumericVars())
      range <- range(dataset[input$x], na.rm=TRUE)
    range[1] <- range[1] - 1
    range[2] <- range[2] + 1
    range
  }
})

# yRange <- reactive({
#   dataset <- finalDF(); if (is.null(dataset)) return()
#   y <- y()
#   if (is.null(y)) return()
#   if (y %in% finalDFNumericVars())
#     range(dataset[[y]], na.rm=TRUE)
# })

## work-around for rounding error in sliderInput
yRange <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(y())) {
    if (y() %in% finalDFNumericVars())
      range <- range(dataset[[y()]], na.rm=TRUE)
    range[1] <- range[1] - 1
    range[2] <- range[2] + 1
    range
  }
})

xFactorVarUniqVals <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(input$x) && input$x %in% finalDFFactorVars()) {
    unique(as.character(dataset[[input$x]]))
  }
})

yFactorVarUniqVals <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(y()) && y() %in% finalDFFactorVars()) {
    levels(dataset[[y()]])
  }
})




####
## conditional: plot label widgets loaded
plotLabelWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, widgets = c('plotTitle', 'xLabel', 'yLabel'))
})


## conditional: facet widgets are loaded
facetWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, widgets = c('facetCol', 'facetRow', 'facetWrap', 'facetScale'))
})

## conditional: no facet was selected
noFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(TRUE)
  facetFam <- c(facetCol(), facetRow(), facetWrap())
  all('None' == facetFam) | all('' == facetFam) | all('.' == facetFam)
})

## conditional: facet grid was selected
facetGridSelected <- reactive({
  facetWidgetsLoaded() && (facetCol() != '.' | facetRow() != '.')
})

## conditional: facet wrap was selected
facetWrapSelected <- reactive({
  facetWidgetsLoaded() && facetWrap() != '.'
})



## reactive that returns TRUE if plot utilizes both x and y controls
isXYCtrlPlot <- reactive({
  if (!is.null(plotType())) {
    return(plotType() %in% c('line', 'scatter', 'bar', 'box', 'path'))
  }
})


## reactive that returns a value "discrete" or "continuous"
xType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(x())) {
    return(if (x() %in% finalDFNumericVars()) 'continuous' else 'discrete')
  }
})


## reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && isXYCtrlPlot() && !is.null(y())) {
    return(if (y() %in% finalDFNumericVars()) 'continuous' else 'discrete')
  }
})

## reactive that returns a value "discrete" or "continuous"
colorType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(color())) {
    return(if (color() %in% finalDFNumericVars()) 'continuous' else 'discrete')
  }
})

## conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  if (is.null(plotAggMeth())) return(FALSE)
  tolower(plotAggMeth()) != 'none'
})

log <- reactiveValues()
