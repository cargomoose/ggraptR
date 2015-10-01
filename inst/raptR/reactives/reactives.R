#### variables for rawDataset() -- probably not very useful

## original variables
origVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  colnames(dataset)
})

## original factor variables
origFactorVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

## original numeric variables
origNumericVars <- reactive({
  dataset <- rawDataset(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})



#### variables for dataset() -- raw or manually aggregated dataset

## processed dataset factor variables
factorVars <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

## processed dataset numeric variables
numericVars <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})

## processed dataset variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- dataset(); if (is.null(dataset)) return()
  n <- input$nUniqValsCntThres; if (is.null(n)) return()
  getVarNamesUniqValsCntLOEN(dataset, n)
})



#### variables for finalDF()

## number of rows
nrows <- reactive({
  if (is.null(finalDF())) return()
  nrow(finalDF())
})

finalDFVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  colnames(dataset)
})

finalDFFactorVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getFactorVarNames(dataset)
})

finalDFNumericVars <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  getNumericVarNames(dataset)
})

# xRange <- reactive({
#   dataset <- finalDF(); if (is.null(dataset)) return()
#   if (is.null(input$x)) return()
#   if (input$x %in% finalDFNumericVars())
#     range(dataset[input$x], na.rm=TRUE)
# })

## work-around for round error in sliderInput (for consistency w/ yRange())
xRange <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFNumericVars())
    range <- range(dataset[input$x], na.rm=TRUE)
  range[1] <- range[1] - 1
  range[2] <- range[2] + 1
  range
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
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(y())) return()
  if (y() %in% finalDFNumericVars())
    range <- range(dataset[[y()]], na.rm=TRUE)
  range[1] <- range[1] - 1
  range[2] <- range[2] + 1
  range
})

xFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(input$x)) return()
  if (input$x %in% finalDFFactorVars()) {
    unique(as.character(dataset[[input$x]]))
  }
})

yFactorVarUniqVals <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(y())) return()
  if (y() %in% finalDFFactorVars()) {
    levels(dataset[[y()]])
  }
})




####
## conditional: plot label widgets loaded
plotLabelWidgetsLoaded <- reactive({
  wgts <- c('plotTitle', 'xLabel', 'yLabel')
  checkWidgetsLoaded(input, wgts)
})


## conditional: facet widgets are loaded
facetWidgetsLoaded <- reactive({
  wgts <- c('facetCol', 'facetRow', 'facetWrap', 'facetScale')
  return(checkWidgetsLoaded(input, wgts))
})

## conditional: no facet was selected
noFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(TRUE)
  facetFam <- c(facetCol(), facetRow(), facetWrap())
  noFacetSelected <- all('None' == facetFam) | all('' == facetFam) | all('.' == facetFam)
  return(noFacetSelected)
})

## conditional: facet grid was selected
facetGridSelected <- reactive({
  if (!facetWidgetsLoaded()) return(FALSE)
  return(facetCol() != '.' | facetRow() != '.')
})

## conditional: facet wrap was selected
facetWrapSelected <- reactive({
  if (!facetWidgetsLoaded()) return(FALSE)
  return(facetWrap() != '.')
})



## reactive that returns TRUE if plot utilizes both x and y controls
isXYCtrlPlot <- reactive({
  if (is.null(plotType())) return()
  return(plotType() %in% c('line', 'scatter', 'bar', 'box', 'path'))
})


## reactive that returns a value "discrete" or "continuous"
xType <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(x())) return()

  if (x() %in% finalDFNumericVars()) {
    return('continuous')
  } else {
    return('discrete')
  }
})


## reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (!isXYCtrlPlot()) return()
  if (is.null(y())) return()
  if (y() %in% finalDFNumericVars()) {
    return('continuous')
  } else {
    return('discrete')
  }
})

## reactive that returns a value "discrete" or "continuous"
colorType <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  if (is.null(color())) return()
  if (color() %in% finalDFNumericVars()) {
    return('continuous')
  } else {
    return('discrete')
  }
})

## conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  if (is.null(plotAggMeth())) return(FALSE)
  tolower(plotAggMeth()) != 'none'
})


