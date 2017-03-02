separatePlotInputs <- reactive({
  if (is.null(plotTypes())) return()
  inputs <- lapply(plotTypes(), function(pType) {
    flattenList(getDefinedPlotInputs())[[pType]]
  })
  names(inputs) <- plotTypes()
  inputs
})

plotInputs <- reactive({
  unique(unlist(separatePlotInputs()))
})


# variables for dataset() -- raw or manually aggregated dataset
categoricalVars <- reactive({
  if (is.null(dataset())) return()
  unique(c(getIsFactorVarNames(dataset()), 
           if (nrow(dataset()) > 30) getVarNamesUniqValsCntLOEN(dataset())))
})

numericVars <- reactive({
  if (is.null(dataset())) return()
  setdiff(colnames(dataset()), categoricalVars())
})

# variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    n <- 6 # magic. Previos value was 'input$nUniqValsCntThres'
    if (!is.null(n)) {
      getVarNamesUniqValsCntLOEN(dataset)
    }
  }
})


# variables for aggDf()
aggDfFactorVars <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset)) {
    getIsFactorVarNames(dataset)
  }
})

aggDfNumericVars <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset)) {
    getIsNumericVarNames(dataset)
  }
})


# facets
facetWidgetsLoaded <- reactive({
  !any(sapply(c('facetCol', 'facetRow', 'facetWrap','facetScale'), 
              function(widget) is.null(input[[widget]])))
})

isFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(F)
  facetFam <- c(facetCol(), facetRow(), facetWrap())
  !(facetFam[1] %in% c('None', '', '.') && length(unique(facetFam)) == 1)
})

facetGridSelected <- reactive({
  facetWidgetsLoaded() && any(c(facetCol(), facetRow()) != '.')
})

facetWrapSelected <- reactive({
  facetWidgetsLoaded() && facetWrap() != '.'
})



# reactive that returns a value "discrete" or "continuous"
xType <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(x())) {
    if (x() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  }
})


# reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && 'y' %in% plotInputs() && !is.null(y())) {
    if (y() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  }
})

# reactive that returns a value "discrete" or "continuous"
colorType <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(color())) {
    if (color() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  } else 'none'
})

# conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  !is.null(plotAggMeth()) && tolower(plotAggMeth()) != 'none'
})
