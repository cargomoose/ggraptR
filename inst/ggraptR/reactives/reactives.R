plotInputsRegister <- reactive({
  inputs <- list(
    scatter=c('x', 'y', 'color', 'treatAsFacVarCol', 'shape',
              'size', 'smooth', 'jitter', 'alpha', 'sizeMag'),
    line=c('x', 'y', 'color', 'colorAsFactor', 'alpha'),
    pointsOverlay=c('shape', 'shapeAsFactor', 'size', 'smooth', 
                     'alpha', 'sizeMag'),
    bar=c('x','y', 'fill', 'fillAsFactor', 'alpha', 'position'),
    histogram=c('x', 'fill', 'fillAsFactor', 'alpha', 'position', 'binWidth'),
    density=c('x', 'fill', 'fillAsFactor', 'alpha', 'densBlkLineCond'),
    box=c('x', 'y', 'fill', 'fillAsFactor', 'alpha'),
    violin=c('y', 'xAsFactor', 'fill', 'fillAsFactor', 'alpha'),
    pairs=c('columns', 'color', 'fill', 'alpha',
                'ggpairsUpCont', 'ggpairsUpCombo', 'ggpairsUpDiscr',
                'ggpairsDiagCont', 'ggpairsDiagDiscr',
                'ggpairsLowCont', 'ggpairsLowCombo', 'ggpairsLowDiscr'))
  inputs$path <- inputs$line
  inputs
})


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
    getIsFactorVarNames(dataset)
  }
})

## original numeric variables
origNumericVars <- reactive({
  dataset <- rawDataset()
  if (!is.null(dataset)) {
    getIsNumericVarNames(dataset)
  }
})


#### variables for dataset() -- raw or manually aggregated dataset
categoricalVars <- reactive({
  if (is.null(dataset())) return()
  unique(c(getIsFactorVarNames(dataset()), 
           if (nrow(dataset()) > 30) getVarNamesUniqValsCntLOEN(dataset(), 6)))
})

numericVars <- reactive({
  if (is.null(dataset())) return()
  setdiff(colnames(dataset()), categoricalVars())
})

## variables with less than or equal to N unique values
varsUniqValsCntLOEN <- reactive({
  dataset <- dataset()
  if (!is.null(dataset)) {
    n <- 6 # magic. Previos value was 'input$nUniqValsCntThres'
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
    getIsFactorVarNames(dataset)
  }
})

finalDFNumericVars <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset)) {
    getIsNumericVarNames(dataset)
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
    if (input$x %in% finalDFNumericVars()) {
      range <- range(dataset[input$x], na.rm=TRUE)
    }
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
    if (y() %in% finalDFNumericVars()) {
      range <- range(dataset[[y()]], na.rm=TRUE)
    }
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
  checkWidgetsLoaded(input, c('plotTitle', 'xLabel', 'yLabel'))
})


## conditional: facet widgets are loaded
facetWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, c('facetCol', 'facetRow', 'facetWrap','facetScale'))
})

## conditional: no facet was selected
noFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(TRUE)
  facetFam <- c(facetCol(), facetRow(), facetWrap())
  all('None' == facetFam) || all('' == facetFam) || all('.' == facetFam)
})

## conditional: facet grid was selected
facetGridSelected <- reactive({
  facetWidgetsLoaded() && (facetCol() != '.' || facetRow() != '.')
})

## conditional: facet wrap was selected
facetWrapSelected <- reactive({
  facetWidgetsLoaded() && facetWrap() != '.'
})



## reactive that returns TRUE if plot utilizes both x and y controls
isXYCtrlPlot <- reactive({
  if (!is.null(plotType())) {
    plotType() %in% c('line', 'scatter', 'bar', 'box', 'path')
  }
})


## reactive that returns a value "discrete" or "continuous"
xType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(x())) {
    if (x() %in% finalDFNumericVars()) 'continuous' else 'discrete'
  }
})


## reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && isXYCtrlPlot() && !is.null(y())) {
    if (y() %in% finalDFNumericVars()) 'continuous' else 'discrete'
  }
})

## reactive that returns a value "discrete" or "continuous"
colorType <- reactive({
  dataset <- finalDF()
  if (!is.null(dataset) && !is.null(color())) {
    if (color() %in% finalDFNumericVars()) 'continuous' else 'discrete'
  } else 'none'
})

## conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  !is.null(plotAggMeth()) && tolower(plotAggMeth()) != 'none'
})

generateCodeReactive <- reactive({
  generateCode(buildPlot())
})

log <- reactiveValues(plot=NULL)

plotLoading <- reactiveValues()
