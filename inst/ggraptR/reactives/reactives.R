plotInputs <- reactive({
  if (is.null(plotType())) return(NULL)
  inputs <- list(
    scatter=c('x', 'y', 'color', 'treatColorAsFactor', 'shape', 'size', 'smooth', 
              'jitter', 'alpha', 'sizeMag'),
    line=c('x', 'y', 'color', 'alpha', 'pointsOverlay'),
    bin2d=c('x', 'y', 'alpha', 'nBins', 'fill'), # position
    bar=c('x','y', 'fill', 'alpha', 'position'),
    histogram=c('x', 'fill', 'alpha', 'position', 'nBins'),
    density=c('x', 'fill', 'alpha', 'densBlackLine'),
    box=c('x', 'y', 'fill', 'alpha'),
    violin=c('x', 'y', 'fill', 'alpha'),
    density2d=c('x', 'y', 'pointsOverlay'),
    pairs=c('columns', 'color', 'fill', #'alpha',
            'ggpairsUpCont', 'ggpairsUpCombo', 'ggpairsUpDiscr',
            'ggpairsDiagCont', 'ggpairsDiagDiscr',
            'ggpairsLowCont', 'ggpairsLowCombo', 'ggpairsLowDiscr'))
  inputs$path <- inputs$line
  inputs[[plotType()]]
})
pointsOverlayInputs <- reactive(c('shape', 'size', 'sizeMag', 'alpha', 'jitter')) #'smooth'


# variables for rawDataset() -- probably not very useful
# original variables
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


# variables for dataset() -- raw or manually aggregated dataset
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

# xRange <- reactive({
#   dataset <- aggDf(); if (is.null(dataset)) return()
#   if (is.null(input$x)) return()
#   if (input$x %in% aggDfNumericVars())
#     range(dataset[input$x], na.rm=TRUE)
# })

## work-around for round error in sliderInput (for consistency w/ yRange())
xRange <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(input$x)) {
    if (input$x %in% aggDfNumericVars()) {
      range <- range(dataset[input$x], na.rm=TRUE)
    }
    range[1] <- range[1] - 1
    range[2] <- range[2] + 1
    range
  }
})

# yRange <- reactive({
#   dataset <- aggDf(); if (is.null(dataset)) return()
#   y <- y()
#   if (is.null(y)) return()
#   if (y %in% aggDfNumericVars())
#     range(dataset[[y]], na.rm=TRUE)
# })

## work-around for rounding error in sliderInput
yRange <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(y())) {
    if (y() %in% aggDfNumericVars()) {
      range <- range(dataset[[y()]], na.rm=TRUE)
    }
    range[1] <- range[1] - 1
    range[2] <- range[2] + 1
    range
  }
})

xFactorVarUniqVals <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(input$x) && input$x %in% aggDfFactorVars()) {
    unique(as.character(dataset[[input$x]]))
  }
})

yFactorVarUniqVals <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(y()) && y() %in% aggDfFactorVars()) {
    levels(dataset[[y()]])
  }
})



## conditional: facet widgets are loaded
facetWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, c('facetCol', 'facetRow', 'facetWrap','facetScale'))
})

## conditional: no facet was selected
isFacetSelected <- reactive({
  if (!facetWidgetsLoaded()) return(F)
  facetFam <- c(facetCol(), facetRow(), facetWrap())
  !(all('None' == facetFam) || all('' == facetFam) || all('.' == facetFam))
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
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(x())) {
    if (x() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  }
})


## reactive that returns a value "discrete" or "continuous"
yType <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && isXYCtrlPlot() && !is.null(y())) {
    if (y() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  }
})

## reactive that returns a value "discrete" or "continuous"
colorType <- reactive({
  dataset <- aggDf()
  if (!is.null(dataset) && !is.null(color())) {
    if (color() %in% aggDfNumericVars()) 'continuous' else 'discrete'
  } else 'none'
})

## conditional reactive: semi-automatic aggregation is on
semiAutoAggOn <- reactive({
  !is.null(plotAggMeth()) && tolower(plotAggMeth()) != 'none'
})

log <- reactiveValues(plot=NULL)

controlsLoading <- reactiveValues(ready=F)
