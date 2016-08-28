#### display conditional reactives

## display x condition reactive
displayXCond <- reactive({
  if (!is.null(input$plotType)) {
    !(input$plotType %in% c('pairs'))
  }
})

## display y condition reactive
displayYCond <- reactive({
  if (!is.null(input$plotType)) {
    !(input$plotType %in% c('histogram', 'density', 'pairs'))
  }
})

## display pairs condition reactive
displayColumnsCond <- reactive({
  if (!is.null(input$plotType)) {
    input$plotType %in% c('pairs')
  }
})



## display color condition reactive
displayColCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('line', 'scatter', 'path', 'pairs')
  }
})

## display treat-as-a-factor-variable (for color) condition reactive
displayTreatAsFacVarColCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('scatter')
  }
})

## display fill condition reactive
displayFillCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('box', 'histogram', 'bar', 'density', 'pairs')
  }
})


## display position condition reactive
displayPosCond <- reactive({
  if (!is.null(input$plotType) && !is.null(!input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('histogram', 'bar')
  }
})

## display shape condition reactive
displayShapeCond <- reactive({
  if (is.null(input$plotType) || is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display size condition reactive
displaySizeCond <- reactive({
  if (is.null(input$plotType) || is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display smooth condition reactive
displaySmthCond <- reactive({
  if (is.null(input$plotType) || is.null(xType()) || is.null(yType()) 
      || is.null(input$showAesWgts)) return()
  
  display <- FALSE
  if (input$plotType=='scatter') {
    if (xType()=='continuous' & yType()=='continuous')
        display <- input$showAesWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) {
      if (xType()=='continuous' & yType()=='continuous')
      display <- input$showAesWgts
    }
  }
  display  
})

## display jitter condition reactive 
displayJitCond <- reactive({
  if (is.null(input$plotType) || is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

displayCoordFlipCond <- reactive({
  !is.null(input$plotType) && !is.null(input$showAesWgts) &&
    input$showAesWgts && input$plotType != 'pairs'
})

## display size magnifier condition reactive (belongs to advanced control widgets)
displaySizeMagCond <- reactive({
  if (is.null(input$plotType) || is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {    
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display bin width condition reactive
displayBinWidthCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$x) && !is.null(input$showAesWgts)
      && input$x %in% finalDFNumericVars() && input$showAesWgts) {
    input$plotType=='histogram'
  }
})

## display density black line condition reactive
displayDensBlkLineCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts)) {
    input$plotType=='density' & input$showAesWgts
  }
})


## display points overlay checkbox condition reactive
displayPtsOverlayCond <- reactive({
  if (!is.null(input$plotType)) {
    input$plotType %in% c('line', 'path')
  }
})

## display additional aggregation select field condition reactive
displayPlotAddAggBy <- reactive({
  if (!is.null(input$showDSTypeAndPlotAggWgts) && !is.null(semiAutoAggOn())) {
    input$showDSTypeAndPlotAggWgts & semiAutoAggOn()
  }
})

## display xlim condition reactive
displayXlim <- reactive({
  if (!is.null(dataset()) && !is.null(input$x) && !is.null(y())
      && !is.null(input$showXYRangeWgts)) {
    input$showXYRangeWgts
  }
})

## display ylim condition reactive
displayYlim <- reactive({
  if (!is.null(dataset()) && !is.null(input$x) && !is.null(y()) && !is.null(input$plotType)
      && !input$plotType=='histogram' && !is.null(input$showXYRangeWgts)) {
    input$showXYRangeWgts
  }
})

## display raw-vs-manual-agg condition reactive
displayRawVsManAgg <- reactive({
  if (!is.null(input$showDSTypeAndPlotAggWgts)) {
    input$showDSTypeAndPlotAggWgts
  }
})


## display plot aggregation method reactive
displayPlotAggMeth <- reactive({
  if (!is.null(input$showDSTypeAndPlotAggWgts)) {
    input$showDSTypeAndPlotAggWgts
  }
})

displayThemeWgts <- reactive({
  if (!is.null(input$showThemeWgts)) {
    input$showThemeWgts
  }
})
