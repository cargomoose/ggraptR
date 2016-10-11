#### display conditional reactives

displayXCond <- reactive({
  if (!is.null(input$plotType)) {
    !(input$plotType %in% c('pairs'))
  }
})

displayYCond <- reactive({
  if (!is.null(input$plotType)) {
    !(input$plotType %in% c('histogram', 'density', 'pairs'))
  }
})

displayColumnsCond <- reactive({
  if (!is.null(input$plotType)) {
    input$plotType %in% c('pairs')
  }
})

displayColCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('line', 'scatter', 'path', 'pairs')
  }
})

# for color
displayTreatAsFacVarColCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType == 'scatter'
  }
})

displayFillCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('box', 'histogram', 'bar', 'density', 'violin', 'pairs')
  }
})

displayPosCond <- reactive({
  if (!is.null(input$plotType) && !is.null(!input$showAesWgts) && input$showAesWgts) {
    input$plotType %in% c('histogram', 'bar')
  }
})

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

# size magnifier. Belongs to advanced control widgets
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

displayBinWidthCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$x) && !is.null(input$showAesWgts)
      && input$x %in% finalDFNumericVars() && input$showAesWgts) {
    input$plotType=='histogram'
  }
})

# density black line
displayDensBlkLineCond <- reactive({
  if (!is.null(input$plotType) && !is.null(input$showAesWgts)) {
    input$plotType=='density' & input$showAesWgts
  }
})

# points overlay checkbox
displayPtsOverlayCond <- reactive({
  if (!is.null(input$plotType)) {
    input$plotType %in% c('line', 'path')
  }
})

# display additional aggregation select field
displayPlotAddAggBy <- reactive({
  if (!is.null(input$showDSTypeAndPlotAggWgts) && !is.null(semiAutoAggOn())) {
    input$showDSTypeAndPlotAggWgts & semiAutoAggOn()
  }
})

displayXlim <- reactive({
  if (!is.null(dataset()) && !is.null(input$x) && !is.null(y())
      && !is.null(input$showXYRangeWgts)) {
    input$showXYRangeWgts
  }
})

displayYlim <- reactive({
  if (!is.null(dataset()) && !is.null(input$x) && !is.null(y()) && 
      !is.null(input$plotType)
      && !input$plotType=='histogram' && !is.null(input$showXYRangeWgts)) {
    input$showXYRangeWgts
  }
})

# raw-vs-manual-agg
displayRawVsManAgg <- reactive({
  if (!is.null(input$showDSTypeAndPlotAggWgts)) {
    input$showDSTypeAndPlotAggWgts
  }
})

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
