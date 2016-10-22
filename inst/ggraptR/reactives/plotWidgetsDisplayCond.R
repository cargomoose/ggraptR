# checks for display conditional reactives
displayColumnsCond <- reactive({
  !is.null(input$plotType) && input$plotType == 'pairs'
})

displayXCond <- reactive({
  !is.null(input$plotType) && !(input$plotType %in% c('pairs'))
})

displayYCond <- reactive({
  notNulls(isolate(input$plotType), x()) && 
    !(input$plotType %in% c('pairs', 'histogram', 'density'))
})

displayColCond <- reactive({
  notNulls(input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('line', 'scatter', 'path', 'pairs')
})

displayGgpairsWgtsCond <- reactive({
  !is.null(input$plotType) && input$plotType == 'pairs'
})

# for color
displayTreatAsFacVarColCond <- reactive({
  notNulls(input$showAesWgts) && input$showAesWgts &&
    input$plotType == 'scatter'
})

displayFillCond <- reactive({
  notNulls(input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('box', 'histogram', 'bar', 'density', 'violin', 'pairs')
})

displayPosCond <- reactive({
  notNulls(input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('histogram', 'bar')
})

displayJitCond <- reactive({
  if (is.null(input$showAesWgts)) return(F)
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

displayShapeCond <- reactive({
  if (is.null(input$showAesWgts)) return(F)
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
  if (is.null(input$showAesWgts)) return(F)
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
  if (anyNull(input$showAesWgts, xType(), yType())) return(F)
  display <- FALSE
  if (input$plotType=='scatter') {
    if (xType()=='continuous' && yType()=='continuous')
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

displayCoordFlipCond <- reactive({
  notNulls(input$showAesWgts) &&
    input$showAesWgts && input$plotType != 'pairs'
})

# size magnifier. Belongs to advanced control widgets
displaySizeMagCond <- reactive({
  if (is.null(input$showAesWgts)) return(F)
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
  notNulls(input$showAesWgts, input$x) && 
    input$x %in% finalDFNumericVars() && input$showAesWgts && 
    input$plotType=='histogram' && !is.null(histMaxBinWidth())
})

# density black line
displayDensBlkLineCond <- reactive({
  notNulls(input$showAesWgts) && 
    input$plotType=='density' && input$showAesWgts
})

# points overlay checkbox
displayPtsOverlayCond <- reactive({
  notNulls(input$showAesWgts) && input$plotType %in% c('line', 'path')
})

# display additional aggregation select field
displayPlotAddAggBy <- reactive({
  notNulls(input$showDSTypeAndPlotAggWgts, input$semiAutoAggOn) && 
    input$showDSTypeAndPlotAggWgts & semiAutoAggOn()
})

displayThemeWgts <- reactive({
  notNulls(input$showThemeWgts, input$reactive) && !input$reactive && input$showThemeWgts
})

displayXlim <- reactive({
  notNulls(dataset(), input$x, y(), input$showXYRangeWgts, input$reactive) &&
    !input$reactive && input$showXYRangeWgts && displayXCond()
})

displayYlim <- reactive({
  notNulls(dataset(), input$x, y(), input$showXYRangeWgts, input$reactive,
            input$plotType) && !input$reactive && input$showXYRangeWgts && displayYCond()
})

# raw-vs-manual-agg
displayRawVsManAgg <- reactive({
  notNulls(input$showDSTypeAndPlotAggWgts) && input$showDSTypeAndPlotAggWgts
})

displayPlotAggMeth <- reactive({
  notNulls(input$showDSTypeAndPlotAggWgts) && input$showDSTypeAndPlotAggWgts
})

