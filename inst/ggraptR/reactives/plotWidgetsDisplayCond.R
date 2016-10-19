#### display conditional reactives

displayXCond <- reactive({
  notNulls(input$dataset, input$plotType) && !(input$plotType %in% c('pairs'))
})

displayYCond <- reactive({
  notNulls(input$dataset, input$plotType) && 
    !(input$plotType %in% c('pairs', 'histogram', 'density'))
})

displayColumnsCond <- reactive({
  notNulls(input$dataset, input$plotType) && input$plotType %in% c('pairs')
})

displayColCond <- reactive({
  notNulls(input$plotType, input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('line', 'scatter', 'path', 'pairs')
})

# for color
displayTreatAsFacVarColCond <- reactive({
  notNulls(input$plotType, input$showAesWgts) && input$showAesWgts &&
    input$plotType == 'scatter'
})

displayFillCond <- reactive({
  notNulls(input$plotType, input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('box', 'histogram', 'bar', 'density', 'violin', 'pairs')
})

displayPosCond <- reactive({
  notNulls(input$plotType, input$showAesWgts) && input$showAesWgts &&
    input$plotType %in% c('histogram', 'bar')
})

displayJitCond <- reactive({
  if (!notNulls(input$plotType, input$showAesWgts)) return(F)
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
  if (!notNulls(input$plotType, input$showAesWgts)) return(F)
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
  if (!notNulls(input$plotType, input$showAesWgts)) return(F)
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
  if (!notNulls(input$plotType, input$showAesWgts)) return(F)
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

displayCoordFlipCond <- reactive({
  !notNulls(input$plotType, input$showAesWgts) &&
    input$showAesWgts && input$plotType != 'pairs'
})

# size magnifier. Belongs to advanced control widgets
displaySizeMagCond <- reactive({
  if (!notNulls(input$plotType, input$showAesWgts)) return(F)
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
  !notNulls(input$plotType, input$showAesWgts, input$x, histMaxBinWidth()) &&
    input$x %in% finalDFNumericVars() && input$showAesWgts && input$plotType=='histogram'
})

# density black line
displayDensBlkLineCond <- reactive({
  !notNulls(input$plotType, input$showAesWgts) && 
    input$plotType=='density' & input$showAesWgts
})

# points overlay checkbox
displayPtsOverlayCond <- reactive({
  !notNulls(input$plotType, input$showAesWgts) && input$plotType %in% c('line', 'path')
})

# display additional aggregation select field
displayPlotAddAggBy <- reactive({
  !notNulls(input$showDSTypeAndPlotAggWgts, input$semiAutoAggOn) && 
    input$showDSTypeAndPlotAggWgts & semiAutoAggOn()
})

displayThemeWgts <- reactive({
  !notNulls(input$showThemeWgts, input$reactive) && !input$reactive && input$showThemeWgts
})

displayXlim <- reactive({
  !notNulls(dataset(), input$x, y(), input$showXYRangeWgts, input$reactive) &&
    !input$reactive && input$showXYRangeWgts && displayXCond()
})

displayYlim <- reactive({
  !notNulls(dataset(), input$x, y(), input$showXYRangeWgts, input$reactive,
            input$plotType) && !input$reactive && input$showXYRangeWgts && displayYCond()
})

# raw-vs-manual-agg
displayRawVsManAgg <- reactive({
  !notNulls(input$showDSTypeAndPlotAggWgts) && input$showDSTypeAndPlotAggWgts
})

displayPlotAggMeth <- reactive({
  !notNulls(input$showDSTypeAndPlotAggWgts) && input$showDSTypeAndPlotAggWgts
})

