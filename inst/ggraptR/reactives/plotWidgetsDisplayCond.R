aesReady <- reactive({  # serves as trigger too
  !is.null(displayYCond()) && showAesWgts()
})

pairsReady <- reactive({
  !is.null(displayGgpairsWgtsCond())
})

extraBlocksReady <- reactive({
  !is.null(plotType())
})

displayXCond <- reactive({
  !is.null(plotType()) && !(plotType() %in% c('pairs'))
})

displayYCond <- reactive({
  # isolate plotType because switching scatter to violin results premature
  # drawing of y-axis based on old x() value - Sepal.Length
  
  # x() is the only reactive trigger. y options must be different from selected x value
  # so it does not make sense to render y before x is ready
  !is.null(displayXCond()) && !is.null(x()) &&
    isolate(!is.null(plotType()) && !(plotType() %in% c('pairs', 'histogram', 'density')))
})

displayGgpairsWgtsCond <- reactive({
  !is.null(plotType()) && plotType() == 'pairs'
})

displayColCond <- reactive({
  pairsReady() && aesReady() && 
    isolate(plotType() %in% c('line', 'scatter', 'path', 'pairs'))
})

displayTreatAsFacVarColCond <- reactive({
  aesReady() && isolate(plotType() == 'scatter')
})

displayFillCond <- reactive({
  pairsReady() && aesReady() && isolate(
    plotType() %in% c('histogram', 'bar', 'box', 'density', 'violin', 'pairs'))
})

displayPosCond <- reactive({
  aesReady() && isolate(plotType() %in% c('histogram', 'bar'))
})

displayJitCond <- reactive({
  aesReady() && isolate(
    plotType() == 'scatter' || (plotType() == 'line' && pointsOverlay()))
})

displayShapeCond <- reactive({
  aesReady() && isolate(
    plotType() == 'scatter' || (plotType() %in% c('line', 'path') && pointsOverlay()))
})

displaySizeCond <- reactive({
  displayShapeCond()
})

displaySmthCond <- reactive({
  displayJitCond()
})

displayCoordFlipCond <- reactive({
  aesReady() && isolate(plotType() != 'pairs')
})

displaySizeMagCond <- reactive({
  displayShapeCond()
})

displayBinWidthCond <- reactive({
  aesReady() && isolate(plotType()) == 'histogram'
})

# density black line
displayDensBlkLineCond <- reactive({
  aesReady() && isolate(plotType() == 'density')
})

displayPointsOverlayCond <- reactive({
  aesReady() && isolate(plotType() %in% c('line', 'path'))
})

 
displayFacetCond <- reactive({
  extraBlocksReady() && isolate(plotType() != 'pairs') && showFacetWgts()
})

displayPlotAddAggBy <- reactive({
  extraBlocksReady() && 
    notNulls(input$showDSTypeAndPlotAggWgts, input$semiAutoAggOn) &&
    input$showDSTypeAndPlotAggWgts & semiAutoAggOn()
})

displayThemeWgts <- reactive({
  extraBlocksReady() && !is.null(input$showThemeWgts) && input$showThemeWgts
})

displayTitlesCond <- reactive({
  extraBlocksReady() && displayThemeWgts() && !is.null(input$reactive) && !input$reactive
})

displayXlim <- reactive({
  showXYRangeWgts() && displayXCond()
})

displayYlim <- reactive({
  showXYRangeWgts() && displayYCond()
})

displayAgg <- reactive({
  !is.null(input$showDSTypeAndPlotAggWgts) && input$showDSTypeAndPlotAggWgts
})
