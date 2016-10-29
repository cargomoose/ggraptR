# serves as trigger too
aesReady <- reactive({
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
  # plotType() -> xOpts() -> xCtrl() -> x() -> yOpts() -> .
  
  # !is.null(yOpts()) &&  ####
  # if (is.null(plotType()) || (displayXCond() && is.null(x))) return()  ####
  
  # x() is the only reactive trigger. y options must be different from selected x value
  # so it does not make sense to render y before x is ready
  !is.null(displayXCond()) && !is.null(x()) &&
    isolate(!is.null(plotType()) && !(plotType() %in% c('pairs', 'histogram', 'density')))
})

displayGgpairsWgtsCond <- reactive({
  !is.null(plotType()) && plotType() == 'pairs'
})

displayColCond <- reactive({
  # sel: !is.null(colorOrig()) && !is.null(colOpts()) && colorOrig() %in% colOpts()
  pairsReady() && aesReady() && 
    isolate(plotType() %in% c('line', 'scatter', 'path', 'pairs'))
})

# for color
displayTreatAsFacVarColCond <- reactive({
  aesReady() && isolate(plotType() == 'scatter')
})

displayFillCond <- reactive({
  # sel: !is.null(fillOrig()) && !is.null(fillOpts()) && fillOrig() %in% fillOpts()
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
  # sel: !is.null(shapeOrig()) && !is.null(shapeOpts()) && shapeOrig() %in% shapeOpts()
  aesReady() && isolate(
    plotType() == 'scatter' || (plotType() %in% c('line', 'path') && pointsOverlay()))
})

displaySizeCond <- reactive({
  # sel: !is.null(sizeOrig()) && !is.null(sizeOpts()) && sizeOrig() %in% sizeOpts()
  displayShapeCond()
})

displaySmthCond <- reactive({
  displayJitCond()
})

displayCoordFlipCond <- reactive({
  aesReady() && isolate(plotType() != 'pairs')
})

# size magnifier. Belongs to advanced control widgets
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

# points overlay checkbox
displayPointsOverlayCond <- reactive({
  aesReady() && isolate(plotType() %in% c('line', 'path'))
})

 
displayFacetCond <- reactive({
  #sel:!is.null(facetRowOrig()) && !is.null(facetOpts()) && facetRowOrig()%in%facetOpts()
  # !is.null(facetColOrig()) && !is.null(facetOpts()) && facetColOrig() %in% facetOpts()
  # !is.null(facetWrapOrig()) && !is.null(facetOpts()) && facetWrapOrig() %in% facetOpts()
  extraBlocksReady() && isolate(plotType() != 'pairs') && showFacetWgts()
})

# display additional aggregation select field
displayPlotAddAggBy <- reactive({
  # sel:!is.null(plotAddAggBy()) && !is.null(plotAddAggByOpts()) &&
  # && all(plotAddAggBy() %in% plotAddAggByOpts())
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
