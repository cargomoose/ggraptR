aesReady <- reactive({  # serves as trigger too
  !is.null(displayYCond()) && showAes() && isolate(x() %in% names(dataset()))
})

pairsReady <- reactive({
  !is.null(displayGgpairsCond())
})

willDrawPoints <- reactive({
  !is.null(plotType()) && (plotType() == 'scatter' || 
    ('pointsOverlay' %in% plotInputs() && pointsOverlay()))
})

displayXCond <- reactive({
  !is.null(plotType()) && 'x' %in% isolate(plotInputs())
})

displayYCond <- reactive({
  # y options must be different from selected x value 
  # so it does not make sense to render y before x is ready
  !is.null(displayXCond()) && !is.null(x()) &&
    !is.null(isolate(plotType())) && 'y' %in% plotInputs()
})

displayGgpairsCond <- reactive({
  !is.null(plotType()) && plotType() == 'pairs'
})

displayColorCond <- reactive({
  pairsReady() && aesReady() && 'color' %in% isolate(plotInputs())
})

displayTreatAsFactorCond <- reactive({
  aesReady() && 'treatColorAsFactor' %in% isolate(plotInputs()) && !is.null(color())
})

displayFillCond <- reactive({
  pairsReady() && aesReady() && 'fill' %in% isolate(plotInputs())
})

displayShapeCond <- reactive({
  aesReady() && willDrawPoints()
})

displaySizeCond <- reactive({
  aesReady() && willDrawPoints()
})

displayCoordFlipCond <- reactive({
  aesReady() && isolate(plotType()) != 'pairs'
})

displayJitterCond <- reactive({
  aesReady() && ('jitter' %in% plotInputs() || willDrawPoints())
})

displaySmthCond <- reactive({
  aesReady() && 'smooth' %in% isolate(plotInputs())
})

displaySizeMagCond <- reactive({
  aesReady() && willDrawPoints()
})

displayAlphaCond <- reactive({
  aesReady() && ('alpha' %in% plotInputs() || willDrawPoints())
})

displayPointsOverlayCond <- reactive({
  aesReady() && isolate('pointsOverlay' %in% plotInputs())
})

displayPosCond <- reactive({
  aesReady() && 'position' %in% isolate(plotInputs())
})

displayBinWidthCond <- reactive({
  aesReady() && 'nBins' %in% isolate(plotInputs())
})

# density black line
displayDensBlackLineCond <- reactive({
  aesReady() && 'densBlackLine' %in% isolate(plotInputs())
})


displayXlimCond <- reactive({
  showXYRange() && displayXCond()
})

displayYlimCond <- reactive({
  showXYRange() && displayYCond()
})

displayFacetCond <- reactive({
  showFacet() && isolate(!is.null(plotType()) && plotType() != 'pairs')
})

displayThemeCond <- reactive({
  showTheme()
})

displayTitlesCond <- reactive({
  displayThemeCond() && !is.null(input$reactive) && !input$reactive
})

displayAggCond <- reactive({
  showDSTypeAndPlotAgg()
})

displayPlotAddAggByCond <- reactive({
  displayAggCond() & semiAutoAggOn()
})
