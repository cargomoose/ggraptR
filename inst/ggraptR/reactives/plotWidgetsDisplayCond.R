aesReady <- reactive({  # serves as trigger too
  !is.null(displayYCond()) && showAesWgts() && isolate(x() %in% names(dataset()))
})

pairsReady <- reactive({
  !is.null(displayGgpairsWgtsCond())
})

extraBlocksReady <- reactive({
  !is.null(plotType())
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

displayGgpairsWgtsCond <- reactive({
  !is.null(plotType()) && plotType() == 'pairs'
})

displayColCond <- reactive({
  pairsReady() && aesReady() && 'color' %in% isolate(plotInputs())
})

displayTreatAsFactorCond <- reactive({
  aesReady() && 'treatColorAsFactor' %in% isolate(plotInputs()) && !is.null(color())
})

displayFillCond <- reactive({
  pairsReady() && aesReady() && 'fill' %in% isolate(plotInputs())
})

displaySmthCond <- reactive({
  aesReady() && 'smooth' %in% isolate(plotInputs())
})

displayShapeCond <- reactive({
  aesReady() && willDrawPoints()
})

displaySizeCond <- reactive({
  aesReady() && willDrawPoints()
})

displaySizeMagCond <- reactive({
  aesReady() && willDrawPoints()
})

displayPointsOverlayCond <- reactive({
  aesReady() && isolate('pointsOverlay' %in% plotInputs())
})

displayCoordFlipCond <- reactive({
  aesReady() && isolate(plotType()) != 'pairs'
})

displayAlphaCond <- reactive({
  aesReady() && ('alpha' %in% plotInputs() || willDrawPoints())
})

displayJitterCond <- reactive({
  aesReady() && ('jitter' %in% plotInputs() || willDrawPoints())
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
