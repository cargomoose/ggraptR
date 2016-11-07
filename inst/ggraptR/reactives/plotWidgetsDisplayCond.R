aesReady <- reactive({  # serves as trigger too
  !is.null(displayYCond()) && showAesWgts()
})

pairsReady <- reactive({
  !is.null(displayGgpairsWgtsCond())
})

extraBlocksReady <- reactive({
  !is.null(plotType())
})

willDrawPoints <- reactive({
  plotType() == 'scatter' || 
    ('pointsOverlay' %in% plotInputs() && pointsOverlay())
})

displayXCond <- reactive({
  !is.null(plotType()) && any(c('x', 'xAsFactor') %in% isolate(plotInputs()))
})

displayYCond <- reactive({
  # isolate plotType because switching scatter to violin results premature
  # drawing of y-axis based on old x() value - Sepal.Length
  # x() is the only reactive trigger. y options must be different from selected x value
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
  aesReady() && 'treatAsFactor' %in% isolate(plotInputs())
})

displayFillCond <- reactive({
  pairsReady() && aesReady() && 'fill' %in% isolate(plotInputs())
})

displayPosCond <- reactive({
  aesReady() && 'position' %in% isolate(plotInputs())
})

displayJitterCond <- reactive({
  aesReady() && 'jitter' %in% isolate(plotInputs())
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
  aesReady() && 'alpha' %in% isolate(plotInputs())
})

displayBinWidthCond <- reactive({
  aesReady() && 'binWidth' %in% isolate(plotInputs())
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
