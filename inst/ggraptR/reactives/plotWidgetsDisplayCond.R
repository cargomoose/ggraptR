# this function is used for early initialization of input$facet.. and other extra wgts. 
# It prevents the plot redrawing when user clicks on 'Apply facet' the first time or 
# similar extra block. The only block that redraws plot is xy limits because the 1st init 
# run does not supply x() value and I can initialize input$xLim with NULL only.
isInit <- reactive({
  is.null(dataset())
})

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
  aesReady() && 'treatColorAsFactor' %in% isolate(plotInputs()) && !is.null(color()) &&
    isolate(color() %in% numericVars())
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
  aesReady() && willDrawPoints() && is.null(size())
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

displayFacetCond <- reactive({
  isInit <- isInit()  # dataset() trigger. Can be unavailable if omit separate assignment
  # because of boolen lazy evaluation
  (showFacet() && !isolate(is.null(plotType()) && plotType() != 'pairs')) || isInit
})

displayXlimCond <- reactive({
  showXYRange() && displayXCond()  # )|| isolate(isInit())
})

displayYlimCond <- reactive({
  showXYRange() && displayYCond()  # )|| isolate(isInit())
})

displayThemeCond <- reactive({
  showTheme() || isolate(isInit())
})

displayTitlesCond <- reactive({
  displayThemeCond() && !is.null(input$reactive) && !input$reactive
})

displayAggCond <- reactive({
  showDSTypeAndPlotAgg() || isolate(isInit())
})

displayPlotAddAggByCond <- reactive({
  !is.null(plotType()) && displayAggCond() && semiAutoAggOn()
})
