# this function is used for early initialization of input$facet.. and other extra wgts. 
# It prevents the plot redrawing when user clicks on 'Apply facet' the first time or 
# similar extra block. The only block that redraws plot is xy limits because the 1st init 
# run does not supply x() value and I can initialize input$xLim with NULL only.
isInit <- reactive({
  is.null(dataset())
})

aesReady <- reactive({  # serves as trigger too
  displayYCond()
  showAes()
})

displayPlotTypesCond <- reactive({
  !is.null(dataset())
})

displayXCond <- reactive({
  'x' %in% plotInputs()
})

displayYCond <- reactive({
  # y() exists only when x(). y options must be different from selected x value 
  # so it does not make sense to render y before x is ready
  displayXCond() && !is.null(x()) && 'y' %in% plotInputs()
})

displayGgpairsColumnsCond <- reactive({
  'pairs' %in% plotTypes()
})

displayGgpairsCond <- reactive({
  displayGgpairsColumnsCond() && showAes()
})

displayColorCond <- reactive({
  aesReady() && 'color' %in% isolate(plotInputs())
})

displayTreatAsFactorCond <- reactive({
  'treatColorAsFactor' %in% plotInputs() && #aesReady() &&
    !is.null(color()) && color() %in% isolate(numericVars())
})

displayFillCond <- reactive({
  aesReady() && 'fill' %in% isolate(plotInputs())
})

displayShapeCond <- reactive({
  aesReady() && 'shape' %in% isolate(plotInputs())
})

displaySizeCond <- reactive({
  aesReady() && 'size' %in% isolate(plotInputs())
})

displayCoordFlipCond <- reactive({
  aesReady() && !'pairs' %in% isolate(plotTypes())
})

displayJitterCond <- reactive({
  aesReady() && 'jitter' %in% isolate(plotInputs())
})

displaySmthCond <- reactive({
  aesReady() && 'smooth' %in% isolate(plotInputs())
})

displaySizeMagCond <- reactive({
  aesReady() && 'sizeMag' %in% isolate(plotInputs()) && is.null(size())
})

displayAlphaCond <- reactive({
  aesReady() && 'alpha' %in% isolate(plotInputs())
})

displayPositionCond <- reactive({
  aesReady() && 'position' %in% isolate(plotInputs()) && !is.null(fill())
})

displayBinsCond <- reactive({
  aesReady() && 'nBins' %in% isolate(plotInputs())
})

displayDensBlackLineCond <- reactive({
  aesReady() && 'densBlackLine' %in% isolate(plotInputs())
})

displayFacetCond <- reactive({
  isInit <- isInit()  # dataset() trigger. Can be unavailable if omit separate assignment
  # because of boolen lazy evaluation
  (showFacet() && isolate(!is.null(plotTypes()) && !'pairs' %in% plotTypes())) || isInit
})

displayXlimCond <- reactive({
  showFiltering() && displayXCond()  # )|| isolate(isInit())
})

displayYlimCond <- reactive({
  showFiltering() && displayYCond()  # )|| isolate(isInit())
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
  !is.null(plotTypes()) && displayAggCond() && semiAutoAggOn()
})
