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
  displayGgpairsColumnsCond()
})


#### aes ####
displayColorCond <- reactive({
  displayYCond() && 'color' %in% isolate(plotInputs())
})

displayTreatAsFactorCond <- reactive({
  'treatColorAsFactor' %in% plotInputs() && 
    !is.null(color()) && color() %in% isolate(numericVars())
})

displayFillCond <- reactive({
  displayYCond() && 'fill' %in% isolate(plotInputs())
})

displayShapeCond <- reactive({
  displayYCond() && 'shape' %in% isolate(plotInputs())
})

displaySizeCond <- reactive({
  displayYCond() && 'size' %in% isolate(plotInputs())
})

displayCoordFlipCond <- reactive({
  displayYCond() && !'pairs' %in% isolate(plotTypes())
})

displayJitterCond <- reactive({
  displayYCond() && 'jitter' %in% isolate(plotInputs())
})

displaySmthCond <- reactive({
  displayYCond() && 'smooth' %in% isolate(plotInputs())
})

displaySizeMagCond <- reactive({
  displayYCond() && 'sizeMag' %in% isolate(plotInputs()) && is.null(size())
})

displayAlphaCond <- reactive({
  displayYCond() && 'alpha' %in% isolate(plotInputs())
})

displayPositionCond <- reactive({
  displayYCond() && 'position' %in% isolate(plotInputs()) && !is.null(fill())
})

displayBinsCond <- reactive({
  displayYCond() && 'nBins' %in% isolate(plotInputs())
})

displayDensBlackLineCond <- reactive({
  displayYCond() && 'densBlackLine' %in% isolate(plotInputs())
})


#### extra ####
# displayTitlesCond <- reactive({
#   !is.null(input$reactive) && !input$reactive 
# })

displayFacetCond <- reactive({
  is.null(plotTypes()) || !'pairs' %in% plotTypes()
})
