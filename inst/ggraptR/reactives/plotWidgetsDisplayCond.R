#### main ####
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

displayColorCond <- reactive({
  'color' %in% plotInputs()
})

displayTreatAsFactorCond <- reactive({
  'treatColorAsFactor' %in% plotInputs() && 
    !is.null(color()) && color() %in% isolate(numericVars())
})

displayFillCond <- reactive({
  'fill' %in% plotInputs()
})

displaySizeCond <- reactive({
  'size' %in% plotInputs()
})

displayShapeCond <- reactive({
  'shape' %in% plotInputs()
})

displayPositionCond <- reactive({
  'position' %in% plotInputs() && !is.null(fill())
})

displayJitterCond <- reactive({
  'jitter' %in% plotInputs()
})

displayCoordFlipCond <- reactive({
  displayXCond() && !'pairs' %in% isolate(plotTypes())
})

displaySmthCond <- reactive({
  'smooth' %in% plotInputs()
})

displayAlphaCond <- reactive({
  'alpha' %in% plotInputs()
})

displaySizeMagCond <- reactive({
  'sizeMag' %in% plotInputs() && is.null(size())
})

displayBinsCond <- reactive({
  'nBins' %in% plotInputs()
})

displayDensBlackLineCond <- reactive({
  'densBlackLine' %in% plotInputs()
})


#### pairs ####
displayGgpairsCond <- reactive({
  displayGgpairsColumnsCond()
})


#### facet ####
displayFacetCond <- reactive({
  is.null(plotTypes()) || !'pairs' %in% plotTypes()
})


#### theme ####
# displayTitlesCond <- reactive({
#   !is.null(input$reactive) && !input$reactive 
# })
