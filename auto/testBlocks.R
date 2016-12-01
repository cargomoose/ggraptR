testPlotGroupInputs <- function(driver) {
  plotNames <- getCurrentPlotNames(driver)
  
  inputIds <- getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')
  
  for (inpId in inputIds) {
    print(inpId)
    input <- driver %>% getEl(c('#', inpId))
    inpType <- attr(input, 'data-shinyjs-resettable-type')
    if (inpType == "Select") {
      if (inpId == 'color') browser()
      optVals <- getOptions(driver, inpId) %>% attr('data-value')
      for (optVal in optVals) {
        Filter(function(x) x %>% attr('data-value') == optVal,
               getOptions(driver, inpId))[[1]] %>% click()
        expect_true(test_shiny_correct(driver, plotNames, inpId))
        if (!is.null(attr(input, 'multiple'))) {
          eraseMultiSelectOpts(driver, inpId)
          waitPlotReady()
        }
      }
    } else if (inpType == "Checkbox") {
      isShow <- grepl('^show', inpId)
      
      for (i in 1:(1+isShow)) {
        input %>% click()
        if (i == 1) {
          expect_true(test_shiny_correct(driver, plotNames, inpId, waitPlot = !isShow))
        }
      }
    } else if (inpType == "Slider") {
      ctrlEl <- getEl(driver, c("#", id, "Ctrl"))
      leftPos <- getEl(ctrlEl, ".irs-line-mid")$getElementLocation()$x
      rightPos <- getEl(ctrlEl, ".irs-line-right")$getElementLocation()$x
      
      for (pos in c(leftPos, rightPos)) {
        dotEl <- ctrlEl %>% getEl(".irs-slider")
        driver$mouseMoveToLocation(webElement = dotEl)
        driver$buttondown()
        driver$mouseMoveToLocation(x = pos - dotEl$getElementLocation()$x, y = -1L)
        driver$buttonup()
        
        expect_true(test_shiny_correct(driver, plotNames, inpId))
      }
    } else {
      stop('Sudden input type: ', inpType)
    }
  }
}

callNextPlot <- function(driver, usedPlotTypeNames, curUsedPlotTypeNames) {
  plotTypeGroupRestOpts <- getOptions(driver, 'plotTypes')
  if (length(plotTypeGroupRestOpts)) {  # if we can another one plot to the current group
    plotTypeGroupRestOpts[[1]] %>% click()
  } else {
    eval.parent(substitute(
      usedPlotTypeNames <- append(usedPlotTypeNames, curUsedPlotTypeNames)))
    eraseMultiSelectOpts(driver, 'plotTypes', length(curUsedPlotTypeNames))
    allOptions <- getOptions(driver, 'plotTypes')
    nextPlotType <- setdiff(allOptions %>% text(), usedPlotTypeNames)[1]
    if (length(nextPlotType)) {
      Filter(function(el) text(el) == nextPlotType, allOptions)[[1]] %>% click()
      waitPlotReady()
    } else {
      return(F)
    }
  }
  TRUE
}
