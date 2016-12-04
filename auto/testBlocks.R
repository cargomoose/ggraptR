callNextPlot <- function(driver, usedPlotTypeNames, curUsedPlotTypeNames) {
  plotTypeGroupRestOpts <- getOptions(driver, 'plotTypes')
  if (length(plotTypeGroupRestOpts)) {  # add another one plot to the current group
    plotTypeGroupRestOpts[[1]] %>% click()
  } else {  # erase current plots
    eval(substitute(  # changes a list by ref
      usedPlotTypeNames <- append(usedPlotTypeNames, curUsedPlotTypeNames)), 
      envir=.GlobalEnv)
    eraseMultiSelectOpts(driver, 'plotTypes', length(curUsedPlotTypeNames))
    
    allOptions <- getOptions(driver, 'plotTypes')
    nextPlotType <- setdiff(allOptions %>% text(), usedPlotTypeNames)[1]
    
    if (length(nextPlotType)) {  # must we stop?
      allOptions %>% filterElByAttr('outerText', nextPlotType) %>% click()
      waitForPlotReady()
    } else {
      return(F)
    }
  }
  TRUE
}

testPlotGroupInputs <- function(driver) {
  plotNames <- getCurrentPlotNames(driver)
  
  inputIds <- getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')
  inputIds <- inputIds[c(1:2, (1:2)+(length(inputIds) - 2))] ####
  
  for (inpId in inputIds) {  
    input <- driver %>% getEl(c('#', inpId))
    inpType <- attr(input, 'data-shinyjs-resettable-type')
    
    if (is.null(inpType)) {
      cat(pastePlus(plotNames), inpId, '[is hidden now, skipped]\n')
      next
    } else {
      cat(pastePlus(plotNames), inpId, '\n')
    }
    
    do.call(paste0('test', inpType), list(driver, inpId, plotNames))
  }
}

testSelect <- function(driver, inpId, plotNames) {
  optVals <- getOptions(driver, inpId) %>% attr('data-value')
  
  for (optVal in optVals) {
    getOptions(driver, inpId) %>% 
      filterElByAttr('data-value', optVal) %>% click()
    
    expect_true(test_shiny_correct(driver, plotNames, inpId))
    if (!is.null(driver %>% getEl(c('#', inpId)) %>% attr('multiple'))) {
      eraseMultiSelectOpts(driver, inpId)
      waitForPlotReady()
    }
  }
}

testSlider <- function(driver, inpId, plotNames) {
  ctrlEl <- getEl(driver, c("#", inpId, "Ctrl"))
  leftPos <- getEl(ctrlEl, ".irs-line-mid")$getElementLocation()$x
  rightPos <- getEl(ctrlEl, ".irs-line-right")$getElementLocation()$x
  
  for (pos in c(rightPos, leftPos)) {
    dotEl <- ctrlEl %>% getEl(".irs-slider")
    driver$mouseMoveToLocation(webElement = dotEl)
    driver$buttondown()
    driver$mouseMoveToLocation(x = pos - dotEl$getElementLocation()$x, y = -1L)
    driver$buttonup()
    
    expect_true(test_shiny_correct(driver, plotNames, inpId))
  }
}

testCheckbox <- function(driver, inpId, plotNames) {
  isShow <- grepl('^show', inpId)
  for (i in 1:(1+isShow)) {
    query <- paste0('//*[@class="widblock" and .//*[@id="', inpId, '"]]',
                    '//*[contains(@class, "shiny-bound-input shinyjs-resettable")]')
    nWidBlockInps <- driver %>% getEls(query) %>% length
    driver %>% getEl(c('#', inpId)) %>% click()
    if (isShow) waitFor(quote(nWidBlockInps != length(driver %>% getEls(query))))
    if (i == 1) {
      expect_true(test_shiny_correct(driver, plotNames, inpId, waitPlot = !isShow))
    }
  }
}
