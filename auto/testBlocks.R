tryAddNextPlot <- function(driver) {
  plotTypeGroupRestOpts <- getOptions(driver, 'plotTypes')
  if (length(plotTypeGroupRestOpts)) {  # add another one plot to the current group
    plotTypeGroupRestOpts[[1]] %>% click()
    TRUE
  } else {
    FALSE
  }
}

startNewPlotGroup <- function(driver, nextPlotType) {
  getOptions(driver, 'plotTypes') %>% 
    filterElByAttr('outerText', nextPlotType) %>% 
    click()
}

expect_plot_group_inputs_correct <- function(driver) {
  plotNames <- getCurrentPlotNames(driver)
  
  inputIds <- getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')

  for (inpId in if (get.anywhere('ggraptrDevMode')) head(inputIds, 2) else inputIds) {
    input <- driver %>% getEl(c('#', inpId))
    inpType <- attr(input, 'data-shinyjs-resettable-type')
    
    if (is.null(inpType)) {
      cat(pastePlus(plotNames), inpId, '[is hidden now, skipped]\n')
      next
    } else {
      # cat(pastePlus(plotNames), inpId, '\n')
    }
    
    do.call(paste0('test', inpType), list(driver, inpId, plotNames))
  }
}

testSelect <- function(driver, inpId, plotNames) {
  optVals <- getOptions(driver, inpId) %>% attr('data-value')
  
  for (optVal in optVals) {
    getOptions(driver, inpId) %>% 
      filterElByAttr('data-value', optVal) %>% click()
    
    expect_true(has_shiny_correct_state(driver, plotNames, inpId))
    if (!is.null(driver %>% getEl(c('#', inpId)) %>% attr('multiple'))) {
      eraseMultiSelectOpts(driver, inpId)
      waitForPlotReady(driver)
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
    
    expect_true(has_shiny_correct_state(driver, plotNames, inpId))
  }
}

testCheckbox <- function(driver, inpId, plotNames) {
  isShow <- grepl('^show', inpId)
  for (i in 1:(1+isShow)) {
    query <- paste0('//*[@class="widblock" and .//*[@id="', inpId, '"]]',
                    '//*[contains(@class, "shiny-bound-input shinyjs-resettable")]')
    nWidBlockInps <- driver %>% getEls(query) %>% length
    driver %>% getEl(c('#', inpId)) %>% click()
    if (isShow) waitFor(quote(nWidBlockInps != length(driver %>% getEls(query))), driver)
    if (i == 1) {
      expect_true(has_shiny_correct_state(driver, plotNames, inpId, waitPlot = !isShow))
    }
  }
}
