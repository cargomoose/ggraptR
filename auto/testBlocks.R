tryAddNextPlot <- function(driver) {
  plotTypeGroupRestOpts <- getSelectOptions(driver, 'plotTypes')
  canAdd <- length(plotTypeGroupRestOpts) > 0
  if (canAdd) sample(plotTypeGroupRestOpts, size=1)[[1]] %>% click()
  canAdd
}

startNewPlotGroup <- function(driver, nextPlotType) {
  getSelectOptions(driver, 'plotTypes') %>% 
    filterElByAttr('outerText', nextPlotType) %>% 
    click()
}

getPlotInputIds <- function(driver) {
  inputIds <- getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')
  
  if (get.anywhere('shortTestMode')) 
    head(inputIds, 2) else inputIds[sample(length(inputIds))]
}

isSelectCorrect <- function(driver, inpId, plotNames) {
  optVals <- getSelectOptions(driver, inpId) %>% attr('data-value')
  
  for (optVal in optVals) {
    getSelectOptions(driver, inpId) %>% 
      filterElByAttr('data-value', optVal) %>% 
      click()
    
    if (!has_shiny_correct_state(driver, plotNames, inpId)) return(FALSE)
    if (!is.null(driver %>% getEl(c('#', inpId)) %>% attr('multiple'))) {
      eraseMultiSelectOpts(driver, inpId)
      waitForPlotReady(driver)
    }
  }
  TRUE
}

isSliderCorrect <- function(driver, inpId, plotNames) {
  ctrlEl <- getEl(driver, c("#", inpId, "Ctrl"))
  leftPos <- getEl(ctrlEl, ".irs-line-mid")$getElementLocation()$x
  rightPos <- getEl(ctrlEl, ".irs-line-right")$getElementLocation()$x
  
  for (pos in c(rightPos, leftPos)) {
    dotEl <- ctrlEl %>% getEl(".irs-slider")
    driver$mouseMoveToLocation(webElement = dotEl)
    driver$buttondown()
    driver$mouseMoveToLocation(x = pos - dotEl$getElementLocation()$x, y = -1L)
    driver$buttonup()
    
    if (!has_shiny_correct_state(driver, plotNames, inpId)) return(FALSE)
  }
  TRUE
}

isCheckboxCorrect <- function(driver, inpId, plotNames) {
  isShow <- grepl('^show', inpId)
  for (i in 1:(1+isShow)) {
    query <- paste0('//*[@class="widblock" and .//*[@id="', inpId, '"]]',
                    '//*[contains(@class, "shiny-bound-input shinyjs-resettable")]')
    nWidBlockInps <- driver %>% getEls(query) %>% length
    chkBoxEl <- driver %>% getEl(c('#', inpId))
    if (!isVisible(chkBoxEl) && isShow) return(T)
    
    chkBoxEl %>% click()
    if (isShow) waitFor(quote(nWidBlockInps != length(driver %>% getEls(query))), driver)
    if (i == 1) {
      res <- has_shiny_correct_state(driver, plotNames, inpId, waitPlot = !isShow)
      if (!res) return(FALSE)
    }
  }
  TRUE
}
