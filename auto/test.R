# not sure about non-gray themes. May not work
source(paste0(getwd(), '/auto/initConfig.R'))
source(paste0(getwd(), '/auto/seleniumUtils.R'))
source(paste0(getwd(), '/auto/shinyTestUtils.R'))

selServer <- startServer()
driver <- getDriver()

test_that("Can connect to app", {  
  expect_equal(driver$getTitle()[[1]], 'ggraptR')  
})

# x <- driver %>% getEl('#alphaCtrl > div > span > span.irs-slider.single')
# x$sendKeysToElement(list(key = selKeys$left_arrow))
# getEls('#plotTypesCtrl .option')

usedPlotTypeNames <- c()
repeat {
  curUsedPlotTypeNames <- getEls(driver, '#plotTypes option') %>% text()
  test_that(paste(curUsedPlotTypeNames, collapse=' + '), {
    expect_true(test_shiny_correct(driver))
    
    inputs <- getEls(driver, c(
      'form', 
      '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
      ' .shiny-bound-input.shinyjs-resettable'))
    
    for (input in inputs) {
      inpId <- attr(input, 'id')
      inpType <- attr(input, 'data-shinyjs-resettable-type')
      if (inpType == "Select") {
        for (opt in fillOptions(driver, inpId)) {
          opt %>% click()
          expect_true(test_shiny_correct(driver))
        }
      } else if (inpType == "Checkbox") {
        isShow <- grepl('^show', inpId)
        for (i in 1:(1+isShow)) {
          input %>% click()
          if (i == 1) expect_true(test_shiny_correct(driver, waitPlot = !isShow))
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
          
          expect_true(test_shiny_correct(driver))
        }
      } else {
        stop('Sudden input type: ', inpType)
      }
    }
  
    plotTypeGroupRestOpts <- fillOptions(driver, 'plotTypes')
    if (length(plotTypeGroupRestOpts)) {
      plotTypeGroupRestOpts[[1]] %>% click()
      # expect_true(test_shiny_correct(driver))
    } else {
      usedPlotTypeNames <- append(usedPlotTypeNames, curUsedPlotTypeNames)
      # erases all plot types
      getEl(driver, '#plotTypes + .selectize-control input')$
        sendKeysToElement(as.list(rep(selKeys$backspace, length(curUsedPlotTypeNames))))
      allOptions <- fillOptions(driver, 'plotTypes')
      nextPlotType <- setdiff(allOptions %>% text(), usedPlotTypeNames)[1]
      if (length(nextPlotType)) {
        Filter(function(el) text(el) == nextPlotType, allOptions)[[1]] %>% click()
        waitPlotReady()
      } else {
        break
      }
    }
  })
}

stopExternals(driver, selServer)
