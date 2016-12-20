# Run this script as 'testthat::test_file(paste0(getwd(), '/test/auto/main.R'))'
# You can monitor the progress by the names of the screenshots in test/auto/report
# issues and new features https://github.com/cargomoose/ggraptR/issues/61

source('funs.R')

# starts ggraptR app, selenium server and phantomjs driver
system(sprintf('R -e "shiny::runApp(\'%s\', port=%s, launch.browser=FALSE)"',
  paste0(getProjWd(), '/inst/ggraptR'), 5050), wait=F)  # R -e "ggraptR::ggraptR(port=%s)
selServer <- startSelServer()
driver <- getDriver(port)
stopifnot(driver$getTitle()[[1]] == 'ggraptR')

test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  expect_true(has_shiny_correct_state(driver, 'diamonds', NULL, NULL, waitPlot=F))
})

# switches to light esoph dataset
datasetEls <- driver %>% getSelectOptions('dataset')
datasetEls %>% filterElByAttr('data-value', 'esoph') %>% click()
allPlotNames <- getAllPlotNames()
if (!is.logical(waitFor('#plotTypesCtrl .item[data-value="histogram"]', driver,
                        timeout=5, errorIfNot = F))) {
  if (!waitFor(quote(try(
      length(allPlotNames) == length(driver %>% getSelectOptions('plotTypes')),
      silent=T)), driver)) {
    driver$screenshot(T)
    stop()
  }
}
getSelectOptions(driver, 'plotTypes')[[1]] %>% click()


usedPlotNames <- if (exists('shortTestMode' && shortTestMode))
  setdiff(allPlotNames, 'Pairs') else c()
isLastIter <- F
while (!isLastIter) {
  waitForPlotReady(driver)
  plotNames <- getCurrentPlotNames(driver)
  
  test_that(sprintf('[%s] [default_inputs] work correct', pastePlus(plotNames)), 
            expect_true(has_shiny_correct_state(driver, plotNames,
                                                NULL, NULL, waitPlot=F)))
  
  for (inpId in getPlotInputIds(driver)) {
    inpType <- driver %>% getEl(c('#', inpId)) %>% attr('data-shinyjs-resettable-type')
    if (is.null(inpType)) {
      cat(pastePlus(plotNames), inpId, '[is hidden now, skipped]')
      next
    }
    
    test_that(sprintf('[%s] [%s] works correct', pastePlus(plotNames), inpId),
              expect_true(do.call(paste0('is', inpType, 'Correct'), 
                                  list(driver, inpId, plotNames))))
  }
  
  isNextPlotAdded <- tryAddNextPlot(driver)
  if (!isNextPlotAdded) {
    usedPlotNames <- append(usedPlotNames, plotNames)
    
    nextPlotTypes <- setdiff(allPlotNames, usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plotNames))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}

stopExternals(driver, selServer)
