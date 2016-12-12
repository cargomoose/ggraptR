# Make sure 'ggraptrDevMode' is FALSE to run tests in the full mode
# Run this script as 'testthat::test_file(paste0(getwd(), '/auto/test.R'))'
# You can monitor the progress by the names of the screenshots in auto/report
# issues and new features https://github.com/cargomoose/ggraptR/issues/61

stopifnot(grepl('/auto$',  getwd()))  # for 'R -e "shiny::runApp'
source(paste0(getwd(), '/testInit.R'))

shortTestMode <- F
port <- 5050
print(paste0('port: ', port, ', fullTestingMode: ', !shortTestMode))

system(sprintf(
  'R -e "shiny::runApp(\'../inst/ggraptR\', port=%s, launch.browser=TRUE)"', 
  port), wait=F)
selServer <- startSelServer()
driver <- getDriver(port)

test_that("Can connect to app", {
  expect_equal(driver$getTitle()[[1]], 'ggraptR')
})
test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  expect_true(has_shiny_correct_state(driver, 'diamonds', NULL, NULL, waitPlot=F))
})

datasetEls <- driver %>% getSelectOptions('dataset')
datasetEls %>% filterElByAttr('data-value', 'esoph') %>% click()
allPlotNames <- getAllPlotNames()
if (!is.logical(waitFor('#plotTypesCtrl .item[data-value="histogram"]', driver,
                        timeout=5, errorIfNot = F))) {
  stopifnot(waitFor(quote(try(
    length(allPlotNames) == length(driver %>% getSelectOptions('plotTypes')),
    silent=T)), driver))
}
getSelectOptions(driver, 'plotTypes')[[1]] %>% click()

usedPlotNames <- if (shortTestMode) setdiff(allPlotNames, 'Pairs') else c()
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
