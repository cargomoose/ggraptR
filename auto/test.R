# Make sure 'ggraptrDevMode' is FALSE to run tests in the full mode
# Run this script as 'testthat::test_file(paste0(getwd(), '/auto/test.R'))'
# You can monitor the progress by the names of the screenshots in auto/report
# Not sure about non-gray themes. May not work

source(paste0(getwd(), if (!grepl('/auto',  getwd())) '/auto', '/initConfig.R'))
source(paste0(getProjWd(), '/auto/seleniumUtils.R'))
source(paste0(getProjWd(), '/auto/shinyTestUtils.R'))
source(paste0(getProjWd(), '/auto/testBlocks.R'))

ggraptrDevMode <- F
selServer <- startSelServer()
driver <- getDriver()

test_that("Can connect to app", {
  expect_equal(driver$getTitle()[[1]], 'ggraptR')
})

source(paste0(getProjWd(), '/inst/ggraptR/globals.R'))
source(paste0(getProjWd(), '/inst/ggraptR/functions/helper.R'))
allPlotNames <- flattenList(definedPlotInputs) %>% names %>% 
  sapply(capitalize) %>% 
  sapply(function(x) gsub('(\\d)', ' \\1', x))
usedPlotTypeNames <- if (ggraptrDevMode) setdiff(allPlotNames, 'Pairs') else c()

isLastIter <- F
while (!isLastIter) {
  waitForPlotReady(driver)
  curUsedPlotTypeNames <- getCurrentPlotNames(driver)
  
  test_that(sprintf('%s default_inputs', pastePlus(curUsedPlotTypeNames)), 
            expect_true(has_shiny_correct_state(driver, curUsedPlotTypeNames,
                                                NULL, waitPlot=F)))
  
  test_that(sprintf('%s customized_inputs', pastePlus(curUsedPlotTypeNames)),
    expect_plot_group_inputs_correct(driver))
  
  isNextPlotAdded <- tryAddNextPlot(driver)
  if (!isNextPlotAdded) {
    usedPlotTypeNames <- append(usedPlotTypeNames, curUsedPlotTypeNames)
    nextPlotType <- setdiff(allPlotNames, usedPlotTypeNames)[1]
    if (!is.na(nextPlotType)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(curUsedPlotTypeNames))
      startNewPlotGroup(driver, nextPlotType)
    } else {
      isLastIter <- T
    }
  }
}

stopExternals(driver, selServer)
