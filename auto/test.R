# not sure about non-gray themes. May not work
source(paste0(getwd(), '/auto/initConfig.R'))
source(paste0(getwd(), '/auto/seleniumUtils.R'))
source(paste0(getwd(), '/auto/shinyTestUtils.R'))
source(paste0(getwd(), '/auto/testBlocks.R'))

selServer <- startSelServer()  # paste0(getwd(), '/auto/resources/')
driver <- getDriver()

test_that("Can connect to app", {  
  expect_equal(driver$getTitle()[[1]], 'ggraptR')  
})

usedPlotTypeNames <- c()
repeat {
  waitForPlotReady()
  curUsedPlotTypeNames <- getCurrentPlotNames(driver)
  test_that(paste(curUsedPlotTypeNames, collapse=' + '), {
    expect_true(test_shiny_correct(driver, curUsedPlotTypeNames, NULL, waitPlot=F))
    testPlotGroupInputs(driver)
    if (!callNextPlot(driver, usedPlotTypeNames, curUsedPlotTypeNames)) break
  })
}

# stopExternals(driver, selServer)
