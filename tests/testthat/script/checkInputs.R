usedPlotNames <- if (exists('shortTestMode') && shortTestMode)
  setdiff(getAllPlotNames(), 'Pairs') else c()

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
      skip(pastePlus(plotNames), inpId, '[is hidden now]')
    } else {
      test_that(sprintf('[%s] [%s] works correct', pastePlus(plotNames), inpId),
                expect_true(do.call(paste0('is', inpType, 'Correct'), 
                                    list(driver, inpId, plotNames))))
    }
  }
  
  isNextPlotAdded <- tryAddNextPlot(driver)
  if (!isNextPlotAdded) {
    usedPlotNames <- append(usedPlotNames, plotNames)
    
    nextPlotTypes <- setdiff(getAllPlotNames(), usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plotNames))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}
