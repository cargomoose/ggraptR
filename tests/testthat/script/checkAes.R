switchToDataset(driver, test_settings$dataset, 
                init_plot = if (test_settings$only_pairs) 'pairs' else 'scatter')
usedPlotNames <- if (!test_settings$only_pairs) c() else {
  cat('\nshort test mode on\n')
  setdiff(getAllPlotNames(), 'Pairs')
}

isLastIter <- F
while (!isLastIter) {
  waitForPlotReady(driver)
  plotNames <- getCurrentPlotNames(driver)
  
  test_that(sprintf('[%s] [default_inputs] work correct', pastePlus(plotNames)), 
            expect_true(has_shiny_correct_state(driver, plotNames,
                                                NULL, NULL, waitPlot=F)))
  
  for (inpId in getPlotInputIds(driver)) {
    inpType <- driver %>% getEl(c('#', inpId)) %>% attr('data-shinyjs-resettable-type')
    test_that(sprintf('[%s] [%s] works correct', pastePlus(plotNames), inpId), {
      if (is.null(inpType)) {
        skip(pastePlus(plotNames, inpId, '[is hidden now]', shorten = F))
      } else {
        expect_true(do.call(paste0('is', inpType, 'Correct'), 
                            list(driver, inpId, plotNames)))
      } 
    })
  }
  
  usedPlotNames <- append(usedPlotNames, setdiff(plotNames, usedPlotNames))
  isNextPlotAdded <- tryAddNextPlot(driver)
  
  if (!isNextPlotAdded) {
    nextPlotTypes <- setdiff(getAllPlotNames(), usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plotNames))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}
