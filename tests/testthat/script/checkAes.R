cat("\nMain aes blocks")

usedPlotNames <- c()
isLastIter <- F

while (!isLastIter) {
  waitForPlotReady(driver)
  plot_names <- get_current_plot_names(driver)
  
  test_that(sprintf('[%s] [default_inputs] work correct', pastePlus(plot_names)), 
            expect_true(has_shiny_correct_state(driver, plot_names,
                                                NULL, NULL, waitPlot=F)))
  
  for (inp_id in get_plot_input_ids(driver)) check_input(driver, inp_id, plot_names)
  
  usedPlotNames <- append(usedPlotNames, setdiff(plot_names, usedPlotNames))
  isNextPlotAdded <- tryAddNextPlot(driver)
  
  if (!isNextPlotAdded) {
    nextPlotTypes <- setdiff(getAllPlotNames(), usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plot_names))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}
