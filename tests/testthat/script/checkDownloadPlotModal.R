cat("\nExport plot and generate code buttons")

driver %>% getEl('#downloadPlot') %>% click()
root <- wait_for('#modalDownloadPlotOptions[style="display: block;"]', driver)

test_that('#downloadPlot button works fine', {
  expect_true(!is.null(wait_for('a#dlPlot', root)))
})

root %>% getEl('button.close') %>% click()
wait_for('#modalDownloadPlotOptions[style="display: none;"]', driver)
