cat("\nExport plot and generate code buttons")

driver %>% getEl('#exportPlot') %>% click()
root <- wait_for('#modalExportOptions[style="display: block;"]', driver)

test_that('#exportPlot button works fine', {
  expect_true(!is.null(wait_for('a#dlPlot', root)))
})

root %>% getEl('button.close') %>% click()
wait_for('#modalExportOptions[style="display: none;"]', driver)

