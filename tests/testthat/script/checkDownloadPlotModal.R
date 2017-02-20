cat("\nExport plot and generate code buttons")

driver %>% getEl('#exportPlot') %>% click()
root <- waitFor('#modalExportOptions[style="display: block;"]', driver)

test_that('#exportPlot button works fine', {
  expect_true(!is.null(waitFor('a#dlPlot', root)))
})

root %>% getEl('button.close') %>% click()
waitFor('#modalExportOptions[style="display: none;"]', driver)

