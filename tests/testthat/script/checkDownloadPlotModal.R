cat("\nDownload plot and generate code buttons")

driver %>% getEl('#dlPlotOpenModalBtn') %>% click()
root <- wait_for('#modalDownloadPlotOptions[style="display: block;"]', driver)

test_that('#dlPlotOpenModalBtn button works fine', {
  expect_true(!is.null(wait_for('a#dlPlotHandler', root)))
})

root %>% getEl('button.close') %>% click()
wait_for('#modalDownloadPlotOptions[style="display: none;"]', driver)
