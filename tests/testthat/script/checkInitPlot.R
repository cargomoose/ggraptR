source('script/utils/funs.R')
# killExternalRprocess()  # if the last run was finished with an error

unlink('report/*')  # to clear 'report' folder content

list2env(run_external_ggraptR(), environment()) %>% invisible()

test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  expect_true(has_shiny_correct_state(driver, '^diamonds', NULL, NULL, 
                                      shortShotName=F, waitPlot=F))
})
