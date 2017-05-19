cat("\nInitial plot")
source('script/utils/funs.R')
# killExternalRprocess()  # if the last run was finished with an error

# to clean 'report' folder content
unlink(paste0(Sys.getenv('R_USER'), '/report/*'))
# assigns 'driver' to the current environment
list2env(get_selenium_externals(), environment()) %>% invisible()

test_that("Initial diamonds plot is correct", {
  wait_for_plot_ready(driver)
  expect_true(has_shiny_correct_state(driver, '^diamonds', NULL, NULL, 
                                      shortShotName=F, waitPlot=F))
})
