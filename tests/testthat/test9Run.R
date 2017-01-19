context("Test ggraptR() run opportunity")
source('script/utils/funs.R')

gg_arg_combinations <- list(
  NULL, 
  'diamonds', 
  'esoph', 
  list(initialPlot='Line'),
  list(appDir=system.file("ggraptR", package = "ggraptR")))  # removed a test of port arg

for (args in gg_arg_combinations) {
  run_result <- F
  test_that(paste('ggraptR runs with arguments:', as_string(args)), {
    list2env(get_selenium_externals(args), environment()) %>% invisible()
    assign('run_result', T, envir=parent.env(environment()))
    release_externals()
    succeed()
  })
  stopifnot(run_result)
}
