context("Test ggraptR() run opportunity")
source('script/utils/funs.R')

gg_arg_combinations <- list(
  NULL, 
  'diamonds', 
  'esoph', 
  list(initialPlot='Line'),
  list(port=5051, appDir=system.file("ggraptR", package = "ggraptR")))

for (args in gg_arg_combinations) {
  run_result <- F
  test_that(paste('ggraptR runs with arguments:', as_string(args)), {
    list2env(run_external_ggraptR(args), environment()) %>% invisible()
    assign('run_result', T, envir=parent.env(environment()))
    release_externals()
    succeed()
  })
  stopifnot(run_result)
}
