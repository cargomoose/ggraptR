context("Test ggraptR() run opportunity")

if (.Platform$OS.typ != 'windows') return()
source('script/utils/funs.R')

gg_arg_combinations <- list(
  list(NULL), 
  list('diamonds'), 
  list('esoph'), 
  list(initialPlot='Line'),
  list(appDir=system.file("ggraptR", package = "ggraptR")))  # removed a test of port arg

for (args in gg_arg_combinations) {
  run_result <- F
  test_that(paste('ggraptR runs with arguments:', as_string(args)), {
    list2env(do.call(get_selenium_externals, args), environment()) %>% invisible()
    assign('run_result', T, envir=parent.env(environment()))
    release_externals()
    succeed()
  })
  stopifnot(run_result)
}
