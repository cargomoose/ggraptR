context("Test ggraptR() run opportunity")

if (.Platform$OS.typ != 'windows') return()
source('script/utils/funs.R')

gg_arg_combinations <- list(
  list(NULL),
  # list(initialDf=get??('esoph')),  # problems with passing data to external process
  list(initialPlot='Line'),
  list(appDir=system.file("ggraptR", package = "ggraptR"))
  )

for (args in gg_arg_combinations) {
  run_result <- F
  test_that('ggraptR runs', {
    list2env(do.call(get_selenium_externals, args), environment()) %>% invisible()
    release_externals()
    succeed()
  })
}
