library(testthat)
library(ggraptR)


if (Sys.getenv("NOT_CRAN") == "true") {  # like global skip_on_cran
  Sys.setenv("R_TESTS" = "")  # accroding to https://github.com/hadley/testthat/issues/144
  test_check("ggraptR")
}
