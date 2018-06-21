pacman::p_load(testthat, ggraptR)


if (Sys.getenv("NOT_CRAN") == "true") {  # like global skip_on_cran
  # https://github.com/r-lib/testthat/issues/144#issuecomment-396902791
  Sys.setenv("R_TESTS" = "")  # accroding to https://github.com/hadley/testthat/issues/144
  test_check("ggraptR")
}
