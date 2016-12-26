source('funs.R')

# R -e "ggraptR::ggraptR(port=%s)
port <- 5050
runAppCmd <- sprintf("shiny::runApp(\'%s\', port=%s, launch.browser=F)", 
                     system.file("ggraptR", package = "ggraptR"), port)
cmd <- sprintf('R -q -e "Sys.getpid()" -e "%s"', runAppCmd)
cat('cmd:', cmd, '\n')
# system(cmd, wait=F)

if (exists('selenRpid')) {
  closeAllConnections()
  system(paste('taskkill /f /pid', selenRpid))
}

selenPipe <- pipe(cmd, open='r')
selenRpid <- gsub('\\[1\\] ', '', readLines(selenPipe, 2)[2])

selServer <- startSelServer()
driver <- getDriver(port)
stopifnot(driver$getTitle()[[1]] == 'ggraptR')

test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  # driver$screenshot(T)
  expect_true(has_shiny_correct_state(driver, 'diamonds', NULL, NULL, waitPlot=F))
})
