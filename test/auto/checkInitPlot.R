if (nrow(showConnections())) {
  if (!exists('selPid')) stop('selPid must exist')
  system(paste('taskkill /f /pid', selPid), show.output.on.console = F)
  closeAllConnections()
  if (nrow(showConnections())) stop('Can not close all connections')
}

source('funs.R')

# R -e "ggraptR::ggraptR(port=%s)
port <- 5050
runAppCmd <- sprintf("shiny::runApp(\'%s\', port=%s, launch.browser=F)", 
                     system.file("ggraptR", package = "ggraptR"), port)
cmd <- sprintf('R -q -e "Sys.getpid()" -e "%s"', runAppCmd)
cat('cmd:', cmd, '\n')

selPipe <- pipe(cmd, open='r')  # system(cmd, wait=F)
selPid <- gsub('\\[1\\] ', '', readLines(selPipe, 2)[2])
selServer <- startSelServer()
driver <- getDriver(port)
if (driver$getTitle()[[1]] != 'ggraptR') {
  browser()
  stop('Page title does not match')
}

test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  # driver$screenshot(T)
  expect_true(has_shiny_correct_state(driver, 'diamonds', NULL, NULL, waitPlot=F))
})
