source('script/utils/funs.R')
killExternalRprocessAnywhere()  # if the last run was finished with an error


port <- 5050
cmds <- c('Sys.getpid()',
          'suppressPackageStartupMessages(library(ggraptR))',
  sprintf('suppressPackageStartupMessages(ggraptR(port=%s, launch.browser=F))', port))

# pipe does not like ';' in "R -e .." that's why created generate_r_cmd() exists
selPipe <- pipe(generate_r_cmd(cmds), open='r')  # system(cmd, wait=F)
selPid <- gsub('\\[1\\] ', '', readLines(selPipe, 2)[2])

selServer <- startSelServer()
driver <- getDriver(port=port)

if (driver$getTitle()[[1]] != 'ggraptR') {
  # If hangs before the next message [>5 sec] close the process manually
  cat('\nTrying to check the reason why [driver$getTitle()[[1]] != "ggraptR"]', fill=T)
  errMsg <- head(suppressWarnings(system(generate_r_cmd(cmds), intern=T)), -2)
  stopExternals(paste(c('Page title does not match. Reason:', errMsg), collapse='\n'))
}

test_that("Initial diamonds plot is correct", {
  waitForPlotReady(driver)
  expect_true(has_shiny_correct_state(driver, '^diamonds', NULL, NULL, 
                                      shortShotName=F, waitPlot=F))
})
