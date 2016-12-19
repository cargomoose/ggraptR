rm(list=ls())
invisible(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
                                  detach,character.only=TRUE,unload=TRUE)))
.rs.restartR()  # to release dll files in devtools packages

origUserLibPath <- .libPaths()[1]
userLibRoot <- dirname(origUserLibPath)
testDir <- paste0(userLibRoot, '/test')
devtoolsPack <- paste0(userLibRoot, '/devtoolsPack')

if (file.exists(testDir)) {
  unlink(testDir, T, T)
}

dir.create(testDir)
.libPaths(testDir)

if (file.exists(devtoolsPack)) {
  invisible(file.copy(paste0(devtoolsPack, '/', dir(devtoolsPack)), testDir, recursive=T))
} else {
  install.packages('devtools')
}

stopifnot({
  res <- try(library(ggraptR), silent=T)
  class(res) == 'try-error' && grepl("package called ‘ggraptR’", res[1])
})

devtools::install_github('cargomoose/ggraptR')
install.packages('backports')  # 1.0.4
install.packages('knitr')  # 1.15.1
install.packages('colourpicker')  # 0.3
install.packages('gdtools')  # 0.1.3
install.packages('RColorBrewer')  # 1.1.2
library(ggraptR)

ggraptR()
# remove test folder
.libPaths(origUserLibPath)
