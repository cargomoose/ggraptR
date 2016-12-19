rm(list=ls())
invisible(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
                                  detach, character.only=TRUE, unload=TRUE)))
.rs.restartR()  # to release dll files in devtools packages
#### wait and run separately

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
library(ggraptR)
ggraptR()
#### close manually


.rs.restartR()  # releases dlls
unlink(testDir, T, T)
.libPaths(origUserLibPath)
print(.libPaths())
