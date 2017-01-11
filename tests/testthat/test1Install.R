context("Installation on clean machine")

#### clean ####
.libPaths(Sys.getenv('R_LIBS_USER'))
invisible(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
                                  detach, character.only=TRUE, unload=TRUE)))
rm(list=ls())
suppressPackageStartupMessages({
  library(devtools)
  library(stringi)
  library(RSelenium)
  library(testthat)
  library(curl)  # contains the only dll that is hard to unload
  library(digest)
  library(httr)
})

#### paths ####
userLibRoot <- dirname(.libPaths()[1])
testDir <- paste0(userLibRoot, '/test')
predefPack <- paste0(userLibRoot, '/predefPack')

#### prepare folders, workspace and libraries ####
if (file.exists(testDir)) {
  unlink(testDir, T, T)
  if (file.exists(testDir)) {
    Sys.sleep(2)
    unlink(testDir, T, T)
  }
  if (file.exists(testDir)) stop('testDir still exists')
}

dir.create(testDir)
.libPaths(testDir)

if (file.exists(predefPack)) {
  invisible(file.copy(paste0(predefPack, '/', dir(predefPack)), testDir, recursive=T))
} else {
  suppressPackageStartupMessages(install.packages(c('devtools', 'testthat')))
  rselenium_path <- find.package('RSelenium', quiet = T)
  if (nchar(rselenium_path)) {
    invisible(file.copy(rselenium_path, testDir), recursive=T)
    invisible(file.copy(rselenium_path, predefPack), recursive=T)
  } else {
    suppressPackageStartupMessages(install.packages('RSelenium'))
    invisible(file.copy(paste0(testDir, '/RSelenium'), predefPack), recursive=T)
  }
  invisible(file.copy(testDir, paste0(predefPack, '/', dir(predefPack)), recursive=T))
}


res <- try(library(ggraptR), silent=T)
if (!(class(res) == 'try-error' && grepl("no package called .ggraptR", res[1]))) {
  browser()
  stop()
}

#### install ggraptR from git. Run it and check the initial plot ####
install(dirname(dirname(getwd())))
browser()
suppressPackageStartupMessages(library(ggraptR))
source('script/checkInitPlot.R')
release_externals()


#### restore and clean ####
for (pkg in paste('package:',names(sessionInfo()$otherPkgs),sep=""))
  suppressWarnings(detach(pkg, character.only=TRUE, unload=TRUE))
# while (!is.null(pkgs <- names(sessionInfo()$loadedOnly)))
#   for (pkg in pkgs) try(unloadNamespace(pkg), T)
for (dll in sapply(getLoadedDLLs(), `[[`, 'path'))
  if (startsWith(dll, testDir)) dyn.unload(dll)  # release dlls to unlink

unlink(testDir, T, T)
.libPaths(Sys.getenv('R_LIBS_USER'))
closeAllConnections()
