# # this test can spoil loaded httr and RSelenium packages so it was renamed and testthat
# # does not see it anymore. Use '.rs.restartR()' after test to repair broken packages
# # Run this test using 'testthat::test_file('tests/testthat/checkInstallation.R')'
# context("Installation on clean machine")
# 
# 
# #### clean ####
# .libPaths(Sys.getenv('R_LIBS_USER'))
# invisible(suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),
#                                   detach, character.only=TRUE, unload=TRUE)))
# rm(list=ls())
# suppressPackageStartupMessages({
#   library(devtools)
#   library(stringi)
#   library(RSelenium)
#   library(testthat)
#   library(curl)
#   library(digest)
#   library(httr)
# })
# initialLoadedPkgs <- names(sessionInfo()$loadedOnly)
# 
# 
# #### paths ####
# userLibRoot <- dirname(.libPaths()[1])
# testDir <- paste0(userLibRoot, '/test')
# predefPack <- paste0(userLibRoot, '/predefPack')
# 
# 
# #### prepare folders, workspace and libraries ####
# if (file.exists(testDir)) {
#   unlink(testDir, T, T)
#   if (file.exists(testDir)) {
#     evalq({
#       dlls <- getLoadedDLLs() %>% sapply(`[[`, 'path') %>%
#         Filter(function(dll) grepl('/test/', dll) ,.)  # for jsonlite.dll
#       for (i in length(dlls)) {
#         library.dynam.unload(names(dlls)[i], dirname(dirname(dirname(dlls[i]))))
#       }
#       # getLoadedDLLs() %>% sapply(`[[`, 'path') %>%
#       #   Filter(function(dll) grepl('/test/', dll) ,.) %>% sapply(dyn.unload)
#     }, envir=.GlobalEnv)
#     unlink(testDir, T, T)
#   }
#   if (file.exists(testDir)) {
#     stop('Could not uninstall testDir. ',
#          'Restart R session (possibly with ".rs.restartR()") to release previous dlls')
#   }
# }
# 
# dir.create(testDir)
# .libPaths(testDir)
# 
# if (file.exists(predefPack)) {
#   invisible(file.copy(paste0(predefPack, '/', dir(predefPack)), testDir, recursive=T))
# } else {
#   suppressPackageStartupMessages(install.packages(c('devtools', 'testthat')))
#   rselenium_path <- find.package('RSelenium', quiet = T)
#   if (nchar(rselenium_path)) {
#     invisible(file.copy(rselenium_path, testDir), recursive=T)
#     invisible(file.copy(rselenium_path, predefPack), recursive=T)
#   } else {
#     suppressPackageStartupMessages(install.packages('RSelenium'))
#     invisible(file.copy(paste0(testDir, '/RSelenium'), predefPack), recursive=T)
#   }
#   invisible(file.copy(testDir, paste0(predefPack, '/', dir(predefPack)), recursive=T))
# }
# 
# 
# res <- try(library(ggraptR), silent=T)
# if (!(class(res) == 'try-error' && grepl("no package called .ggraptR", res[1]))) {
#   debug_stop('no package called .ggraptR')
# }
# 
# 
# #### install ggraptR from git. Run it and check the initial plot ####
# install(dirname(dirname(getwd())))
# suppressPackageStartupMessages(library(ggraptR))
# source('script/checkInitPlot.R')
# release_externals()
# 
# 
# #### restore and clean ####
# for (pkg in paste('package:', names(sessionInfo()$otherPkgs), sep=""))
#   suppressWarnings(detach(pkg, character.only=TRUE, unload=TRUE))
# 
# for (i in 10:1) {
#   pkgs <- setdiff(names(sessionInfo()$loadedOnly), initialLoadedPkgs)
#   if (!length(pkgs)) break
#   for (pkg in pkgs) try(unloadNamespace(pkg), T)
# }
# 
# unlink(testDir, T, T)
# .libPaths(Sys.getenv('R_LIBS_USER'))
# closeAllConnections()
# print('Success')
