rm(list=ls())
library(RSelenium)
library(dplyr)
library(testthat)

unlink(paste0(getwd(), '/auto/report/*'))  # to clear 'report' folder content
