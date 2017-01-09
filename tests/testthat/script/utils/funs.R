rm(list=ls())
library(RSelenium)
library(dplyr)
library(testthat)

source('script/utils/seleniumUtils.R')
source('script/utils/shinyUtils.R')
source('script/utils/ggraptrUtils.R')

unlink('report/*')  # to clear 'report' folder content

isNotFoundException <- function(e) {
  any(c('error', 'try-error') %in% class(e)) && grepl('object .* not found',unlist(e[1]))
}

isStaleException <- function(e) grepl('StaleElementReference', getErrorMessage(e))

is.error <- function(obj) any(c('error', 'try-error') %in% class(obj))

getErrorMessage <- function(err) {
  stopifnot(is.error(err))
  unlist(err[1])
}

get.anywhere <- function(strObjName) {
  get(strObjName, 
      envir=Filter(function(x) strObjName %in% ls(envir=x), 
                   sys.frames())[[1]])
}

eval.in.any.env <- function(targetExpr) {
  tryCatch(eval(targetExpr), error=function(e) {
    if (isNotFoundException(e)) {
      for (env in sys.frames()) {
        res <- try(eval(targetExpr, envir = env), T)
        if (!isNotFoundException(res)) {
          # cat(which(ls(envir=sys.frame(3)) == sapply(
            # sys.frames(), function(en) ls(envir=en))), length(sys.frames()), '\n')
          if (is.error(res)) stop(getErrorMessage(res))
          return(res)
        }
      }
    }
    stop(getErrorMessage(e))
  })
}

pastePlus <- function(..., shorten=T) {
  paste(if (shorten) sapply(c(...), function(x) substr(gsub(' ', '', x), 1, 4))
        else c(...), 
        collapse='+')
}

getAllPlotNames <- function() {
  source('../../inst/ggraptR/globals.R')
  source('../../inst/ggraptR/functions/helper.R')
  flattenList(getDefinedPlotInputs()) %>% names %>% 
    sapply(capitalize) %>% 
    sapply(function(x) gsub('(\\d)', ' \\1', x))
}
