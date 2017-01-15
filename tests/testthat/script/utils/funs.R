suppressPackageStartupMessages(library(RSelenium))
suppressPackageStartupMessages(library(dplyr))

source('script/utils/seleniumUtils.R')
source('script/utils/shinyUtils.R')
source('script/utils/ggraptrUtils.R')


eval.in.any.env <- function(targetExpr) {
  # sys.frames() %>% sapply(function(env) ls(envir=env)  # for debug
  if (class(substitute(targetExpr)) == '{') targetExpr <- as.call(substitute(targetExpr))

  for (env in sys.frames()) {
    res <- try(eval(targetExpr, envir = env), T)
    if (!is_error_of(res, 'object .* not found') && 
        !is_error_of(res, 'Summary: NoSuchDriver')) {
      if (is.error(res)) stop(getErrorMessage(res))
      return(res)
    }
  }
  stop(getErrorMessage(res))
}

is.error <- function(obj) any(c('error', 'try-error') %in% class(obj))

getErrorMessage <- function(err) {
  stopifnot(is.error(err))
  unlist(err[1])
}

is_error_of <- function(e, msg) {
  is.error(e) && grepl(msg, getErrorMessage(e))
}

get.anywhere <- function(strObjName) {
  get(strObjName, 
      envir=Filter(function(x) strObjName %in% ls(envir=x), 
                   sys.frames())[[1]])
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

# pipe does not like ';' in "R -e .." that's why created generate_r_cmd() exists
generate_r_cmd <- function(cmds, out_file_name) {
  sprintf('R -q %s >%s 2>&1', 
          paste(paste0('-e "', cmds, '"'), collapse = ' '), out_file_name)
}
