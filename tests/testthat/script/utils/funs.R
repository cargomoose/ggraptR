pacman::p_load(RSelenium, dplyr)

source('../../inst/ggraptR/functions/helper.R')
sourceAllInDir('script/utils', except = 'funs.R')


eval.in.any.env <- function(targetExpr) {
  # sys.frames() %>% sapply(function(env) ls(envir=env)  # for debug
  if (class(substitute(targetExpr)) == '{') targetExpr <- as.call(substitute(targetExpr))

  for (env in sys.frames()) {
    res <- try(eval(targetExpr, envir = env), T)
    if (!is_error_of(res, 'object .* not found') && 
        !is_error_of(res, 'Summary: NoSuchDriver') && length(res)) {
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

debug_stop <- function(msg, expr=NULL) {
  driver$screenshot(T)
  if (!is.null(substitute(expr))) {
    expr_val <- eval(expr)
  }
  browser()
  if (need_stop_externals) {
    stop_externals(msg)
  } else {
    stop(msg)
  }
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

getAllPlotNames <- function(n_num=3, n_cat=3) {
  source('../../inst/ggraptR/globals.R')
  source('../../inst/ggraptR/functions/helper.R')
  flattenList(getDefinedPlotInputs(n_num, n_cat)) %>% names %>% 
    sapply(capitalize) %>% 
    sapply(function(x) gsub('(\\d)', ' \\1', x))
}
