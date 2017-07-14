wait_for_plot_ready <- function(driver, need_blank_plot=F) {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  if (driver$getWindowSize()$height != 1080 || driver$getWindowSize()$width != 1920) {
    stop_externals('Wrong driver screen resolution')
  }
  
  Sys.sleep(2)  # for blank plot
  if (need_blank_plot) return()
  
  emptyPicHtml <- paste0(rep('A', 1e3), collapse='')
  # emptyPicQuery    <- sprintf('#plot img[src*="%s"]', emptyPicHtml)
  nonEmptyPicQuery <- sprintf('#plot img:not([src*="%s"])', emptyPicHtml)
  # isBlank <- wait_for(emptyPicQuery, source=driver, errorIfNot=F, timeout = 4)
  res <- wait_for(c(nonEmptyPicQuery, '#plot.shiny-output-error'), source = driver)
  
  wait_for('html:not(.shiny-busy)', source = driver)  # for long calculations like pairs
  res
}

wait_for_table_ready <- function(driver) {
  Sys.sleep(0.5)
  wait_for(paste0('.dataTables_processing[style="display: none;"]'), driver)
  # wait_for({ !isVisible(driver %>% getEl('.dataTables_processing')) }, driver)
}

# target - charater of css/xpath query, function returning T/F, expression like {a+2==5}
# source - webDriver or webElement
wait_for <- function(target, source=driver, timeout=20, errorIfNot=T, catchStale=F) {
  oneWaitDur <- 0.5  # sec
  
  targetFun <- 
    if (class(substitute(target)) == '{') {  # for quoted expression
      call_obj <- as.call(substitute(target))
      # target expression must not started with 'if'. Fail-fast
      stopifnot(!startsWith(as.character(call_obj[2]), 'if ('))
      function(placeholder) eval.in.any.env(call_obj)
    } else if (is.function(target)) {
      target
    } else if (is.character(target)) {
      function(x) unlist(lapply(target, function(el) getEls(x, el)))
    } else {
      stop(sprintf('Not implemented for target class [%s]', class(target)))
    }
  
  tm <- Sys.time()
  while (Sys.time() - tm < timeout) {
    res <- suppressMessages(tryCatch(
      targetFun(source),
      error=function(e) {
        if (is_error_of(e, 'Summary: UnknownError') || 
            (catchStale && is_error_of(e, 'StaleElementReference'))) {
          FALSE
        } else {
          debug_stop(e$message)
        }
      }))
    
    if (is.list(res)) {
      if (length(target) == 1) {
        if (length(res)) {
          return(invisible(if (length(res) > 1) res else res[[1]]))
        }
      } else {
        if (length(Filter(length, res)) == 1) {
          return(Filter(length, res)[[1]])
        }
      }
    } else if (is.logical(res) && res) {
      return(TRUE)
    }
    Sys.sleep(oneWaitDur)
  }
  
  if (errorIfNot) {
    debug_stop('Could not wait')
  }
  FALSE
}
