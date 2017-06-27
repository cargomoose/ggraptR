#' Launch ggraptR in the default browser
#'
#' @param initialDf initial dataframe to plot
#' @param ... extra arguments. shiny::runApp arguments or initialPlot if needed
#' 
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR(initialDf='mtcars', initialPlot=c('Scatter', 'Line'), appDir='inst/ggraptR')
#' }
#' @importFrom ggplot2 diamonds
#' @export
ggraptR <- function(initialDf = ggplot2::diamonds, ...) {
  extraArgs <- list(...)
  if (is.null(extraArgs$block_rsession) || !extraArgs$block_rsession) {
    do.call(run_external_ggraptR, args = c(
      extraArgs, if (!'launch.browser' %in% names(extraArgs)) list(launch.browser = T)))
    return()
  }
  extraArgs$block_rsession <- NULL
  
  if ('initialPlot' %in% names(extraArgs)) {
    initialPlot <- extraArgs$initialPlot
  }
  shinyArgs <- extraArgs[names(extraArgs) != 'initialPlot']
  
  defaultShinyArgs <- list(
    appDir=system.file("ggraptR", package = "ggraptR"),
    display.mode='normal', port=6012, launch.browser=T)
  for (defName in names(defaultShinyArgs)) {
    if (is.null(shinyArgs[[defName]])) {
      shinyArgs[[defName]] <- defaultShinyArgs[[defName]]
    }
  }
  
  if (shinyArgs$appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = F)
  }
  if ('' %in% names(shinyArgs)) stop('all extra arguments must be named')
  
  stopifnot(is.data.frame(initialDf))
  initialDfName <- gsub('.*::', '', deparse(substitute(initialDf)))
  
  print(shinyArgs)
  do.call(shiny::runApp, args=shinyArgs)
  cat('>> Exit ggraptR()\n')
}


run_external_ggraptR <- function(...) {
  # suppressPackageStartupMessages(require(dplyr))
  ggraptrArgLst <- list(...)
  ggraptrArgLst$block_rsession <- T  # to prevent recursion
  log_file <- ggraptrArgLst$log_file
  if (is.null(log_file)) log_file <- paste0(Sys.getenv('R_USER'), '/ggraptR.log')
  ggraptrArgLst$log_file <- NULL
  
  if (!file.exists(log_file)) {
    file.create(log_file)
  } else {
    if (!any(grepl('Exit ggraptR', suppressWarnings(tail(readLines(log_file)))))) {
      warning('The previous ggraptR session was not closed properly')
    }
  }
  
  if (is.null(ggraptrArgLst$launch.browser)) ggraptrArgLst$launch.browser <- F
  cmds <- c('Sys.getpid()',
            'suppressPackageStartupMessages(library(ggraptR))',
            sprintf('suppressPackageStartupMessages(ggraptR(%s))', 
                    lst_to_string(ggraptrArgLst)))
  
  cmd_line <- sprintf('R -q %s >%s 2>&1', 
                      paste(paste0('-e "', cmds, '"'), collapse = ' '), log_file)
  system(cmd_line, wait=F)
  
  i_trial <- 8
  while (!any(grepl('Listening on', suppressWarnings(readLines(log_file))))) {
    if (i_trial == 0) stop(
      'Could not launch ggraptR. Duplicated instance? Logs: ', log_file)
    i_trial <- i_trial - 1
    Sys.sleep(1)
  }
}

lst_to_string <- function(x) {
  lst <- Map(function(el) if (is.null(el)) 'NULL' else el, 
             if (is.list(x)) x else list(x))  # list(x) prevents coertions
  
  1:length(lst) %>% 
    vapply(function(i) {
      keys <- names(lst)
      val <- if (is.character(lst[[i]])) sprintf("'%s'", lst[[i]]) else lst[[i]]
      if (is.null(keys) || keys[i] == '') val else paste(keys[i], val, sep='=')
    }, FUN.VALUE='') %>% 
    paste(collapse=', ')
}
