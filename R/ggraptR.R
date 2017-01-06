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
#' @export
ggraptR <- function(initialDf="diamonds", ...) {
  shinyArgs <- list(...)
  
  if ('initialPlot' %in% names(shinyArgs)) {
    initialPlot <- shinyArgs$initialPlot
    shinyArgs$initialPlot <- NULL
  }
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
  
  # this variable will be used in generalWidgets.R with 'sys.frame(1)'
  if (typeof(initialDf) != "character") initialDf <- deparse(substitute(initialDf))
  if (!exists(initialDf)) {
    if (initialDf == 'diamonds') library(ggplot2) else stop('Initial dataset not found')
  }
  
  do.call(shiny::runApp, args=shinyArgs)
}
