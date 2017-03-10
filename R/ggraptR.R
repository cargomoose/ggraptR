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
ggraptR <- function(initialDf = diamonds, ...) {
  extraArgs <- list(...)

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
  initialDfName <- deparse(substitute(initialDf))
  
  do.call(shiny::runApp, args=shinyArgs)
}
