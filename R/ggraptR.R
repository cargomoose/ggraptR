#' Launch ggraptR in the default browser
#'
#' @param initialDf initial dataframe to plot
#' @param appDir sets up application directory
#' @param initialPlot initial plot name from globals.R definedPlotInputs list
#' 
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR('mtcars', 'inst/ggraptR')
#' }
#' @export
ggraptR <- function(initialDf="diamonds", 
                    appDir=system.file("ggraptR", package = "ggraptR"),
                    initialPlot='scatter') {
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = F)
  }
  
  # this variable will be used in generalWidgets.R with 'sys.frames()[[1]]'
  initialDf <- if (typeof(initialDf) == "character")
    initialDf else list(deparse(substitute(initialDf)))

  shiny::runApp(appDir, display.mode = "normal")
}
