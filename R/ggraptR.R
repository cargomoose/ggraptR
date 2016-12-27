#' Launch ggraptR in the default browser
#'
#' @param initialDf initial dataframe to plot
#' @param appDir sets up application directory
#' @param initialPlot initial plot name from globals.R definedPlotInputs list
#' @param port port for shiny::runApp(..port=)
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
                    initialPlot=NULL, port=6012) {
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = F)
  }
  
  # this variable will be used in generalWidgets.R with 'sys.frame(1)'
  if (typeof(initialDf) != "character") initialDf <- deparse(substitute(initialDf))
  if (!exists(initialDf)) stop('Initial dataset not found')
  
  shiny::runApp(appDir, display.mode = "normal", port=port)
}
