#' Launch ggraptR in the default browser
#'
#' @param initialDf initial dataframe to plot
#' @param appDir sets up application directory
#' 
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR('mtcars', 'inst/ggraptR')
#' }
#' @export
ggraptR <- function(initialDf="diamonds", 
                    appDir=system.file("ggraptR", package = "ggraptR")) {
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = F)
  }
  
  # initial "global" variables
  # current environment will be used like global variable list
  # see inst/ggraptR/global_variables.R
  initialDf <- if (typeof(initialDf) == "character")
    initialDf else list(deparse(substitute(initialDf)))
  fileDefault <- list(width=10, height=10, DPI=100, widthMax=50,
                      heightMax=50, DPIMax=500)

  shiny::runApp(appDir, display.mode = "normal")
}
