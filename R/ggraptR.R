#' Launch ggraptR in the default browser
#'
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR()
#' }
#' @export
ggraptR <- function(defaultDataFrame="") {
  appDir <- system.file("ggraptR", package = "ggraptR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  # set global variable for selected data frame (see generalWidgets.R)
  gDefaultDataFrame <<- defaultDataFrame
  shiny::runApp(appDir, display.mode = "normal")
}
