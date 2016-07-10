#' Launch ggraptR in the default browser
#'
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR()
#' }
#' @export
ggraptR <- function() {
  appDir <- system.file("ggraptR", package = "ggraptR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

