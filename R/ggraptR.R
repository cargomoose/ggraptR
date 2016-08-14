#' Launch ggraptR in the default browser
#'
#' @details See \url{http://github.com/cargomoose/raptR} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   ggraptR()
#' }
#' @export

#prep the default data frame global variable to be used by ggraptR (see generalWidgets.R)
gDefaultDataFrame <<- ""

ggraptR <- function(df) {
  
  appDir <- system.file("ggraptR", package = "ggraptR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  gDefaultDataFrame <<- df

  shiny::runApp("/home/juliux/raptr/inst/ggraptR", display.mode = "normal") #appDir
  
}

