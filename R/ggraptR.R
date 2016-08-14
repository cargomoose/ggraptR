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

ggraptR <- function(df="diamonds") {
  
  appDir <- system.file("ggraptR", package = "ggraptR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  if (typeof(df) == "character")
  {
    gDefaultDataFrame <<- df    
  }
  else
  {
    gDefaultDataFrame <<- list(deparse(substitute(df)))
  }

  shiny::runApp(appDir, display.mode = "normal")
  
}
