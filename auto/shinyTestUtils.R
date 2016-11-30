waitPlotReady <- function() {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  stopifnot(driver$getWindowSize()$height== 1080 && driver$getWindowSize()$width == 1920)
  emptyPicContent <- paste0(rep('A', 1e3),collapse='')
  
  waitFor(sprintf('#plot img[src*="%s"]', emptyPicContent))  # blank plot
  waitFor(c(sprintf('#plot img:not([src*="%s"])', emptyPicContent), 
            '#plot.shiny-output-error')) %>% invisible  # normal plot or an error
}

fillOptions <- function(driver, id) {
  # shiny 'select' inputs does not have their options from start. Load on click
  stopifnot(attr(getEl(driver, c('#', id)), 'data-shinyjs-resettable-type') == "Select")
  selCtrlEl <- getEl(driver, c('select#', id, ' + div'))
  getEl(selCtrlEl, '.selectize-input') %>% click()
  waitFor('.option', selCtrlEl)
}

test_shiny_correct <- function(driver, makeShot=T, waitPlot=T) {
  if (waitPlot) waitPlotReady()
  if (makeShot) {
    driver$screenshot(display = T)
    driver$screenshot(
      file = sprintf('%s/auto/report/%s.png', getwd(), round(as.numeric(Sys.time())*1e3)))
  }
  !length(getEls(driver, '.shiny-output-error'))
}
