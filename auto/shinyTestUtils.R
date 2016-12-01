waitPlotReady <- function() {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  stopifnot(driver$getWindowSize()$height== 1080 && driver$getWindowSize()$width == 1920)
  emptyPicContent <- paste0(rep('A', 1e3),collapse='')
  
  waitFor(sprintf('#plot img[src*="%s"]', emptyPicContent))  # blank plot
  waitFor(c(sprintf('#plot img:not([src*="%s"])', emptyPicContent), 
            '#plot.shiny-output-error')) %>% invisible  # normal plot or an error
}

getOptions <- function(driver, id) {
  # shiny 'select' inputs does not have their options from start. Load on click
  idEl <- getEl(driver, c('#', id))
  if (attr(idEl, 'data-shinyjs-resettable-type') != "Select") {
    stop('Wrong id for select element: ', id)
  }
  
  selCtrlEl <- getEl(driver, c('select#', id, ' + div'))
  # if (!is.null(attr(idEl, 'multiple'))) {
  getEl(selCtrlEl, '.selectize-input') %>% click()
  res <- waitFor('.option:not(.selected)', selCtrlEl)
  if (!is.list(res)) list(res) else res
}

eraseMultiSelectOpts <- function(driver, selectId, howMany=1) {
  getEl(driver, '#', selectId, ' + .selectize-control input')$
    sendKeysToElement(as.list(rep(selKeys$backspace, howMany)))
}

getCurrentPlotNames <- function(driver) {
  getEls(driver, '#plotTypes option') %>% text()
}

test_shiny_correct <- function(driver, plotNames, elId, makeShot=T, waitPlot=T) {
  if (waitPlot) waitPlotReady()
  if (makeShot) {
    driver$screenshot(display = T)
    driver$screenshot(
      file = sprintf('%s/auto/report/%s-%s-%s.png', getwd(), 
                     paste(plotNames, collapse='+'), toString(elId),
                     round(as.numeric(Sys.time())*1e3)))
  }
  !length(getEls(driver, '.shiny-output-error'))
}
