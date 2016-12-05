waitForPlotReady <- function() {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  stopifnot(driver$getWindowSize()$height== 1080 && driver$getWindowSize()$width == 1920)
  emptyPicContent <- paste0(rep('A', 1e3),collapse='')
  
  res <- waitFor(sprintf('#plot img[src*="%s"]', emptyPicContent), 
                 errorIfNot=F, timeout = 4)  # blank
  if (!is.logical(res) || res) {
    res <- waitFor(c(sprintf('#plot img:not([src*="%s"])', emptyPicContent), 
                     '#plot.shiny-output-error'))  # normal plot or an err
  }
  res
}

getOptions <- function(driver, id) {
  # shiny 'select' inputs does not have their options from start. Load on click
  idEl <- getEl(driver, c('#', id))
  if (attr(idEl, 'data-shinyjs-resettable-type') != "Select") {
    stop('Wrong id for select element: ', id)
  }
  
  selCtrlEl <- getEl(driver, c('select#', id, ' + div'))
  selEl <- getEl(selCtrlEl, '.selectize-input')
  availOptsQuery <- '.option:not(.selected)'
  
  if (!grepl('\\binput-active\\b', attr(selEl, 'class'))) {
    selEl %>% click()  # makes available options visible
    waitFor('.selectize-input.input-active', selCtrlEl)
  }
  selCtrlEl %>% getEls(availOptsQuery)
}

eraseMultiSelectOpts <- function(driver, selectId, howMany=1) {
  eraseOpts <- function(driver, selectId, n) {
    getEl(driver, c('#', selectId, ' + .selectize-control input'))$
    sendKeysToElement(as.list(rep(selKeys$backspace, n)))
  }
  getItemsLength <- function(driver, selectId) {
    getEls(driver, c('#', selectId, ' + .selectize-control .item')) %>% 
      length()
  }
  
  nItemsBeforeErasing <- getItemsLength(driver, selectId)
  isEraseAll <- nItemsBeforeErasing == howMany
  eraseOpts(driver, selectId, max(1, howMany - 1))
  if (howMany > 1) {
    waitForPlotReady()
    getOptions(driver, selectId)  # sets the focus to the select element
    if (!isEraseAll) nItemsBeforeErasing <- getItemsLength(driver, selectId)
    eraseOpts(driver, selectId, 1)
  }
  # waitFor(quote(
    # is.null(getEl(driver, c('#', selectId, 'Ctrl .selectize-input.has-items')))))
  # driver %>% getEl(sprintf('#%sCtrl .selectize-input', selectId)) %>% html %>% print
  #  .input-active.dropdown-active
  
  
  waitFor(if (isEraseAll) sprintf('#%sCtrl .selectize-input:not(.focus)', selectId) else
    quote(nItemsBeforeErasing != getItemsLength(driver, selectId)))
}

getCurrentPlotNames <- function(driver) {
  getEls(driver, '#plotTypes option') %>% text()
}

test_shiny_correct <- function(driver, plotNames, elId, makeShot=T, waitPlot=T) {
  if (waitPlot) waitForPlotReady()
  if (makeShot) {
    driver$screenshot(
      file = sprintf('%s/auto/report/%s-%s-%s.png', getwd(), 
                     pastePlus(plotNames), toString(elId),
                     round(as.numeric(Sys.time())*1e3)))
  }
  !length(getEls(driver, '.shiny-output-error'))
}
