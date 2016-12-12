has_shiny_correct_state <- function(driver, plotNames, elId, elVal, waitPlot=T) {
  if (waitPlot) waitForPlotReady(driver)
  fileName <- sprintf('%s/auto/report/%s_[%s=%s].png', getProjWd(), 
                      pastePlus(plotNames), toString(elId), substr(toString(elVal), 1, 5))
  if (!is.character(fileName)) {
    driver$screenshot(T)
    browser()
  }
  driver$screenshot(file=fileName)
  !length(getEls(driver, '.shiny-output-error'))
}

waitForPlotReady <- function(driver) {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  stopifnot(driver$getWindowSize()$height== 1080 && driver$getWindowSize()$width == 1920)
  emptyPic <- paste0(rep('A', 1e3),collapse='')
  
  res <- waitFor(sprintf('#plot img[src*="%s"]', emptyPic), 
                 source=driver, errorIfNot=F, timeout = 4)  # blank
  if (!is.logical(res) || res) {
    res <- waitFor(
      c(sprintf('#plot img:not([src*="%s"])', emptyPic), '#plot.shiny-output-error'),
      source=driver)  # normal plot or an err
  }
  res
}

is.select.el <- function(driver, selId) {
  (getEl(driver, c('#', selId)) %>% attr('data-shinyjs-resettable-type')) == "Select"
}

getSelectOptions <- function(driver, selId, withSelected=F) {
  # shiny 'select' inputs does not have their options from start. Load on click
  if (!is.select.el(driver, selId)) stop('Wrong id for select element: ', selId)
  
  selControlEl <- getEl(driver, c('select#', selId, ' + div'))
  selEl <- getEl(selControlEl, '.selectize-input')
  
  if (!grepl('\\binput-active\\b', attr(selEl, 'class'))) {
    selEl %>% click()  # makes available options visible
    waitFor('.selectize-input.input-active', selControlEl)
  }
  notSel <- selControlEl %>% getEls('.option:not(.selected)')
  if (withSelected) {
    c(notSel, selControlEl %>% getEls('.option.selected'))
  } else {
    notSel
  }
}

eraseMultiSelectOpts <- function(driver, selectId, howMany=1) {
  eraseOpts <- function(driver, selectId, n) {
    getEl(driver, c('#', selectId, ' + .selectize-control input'))$
    sendKeysToElement(as.list(rep(selKeys$backspace, n)))
  }
  getItemsLength <- function(driver, selectId) {
    length(getEls(driver, c('#', selectId, ' + .selectize-control .item')))
  }
  
  if (!is.select.el(driver, selectId)) stop('Wrong id for select element: ', selectId)
  
  nItemsBeforeErasing <- getItemsLength(driver, selectId)
  isEraseAll <- nItemsBeforeErasing == howMany
  eraseOpts(driver, selectId, max(1, howMany - 1))
  if (howMany > 1) {
    waitForPlotReady(driver)
    getSelectOptions(driver, selectId)  # sets the focus to the select element
    if (!isEraseAll) nItemsBeforeErasing <- getItemsLength(driver, selectId)
    eraseOpts(driver, selectId, 1)
  }
  
  waitFor(if (isEraseAll) sprintf('#%sCtrl .selectize-input:not(.focus)', selectId) else
            quote(nItemsBeforeErasing != getItemsLength(driver, selectId)), 
          source=driver)
}

getCurrentPlotNames <- function(driver) {
  getEls(driver, '#plotTypes option') %>% text()
}
