has_shiny_correct_state <- function(driver, plotNames, elId, elVal, 
                                    shortShotName=T, waitPlot=T) {
  if (waitPlot) waitForPlotReady(driver)
  if (!dir.exists('report')) {
    dir.create('report')
    cat('Created "report" directory at', getwd())
  }
  fileName <- sprintf('report/%s_[%s=%s].png', 
                      pastePlus(plotNames, shorten=shortShotName), toString(elId), 
                      # substr(toString(elVal), 1, 5)  # plotThemeName problem
                      toString(elVal))
  if (!is.character(fileName)) {
    driver$screenshot(T)
    browser()
  }
  driver$screenshot(file=fileName)
  !length(getEls(driver, '.shiny-output-error'))
}

waitForPlotReady <- function(driver) {
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  if (driver$getWindowSize()$height != 1080 || driver$getWindowSize()$width != 1920) {
    stop_externals('Wrong driver screen resolution')
  }
  emptyPic <- paste0(rep('A', 1e3), collapse='')
  
  isBlank <- waitFor(sprintf('#plot img[src*="%s"]', emptyPic), 
                     source=driver, errorIfNot=F, timeout = 4)
  if (!is.logical(isBlank) || isBlank) {
    waitFor(
      c(sprintf('#plot img:not([src*="%s"])', emptyPic), '#plot.shiny-output-error'),
      source=driver)  # normal plot or an err
  } else {
    isBlank
  }
}

wait_for_table_ready <- function(driver) {
  Sys.sleep(0.2)
  waitFor(paste0('.dataTables_processing[style="display: none;"]'), driver)
  # waitFor({ !isVisible(driver %>% getEl('.dataTables_processing')) }, driver)
}

isSelectEl <- function(selId, source=driver) {
  (getEl(driver, c('#', selId)) %>% attr('data-shinyjs-resettable-type')) == "Select"
}

getSelectOptions <- function(driver, selId, withActivated=F) {
  if (!isSelectEl(selId, source=driver)) stop_externals('!isSelectEl in getSelectOptions')
  
  selControlEl <- getEl(driver, c('select#', selId, ' + div'))
  selEl <- getEl(selControlEl, '.selectize-input')
  
  if (!grepl('\\binput-active\\b', attr(selEl, 'class'))) {
    # shiny 'select' inputs do not have their options at the beginning. Click to load
    selEl %>% click()  # makes available options visible
    waitFor('.selectize-input.input-active', selControlEl)
  }
  notSel <- selControlEl %>% getEls('.option:not(.selected)')
  if (withActivated) {
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
  
  if (!isSelectEl(selectId, source=driver)) {
    stop_externals('!isSelectEl in eraseMultiSelectOpts')
  }
  
  nItemsBeforeErasing <- getItemsLength(driver, selectId)
  isEraseAll <- selectId == 'plotTypes' && nItemsBeforeErasing == howMany
  eraseOpts(driver, selectId, max(1, howMany - 1))
  if (howMany > 1) {
    waitForPlotReady(driver)
    getSelectOptions(driver, selectId)  # sets the focus to the select element
    if (!isEraseAll) nItemsBeforeErasing <- getItemsLength(driver, selectId)
    eraseOpts(driver, selectId, 1)
  }
  
  if (isEraseAll) {
    waitFor(sprintf('#%sCtrl .selectize-input:not(.focus)', selectId), source=driver)
  } else {
    waitFor({ nItemsBeforeErasing != getItemsLength(driver, selectId) }, source=driver)
  }
}
