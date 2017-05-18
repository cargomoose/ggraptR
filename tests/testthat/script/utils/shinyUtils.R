has_shiny_correct_state <- function(driver, plotNames, elId, elVal, 
                                    shortShotName=T, waitPlot=T) {
  if (waitPlot) wait_for_plot_ready(driver)
  report_path <- paste0(Sys.getenv('R_USER'), '/report')
  if (!dir.exists(report_path)) {
    dir.create(report_path)
    cat(' >>Created "report" directory at', report_path, ' ')
  }
  fileName <- sprintf('%s/%s_[%s=%s].png', 
                      report_path, 
                      pastePlus(plotNames, shorten=shortShotName), 
                      toString(elId), 
                      # substr(toString(elVal), 1, 5)  # plotThemeName problem
                      toString(elVal))
  if (!is.character(fileName)) {
    debug_stop('fileName for plot is not character')
  }
  
  res <- !length(getEls(driver, '.shiny-output-error'))
  if (!res) {
    driver$screenshot(T)
    fileName <- paste0('!ERROR_', fileName)
  }
  driver$screenshot(file=fileName)
  res
}

# wait_for may be unsafe in case you need to wait for a long loading element in a tab
switch_tab <- function(driver, waited_tab_name) {
  driver %>% getEl(sprintf('a[data-value="%s"]', waited_tab_name)) %>% click()
  wait_for(sprintf('.tab-pane.active[data-value="%s"]', waited_tab_name), driver)
}

isSelectEl <- function(selId, source=driver) {
  (getEl(driver, c('#', selId)) %>% attr('data-shinyjs-resettable-type')) == "Select"
}

getSelectOptions <- function(driver, selId, withActivated=F) {
  if (!isSelectEl(selId, source=driver)) stop_externals('!isSelectEl in getSelectOptions')
  
  selControlEl <- driver %>% getEl(c('select#', selId, ' + div'))
  selEl <- selControlEl %>% getEl('.selectize-input')
  
  if (!grepl('\\binput-active\\b', attr(selEl, 'class'))) {
    # shiny 'select' inputs do not have their options at the beginning. Click to load
    selEl %>% click()  # makes available options visible
    wait_for('.selectize-input.input-active', selControlEl, catchStale = T)
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
    el <- getEl(driver, c('#', selectId, ' + .selectize-control input'))
    el$sendKeysToElement(as.list(rep(selKeys$backspace, n)))
  }
  getItemsLength <- function(driver, selectId) {
    length(get_selected_items(driver, selectId))
  }
  
  if (!isSelectEl(selectId, source=driver)) {
    stop_externals('!isSelectEl in eraseMultiSelectOpts')
  }
  
  nItemsBeforeErasing <- getItemsLength(driver, selectId)
  if (howMany == 'all') howMany <- nItemsBeforeErasing
  isEraseAll <- selectId == 'plotTypes' && nItemsBeforeErasing == howMany
  eraseOpts(driver, selectId, max(1, howMany - 1))
  if (howMany > 1) {
    wait_for_plot_ready(driver)
    getSelectOptions(driver, selectId)  # sets the focus to the select element
    if (!isEraseAll) nItemsBeforeErasing <- getItemsLength(driver, selectId)
    eraseOpts(driver, selectId, 1)
  }
  
  if (isEraseAll) {
    wait_for(sprintf('#%sCtrl .selectize-input:not(.focus)', selectId), source=driver)
  } else {
    wait_for({ nItemsBeforeErasing != getItemsLength(driver, selectId) }, source=driver)
  }
}
