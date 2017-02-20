switchToDataset <- function(driver, dataset, init_plot = 'scatter',
                            need_wait_for_plot_ready=F) {
  
  stopifnot(!is.null(dataset) && 
              dataset != attr(getEl(driver, '#dataset > option'), 'value'))
  driver %>% getSelectOptions('dataset') %>% 
    filter_el_by_attr('data-value', dataset) %>% click()
  waitAfterDatasetChanged(driver)
  
  if (!is.null(init_plot)) {
    driver %>% getSelectOptions('plotTypes') %>% 
      filter_el_by_attr('data-value', init_plot) %>% click()
    if (need_wait_for_plot_ready) waitForPlotReady(driver)
  }
}

# sophisticated wait for histogram plotType and then for null plotType
waitAfterDatasetChanged <- function(driver) {
  # alternative: #showAes[data-shinyjs-resettable-value="false"]
  waitRes <- waitFor('#plotTypesCtrl .selectize-input:not("has-items")', driver,
                     timeout=5, errorIfNot = F)
  if (isWebElement(waitRes)) {
    if (!waitFor({
          length(getAllPlotNames()) == length(driver %>% getSelectOptions('plotTypes')) 
        }, errorIfNot = F, catchStale=T)) {
      browser()
      stop_externals('waitAfterDatasetChanged failed')
    }
  }
}
  
go_to_tab <- function(driver, tab_name) {
  cur_tab <- driver %>% getEl('#conditionedPanels > li.active') %>% 
    text() %>% tolower() %>% trimws()
  if (!tab_name %in% setdiff(c('plot', 'table', 'log'), cur_tab)) {
    stop_externals(sprintf('It is impossible to click on tab [%s]', tab_name))
  }
  
  driver %>% getEl(c('a[data-value="', tab_name, 'Tab"]')) %>% click()
  
  if (tab_name == 'plot') {
    waitFor({ isVisible(driver %>% getEl('#plot')) }, driver, timeout = 3)
  } else if (tab_name == 'table') {
    waitFor({ isVisible(driver %>% getEl('#displayTable')) }, driver, timeout = 3)
    wait_for_table_ready(driver)
  } else {
    waitFor({ isVisible(driver %>% getEl('#plotLog')) }, driver, timeout = 3)
  }
}

tryAddNextPlot <- function(driver) {
  plot_types_id <- 'plotTypes'
  plotTypeGroupRestOpts <- function() getSelectOptions(driver, plot_types_id)
  canAdd <- length(plotTypeGroupRestOpts()) > 0
  if (canAdd) {
    cur_plots <- get_current_plot_names(driver)
    if (length(cur_plots) == 2) {
      eraseMultiSelectOpts(driver, plot_types_id, howMany = 2)
    }
    sample(plotTypeGroupRestOpts(), size=1)[[1]] %>% click()
  }
  canAdd
}

startNewPlotGroup <- function(driver, nextPlotType) {
  getSelectOptions(driver, 'plotTypes') %>% 
    filter_el_by_attr('outerText', nextPlotType) %>% 
    click()
}

get_plot_input_ids <- function(driver) {
  inputIds <- getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')
}

get_current_plot_names <- function(driver) {
  getEls(driver, '#plotTypes option') %>% text()
}

get_widblock_input_ids <- function(driver, block_name) {
  getEls(driver, c(
    '.widblock',
    '> #', block_name, 'Ctrl',
    '~ * .shiny-bound-input.shinyjs-resettable')) %>% 
    Filter(function(el) attr(el, 'data-shinyjs-resettable-type') %in% 
             c('Select', 'Slider', 'Checkbox'), .) %>% 
    attr('id')
}

check_input <- function(driver, inp_id, plot_names) {
  inp_type <- driver %>% getEl(c('#', inp_id)) %>% attr('data-shinyjs-resettable-type')
  test_that(sprintf('[%s] [%s] works correct', pastePlus(plot_names), inp_id), {
    if (is.null(inp_type)) {
      skip(pastePlus(plot_names, inp_id, '[is hidden now]', shorten = F))
    } else {
      expect_true(do.call(paste0('is', inp_type, 'Correct'), 
                          list(driver, inp_id, plot_names)))
    } 
  })
}

isSelectCorrect <- function(driver, inpId, plotNames) {
  withActivated <- grepl('^pairs', inpId)
  optVals <- getSelectOptions(driver, inpId, withActivated) %>%
    attr('data-value') %>% 
    # workaround for a strange bug with non-linear smoothing that appears only in tests
    setdiff('auto')
  
  for (optVal in optVals) {
    getSelectOptions(driver, inpId, withActivated) %>% 
      filter_el_by_attr('data-value', optVal) %>% 
      click()
    
    if (!has_shiny_correct_state(driver, plotNames, inpId, optVal)) {
      warning(sprintf('Error on [%s=%s]', inpId, optVal))
      return(FALSE)
    }
    if (!is.null(driver %>% getEl(c('#', inpId)) %>% attr('multiple'))) {
      eraseMultiSelectOpts(driver, inpId)
      waitForPlotReady(driver)
    }
  }
  
  TRUE
}

isSliderCorrect <- function(driver, inpId, plotNames) {
  ctrlEl <- getEl(driver, c("#", inpId, "Ctrl"))
  leftPos <- getEl(ctrlEl, ".irs-line-mid")$getElementLocation()$x
  rightPos <- getEl(ctrlEl, ".irs-line-right")$getElementLocation()$x
  
  get_sliders <- function() ctrlEl %>% getEls(".irs-slider")
  initPositions <- sapply(get_sliders(), function(slider) slider$getElementLocation()$x)
  if (length(initPositions) > 1 && !inpId %in% c('xlim', 'ylim')) {
    warning('May contain a bug when to- and from- sliders will try to change their order')
  }
  
  for (sl in 1:length(initPositions)) {
    for (pos in c(leftPos, rightPos, initPositions[sl])) {
      moveSlider(driver, get_sliders()[[sl]], pos)
  
      val <- ctrlEl %>% getEl(c('.irs-from', '.irs-to')[sl]) %>% text()
      if (!has_shiny_correct_state(driver, plotNames, inpId, val)) {
        warning(sprintf('Error on [%s=%s]', inpId, val))
        return(F)
      }
    }
  }
  
  TRUE
}

isCheckboxCorrect <- function(driver, inpId, plotNames, 
                              eval_when_active=NULL) {
  is_section <- grepl('^show', inpId)
  getBox <- function() driver %>% getEl(c('#', inpId))
  for (i in 1:(1+is_section)) {
    n_inputs_query <- paste0('//*[@class="widblock" and .//*[@id="', inpId, '"]]',
                    '//*[contains(@class, "shiny-bound-input shinyjs-resettable")]')
    n_inputs <- driver %>% getEls(n_inputs_query) %>% length
    
    chkBoxEl <- getBox()
    if (!isVisible(chkBoxEl) && is_section) return(T)  # pairs showXYRange is invisible
    # if (is_section && inpId != 'showXYRange') browser()
    chkBoxEl %>% click()
    
    if (is_section && inpId != 'showXYRange') {
      waitFor({ n_inputs != length(driver %>% getEls(n_inputs_query)) })
    } else {
      waitForPlotReady(driver)
    }
    if (i == 1) {
      res <- has_shiny_correct_state(driver, plotNames, inpId,
                                     unlist(getBox()$isElementSelected()), waitPlot=F)
      if (!res) return(FALSE)
      if (!is.null(substitute(eval_when_active))) {
        stopifnot(is_section)
        eval(substitute(eval_when_active))
      }
    }
  }
  TRUE
}
