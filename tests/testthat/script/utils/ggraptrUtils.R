checked_types <- c('Select', 'Slider', 'Checkbox')
  
switchToDataset <- function(driver, dataset, init_plot = NULL,
                            need_wait_for_plot_ready = T) {
  
  stopifnot(!is.null(dataset) && 
              dataset != attr(getEl(driver, '#datasetName > option'), 'value'))
  
  n_plot_types <- length(get_selected_items(driver, 'plotTypes'))
  driver %>% getSelectOptions('datasetName') %>% 
    filter_el_by_attr('data-value', dataset) %>% click()
  wait_for({ length(get_current_plot_names(driver)) != n_plot_types }, driver, 
           catchStale = T)
  # wait_for(  # alternative: #showAes[data-shinyjs-resettable-value="false"]
  # sprintf('#plotTypesCtrl .selectize-input%s',
  #         if (n_plot_types) ':not(.has-items)' else '.has-items'), driver, catchStale=T)
  
  if (!is.null(init_plot) && !setequal(init_plot, get_current_plot_names(driver))) {
    if (length(get_current_plot_names(driver)) > 0) {
      eraseMultiSelectOpts(driver, 'plotTypes', 'all')
    }
    
    driver %>% 
      getSelectOptions('plotTypes') %>% 
      filter_el_by_attr('data-value', init_plot) %>% 
      click()
    if (need_wait_for_plot_ready) wait_for_plot_ready(driver)
  }
}

go_to_tab <- function(driver, tab_name, error_if_not=T) {
  cur_tab <- driver %>% getEl('#conditionedPanels > li.active') %>% 
    text() %>% tolower() %>% trimws()
  all_tabs <- driver %>% getEls('#conditionedPanels li') %>% text %>% tolower %>% trimws
  tab_name <- tab_name %>% tolower()
  if (!tab_name %in% setdiff(all_tabs, cur_tab)) {
    if (error_if_not)  {
      stop_externals(sprintf('It is impossible to click on tab [%s]', tab_name))
    } else {
      return()
    }
  }
  
  driver %>% getEl(c('a[data-value="', tab_name, 'Tab"]')) %>% click()
  
  if (tab_name == 'plot') {
    wait_for({ isVisible(driver %>% getEl('#plot')) }, driver, timeout = 3)
    # Sys.sleep(4)
  } else if (tab_name == 'table') {
    wait_for({ isVisible(driver %>% getEl('#displayTable')) }, driver, timeout = 3)
    wait_for_table_ready(driver)
  } else {
    wait_for({ isVisible(driver %>% getEl('#plotLog')) }, driver, timeout = 3)
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
  getEls(driver, c(
    'form', 
    '> div[data-display-if="input.conditionedPanels == \\"plotTab\\""]',
    ' .shiny-bound-input.shinyjs-resettable')) %>% 
    Filter(function(el) attr(el,'data-shinyjs-resettable-type') %in% checked_types,.) %>% 
    attr('id')
}

get_selected_items <- function(driver, inpId) {
  driver %>% getEls(c('#', inpId, 'Ctrl .selectize-control .item'))
}

get_possible_options <- function(driver, inpId) {
  driver %>% getEls(c('#', inpId, 'Ctrl .selectize-control .option'))
}

get_current_plot_names <- function(driver) {
  driver %>% get_selected_items('plotTypes') %>% text()
}

get_current_dataset_name <- function(driver) {
  driver %>% get_selected_items('datasetName') %>% text()
}

get_panel <- function(driver, panel_name) {
  driver %>% getEl(c('.panel[value="', panel_name, '"]'))
}

get_panel_inputs <- function(driver, panel_name) {
  driver %>% 
    getEls(c('.panel[value="',panel_name,'"] .shiny-bound-input.shinyjs-resettable')) %>% 
    Filter(function(el) attr(el, 'data-shinyjs-resettable-type') %in% checked_types, .)
}

expand_panel <- function(driver, panel, collapse = FALSE) {
  if (is.character(panel)) {
    panel <- get_panel(driver, panel)
  }
  link <- panel %>% getEl('a[data-toggle="collapse"]')
  if (!as.logical(link %>% attr('aria-expanded')) || collapse) link %>% click()
  if (!collapse) wait_for('a[aria-expanded="true"]', panel)  # acts like assert
  Sys.sleep(3)
}

pick_select_value <- function(driver, select_name, select_value, 
                              withActivated=F, sleep=0) {
  getSelectOptions(driver, select_name, withActivated) %>% 
    filter_el_by_attr('data-value', select_value) %>% 
    click()
  Sys.sleep(sleep)
}

check_input <- function(driver, inp_id, plot_names) {
  inp_type <- driver %>% getEl(c('#', inp_id)) %>% attr('data-shinyjs-resettable-type')
  test_that(sprintf('[%s] [%s] works correct', pastePlus(plot_names), inp_id), {
    if (is.null(inp_type)) {
      skip(pastePlus(plot_names, inp_id, '[is hidden now]', shorten = F))
    } else if (!inp_type %in% checked_types) {
      skip(pastePlus(plot_names, inp_id, '[is not supported type]', shorten = F))
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
    pick_select_value(driver, inpId, optVal, withActivated)
    
    if (!has_shiny_correct_state(driver, plotNames, inpId, optVal)) {
      warning(sprintf('Error on [%s=%s]', inpId, optVal))
      return(FALSE)
    }
    if (!is.null(driver %>% getEl(c('#', inpId)) %>% attr('multiple'))) {
      eraseAll <- length(get_selected_items(driver, inpId)) == 1
      eraseMultiSelectOpts(driver, inpId)
      wait_for_plot_ready(driver, !eraseAll)
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
  if (length(initPositions) > 1) {
    warning('May contain a bug when to- and from- sliders will try to change their order')
  }
  
  for (sl in 1:length(initPositions)) {
    for (pos in c(leftPos, rightPos, initPositions[sl])) {
      moveSlider(driver, get_sliders()[[sl]], pos)
  
      val <- ctrlEl %>% getEl(c('.irs-single')[sl]) %>% text()
      if (!has_shiny_correct_state(driver, plotNames, inpId, val)) {
        warning(sprintf('Error on [%s=%s]', inpId, val))
        return(F)
      }
    }
  }
  
  TRUE
}

isCheckboxCorrect <- function(driver, inpId, plotNames, eval_when_active=NULL) {
  getBox <- function() driver %>% getEl(c('#', inpId))
  getBox() %>% click()
  wait_for_plot_ready(driver)
  has_shiny_correct_state(
    driver, plotNames, inpId, unlist(getBox()$isElementSelected()), waitPlot=F)
}
