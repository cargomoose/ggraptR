switchToDataset <- function(driver, testedDataset, init_plot = 'scatter',
                            needWaitForPlotReady=F) {
  driver %>% getSelectOptions('dataset') %>% 
    filterElByAttr('data-value', testedDataset) %>% click()
  waitAfterDatasetChanged(driver)
  
  driver %>% getSelectOptions('plotTypes') %>% 
    filterElByAttr('data-value', init_plot) %>% click()
  
  if (needWaitForPlotReady) waitForPlotReady(driver)
}

# sophisticated wait for histogram plotType and then for null plotType
waitAfterDatasetChanged <- function(driver) {
  waitRes <- waitFor('#plotTypesCtrl .item[data-value="histogram"]', driver,
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
    filterElByAttr('outerText', nextPlotType) %>% 
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
    attr('data-value')
  # workaround for a strange bug related to non-linear smooth that appears only in tests
  optVals <- setdiff(optVals, 'auto')  
  
  for (optVal in optVals) {
    getSelectOptions(driver, inpId, withActivated) %>% 
      filterElByAttr('data-value', optVal) %>% 
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
  initPos <- (ctrlEl %>% getEl(".irs-slider"))$getElementLocation()$x
  
  for (pos in c(leftPos, rightPos, initPos)) {
    dotEl <- ctrlEl %>% getEl(".irs-slider")
    moveSlider(driver, dotEl, pos)

    val <- ctrlEl %>% getEl('.irs-single') %>% text()
    if (!has_shiny_correct_state(driver, plotNames, inpId, val)) {
      warning(sprintf('Error on [%s=%s]', inpId, val))
      return(F)
    }
  }
  TRUE
}

isCheckboxCorrect <- function(driver, inpId, plotNames, block_expr=NULL) {
  is_block <- grepl('^show', inpId)
  getBox <- function() driver %>% getEl(c('#', inpId))
  for (i in 1:(1+is_block)) {
    query <- paste0('//*[@class="widblock" and .//*[@id="', inpId, '"]]',
                    '//*[contains(@class, "shiny-bound-input shinyjs-resettable")]')
    nWidBlockInps <- driver %>% getEls(query) %>% length
    chkBoxEl <- getBox()
    if (!isVisible(chkBoxEl) && is_block) return(T)  # pairs showXYRange is invisible
    
    chkBoxEl %>% click()
    if (is_block && inpId != 'showXYRange') {
      waitFor({ nWidBlockInps != length(driver %>% getEls(query)) })
    } else {
      waitForPlotReady(driver)
    }
    if (i == 1) {
      res <- has_shiny_correct_state(driver, plotNames, inpId,
                                     unlist(getBox()$isElementSelected()), waitPlot=F)
      if (!res) return(FALSE)
      if (!is.null(substitute(block_expr))) {
        stopifnot(is_block)
        eval(substitute(block_expr))
      }
    }
  }
  TRUE
}
