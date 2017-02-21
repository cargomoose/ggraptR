cat("\nTab: plot\n")
go_to_tab(driver, 'plot')


block_name <- 'showFacet'
cat(paste0("\n", block_name))
driver %>% isCheckboxCorrect(inpId = block_name, plotNames = NULL, eval_when_active = {
  facet_ids <- get_widblock_input_ids(driver, block_name)
  
  make_select_none <- function(driver, inp_id) {
    opts <- driver %>% getSelectOptions(inp_id) %>%
      filter_el_by_text('None') %>% click()
    waitForPlotReady(driver)
  }
  
  # facet scale will be clicked but later report png rewritten
  # with combination facetWrap+facetScale
  facet_row_col_ids <- setdiff(facet_ids, 'facetWrap')
  for (inp_id in facet_row_col_ids) check_input(driver, inp_id, NULL)
  for (inp_id in facet_row_col_ids) make_select_none(driver, inp_id)
  
  facet_wrap_ids <- setdiff(facet_ids, c('facetRow', 'facetCol'))
  for (inp_id in facet_wrap_ids) check_input(driver, inp_id, NULL)
  for (inp_id in facet_wrap_ids) make_select_none(driver, inp_id)
})


for (block_name in c('showXYRange', 'showTheme', 'showDSTypeAndPlotAgg')) {
  cat(paste0("\n", block_name))
  driver %>% isCheckboxCorrect(inpId = block_name, plotNames = NULL, eval_when_active = {
    inp_ids <- if (block_name == 'showDSTypeAndPlotAgg') {
      c('plotAggMeth', 'plotAddAggBy')
    } else {
      get_widblock_input_ids(driver, block_name)
    }
    for (inp_id in inp_ids) check_input(driver, inp_id, NULL)
  })
}


block_name <- 'showAes'
cat(paste0("\n", block_name))
usedPlotNames <- c()
isLastIter <- F
while (!isLastIter) {
  waitForPlotReady(driver)
  plot_names <- get_current_plot_names(driver)
  
  test_that(sprintf('[%s] [default_inputs] work correct', pastePlus(plot_names)), 
            expect_true(has_shiny_correct_state(driver, plot_names,
                                                NULL, NULL, waitPlot=F)))
  
  for (inp_id in get_plot_input_ids(driver)) check_input(driver, inp_id, plot_names)
  
  usedPlotNames <- append(usedPlotNames, setdiff(plot_names, usedPlotNames))
  isNextPlotAdded <- tryAddNextPlot(driver)
  
  if (!isNextPlotAdded) {
    nextPlotTypes <- setdiff(getAllPlotNames(), usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plot_names))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}
