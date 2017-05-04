cat("\nTab: plot\n")
go_to_tab(driver, 'plot', error_if_not=F)


### facet ####
panel_name <- 'Facet'
cat(paste0("\n", panel_name))
make_select_none <- function(driver, inp_id) {
  opts <- driver %>% getSelectOptions(inp_id) %>%
    filter_el_by_text('None') %>% click(T)
}
inp_ids <- get_panel_input_ids(driver, panel_name)
stopifnot(length(inp_ids) > 0)
driver %>% expand_panel(panel_name)
# facet scale will be clicked but later report png rewritten
# with combination facetWrap+facetScale
facet_row_col_ids <- setdiff(inp_ids, 'facetWrap')
cur_opts_of_first_id <- getSelectOptions(driver, facet_row_col_ids[1]) %>% text()
if (!setequal(cur_opts_of_first_id, c('agegp', 'alcgp', 'tobgp'))) {
  debug_stop('Facet row values do not match')
}
for (inp_id in facet_row_col_ids) {
  check_input(driver, inp_id, NULL)
}
for (inp_id in facet_row_col_ids) make_select_none(driver, inp_id)
facet_wrap_ids <- setdiff(inp_ids, c('facetRow', 'facetCol'))
for (inp_id in facet_wrap_ids) check_input(driver, inp_id, NULL)
for (inp_id in facet_wrap_ids) make_select_none(driver, inp_id)


#### theme, aggregations ####
for (panel_name in c('Theme', 'Aggregation')) {
  cat(paste0("\n", panel_name))
  inp_ids <- if (panel_name == 'Aggregation') {
    c('plotAggMeth', 'plotAddAggBy')
  } else {
    get_panel_input_ids(driver, panel_name)
  }
  stopifnot(length(inp_ids) > 0)
  driver %>% expand_panel(panel_name)
  for (inp_id in inp_ids) check_input(driver, inp_id, NULL)
}


#### main aes block ####
cat(paste0("\n", 'Aesthetics'))
driver %>% getEl('#reset_input') %>% click()
Sys.sleep(10)
if (get_selected_items(driver, 'datasetName') %>% `[[`(1) %>% text != 'esoph') {
  switchToDataset(driver, 'esoph', 'scatter', need_wait_for_plot_ready = F)
}

usedPlotNames <- c()
isLastIter <- F
while (!isLastIter) {
  wait_for_plot_ready(driver)
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
