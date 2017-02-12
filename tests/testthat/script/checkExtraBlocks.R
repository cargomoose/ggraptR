cat("\nFacets, XY limits and other extra blocks")

block_name <- 'showFacet'
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
  driver %>% isCheckboxCorrect(inpId = block_name, plotNames = NULL, eval_when_active = {
    inp_ids <- if (block_name == 'showDSTypeAndPlotAgg') {
      c('plotAggMeth', 'plotAddAggBy')
    } else {
      get_widblock_input_ids(driver, block_name)
    }
    for (inp_id in inp_ids) check_input(driver, inp_id, NULL)
  })
}
