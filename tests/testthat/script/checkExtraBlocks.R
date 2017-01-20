block_name <- 'showFacet'
driver %>% isCheckboxCorrect(block_name, NULL, {
  make_select_none <- function(driver, inp_id) {
    opts <- driver %>% getSelectOptions(inp_id)
    filterElByAttr('data-value', 'None') %>% click()
    waitForPlotReady(driver)
  }
  
  facet_ids <- getEls(driver, c(
    '.widblock', 
    '> #', block_name, 'Ctrl',
    '~ * .shiny-bound-input.shinyjs-resettable')) %>% 
    attr('id')
  facet_row_col_ids <- setdiff(facet_ids, 'facetWrap')
  
  for (inp_id in facet_row_col_ids) check_input(driver, inp_id, NULL)
  for (inp_id in facet_row_col_ids) make_select_none(driver, inp_id)
  
  facet_wrap_ids <- setdiff(facet_ids, c('facetRow', 'facetCol'))
  
  for (inp_id in facet_wrap_ids) check_input(driver, inp_id, NULL)
  for (inp_id in facet_wrap_ids) make_select_none(driver, inp_id)
})


