cat("\nTab: table")
go_to_tab(driver, 'Table')

get_table <- function() driver %>% getEl('#displayTable')
getSelectOptions(driver, 'aggBy')[[1]] %>% click()
wait_for_table_ready(driver)
getSelectOptions(driver, 'aggTarget')[[1]] %>% click()
wait_for_table_ready(driver)

inpId <- 'aggMeth'
get_meth_opts <- function() getSelectOptions(driver, inpId, T)
for (optVal in attr(get_meth_opts(), 'data-value')) {
  get_meth_opts() %>% filter_el_by_attr('data-value', optVal) %>% click()
  wait_for_table_ready(driver)
}
driver$screenshot(file='report/table.png')

aggreg_res_serial <- 
  paste0('Showentriesagegpncases_meanncases_sumncases_minncases_maxcountncases_',
         'median125340071011502354406903150345542884606163455644757629164565743675501',
         '71536751181302111Showing1to6of6entriesPrevious1Next')
test_that('Table aggregations work correct', {
  expect_equal(get_table() %>% text() %>% gsub('\\W', '', .), aggreg_res_serial)
})

csv_url <- paste0(driver %>% getEl('#dlCSV') %>% attr('href'))
csv_lines <- readLines(csv_url)
csv_lines <- sapply(1:length(csv_lines),  # add row numbers as browser table has
                    function(i) if (i > 1) paste(i-1, csv_lines[i]) else csv_lines[i])
test_that('Download button works correct', { 
  expect_true(grepl(gsub('\\W', '', csv_lines %>% paste(collapse='')), 
                    aggreg_res_serial)) 
  }
)
