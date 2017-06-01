cat("\nTab: code")
go_to_tab(driver, 'Code')
test_target <- 'Plot log'
log <- driver %>% getEl('.tab-pane.active[data-value="codeTab"]') %>% 
  text() %>% strsplit('\n') %>% `[[`(1)
color_as_factor_code <- log[grepl('colour\\=as\\.factor', log)]
expected_code <- paste0(
  'ggplot(diamonds, aes(y=price, x=carat)) ',
  '+ geom_point(aes(colour=as.factor(carat)), stat=\"identity\", position=\"jitter\", ',
  'alpha=0.5, size=3) + theme_grey() + theme(text=element_text(family=\"sans\", ',
  'face=\"plain\", color=\"#000000\", size=15, hjust=0.5, vjust=0.5)) + ',
  'scale_size(range=c(1, 3)) + guides(colour=guide_legend(title=\"carat\")) + ',
  'xlab(\"carat\") + ylab(\"price\")')
test_that(paste(test_target, 'works correct'), {
  expect_equal(color_as_factor_code, expected_code)
})


test_target <- 'Console'
console_input_el <- driver %>% getEl('input#console')
console_cmd <- 'ls(1)'
console_input_el$sendKeysToElement(list(console_cmd))
wait_for({ driver %>% getEl('input#console') %>% attr('value') == console_cmd }, driver)
driver %>% getEl('button#evalConsoleBtn') %>% click()
test_that(paste(test_target, 'works correct'), {
  expect_true(
    wait_for({ driver %>% getEl('#consoleCtrl') %>% text %>% nchar > 0 }, driver))
  expect_true(!grepl('shiny-output-error', 
                     driver %>% getEl('#consoleCtrl') %>% attr('class')))
})

# every click on evalConsoleBtn will result changing current dataset on uploaded
go_to_tab(driver, 'Plot')
switchToDataset(driver, 'esoph', init_plot = 'scatter')
