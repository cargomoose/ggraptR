cat("\nAdd dataset")


#### file uploading ####
custom_dataset_filepath <- 'esoph_short.csv'
write.csv(esoph[1:7, 2:ncol(esoph)], file = custom_dataset_filepath, row.names = F)

driver %>% getEl('#uploadData') %>% click()
# uplEl <- modalEl %>% getEl('input#file')
uplEl <- wait_for('input#file', driver)
uplEl$setElementAttribute('style', '')  # RSelenium's requirement
uplEl$sendKeysToElement(list(custom_dataset_filepath))
wait_for({length(driver %>% get_current_plot_names()) == 0}, catchStale = T)
test_that("File uploading works correct", {
  expect_equal(basename(custom_dataset_filepath), get_current_dataset_name(driver))
})

file.remove(custom_dataset_filepath)


#### database uploading ####
driver %>% getEl('#uploadData') %>% click()
modalEl <- wait_for('#modalUploadOptions[style="display: block;"]', driver)
switch_tab(driver, 'addDbTab')
data_name <- driver %>% getEl('#dbSqlQuery') %>% attr('value') %>% 
  stringr::str_extract('(?i)(?<=from )\\S+')
driver %>% getEl('button#dbExecuteBtn') %>% click(T)
test_that("Database uploading works correct", {
  expect_equal(data_name, get_current_dataset_name(driver))
  has_shiny_correct_state(driver, 'uploading', 'database', data_name, 
                          shortShotName=F, waitPlot=F)
})

 
# # random dataset
# rnd_df_name <- sample(c(basename(custom_dataset_filepath), 'esoph'), 1)
# cat(' [randomly choosed tested dataset: ', rnd_df_name, ']', sep = '')
