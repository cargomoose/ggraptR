cat("\nAdd dataset")


#### file uploading ####
custom_dataset_filepath <- 'esoph_problem.csv'
write.csv(esoph[1:4], file = custom_dataset_filepath, row.names = F)
upload_file(driver, custom_dataset_filepath)
test_that("File uploading works correct", {
  expect_equal(basename(custom_dataset_filepath), get_current_dataset_name(driver))
})
n_current_plots <- driver %>% get_current_plot_names()
if (length(n_current_plots)) eraseMultiSelectOpts(driver, 'plotTypes', n_current_plots)
test_that("Correct reaction on dataset with n_num == 1", {
  expect_true(!length(setdiff(getSelectOptions(driver, 'plotTypes') %>% text(),
                             getAllPlotNames(n_num=1))))
})
file.remove(custom_dataset_filepath)
# write.csv(esoph[1:3], file = custom_dataset_filepath, row.names = F)
# upload_file(driver, custom_dataset_filepath)
# test_that("File uploading works correct", {
#   expect_equal(basename(custom_dataset_filepath), get_current_dataset_name(driver))
# })
# test_that("Correct reaction on dataset with n_num == 0", {
#   expect_true(!is.null(wait_for('#pTypesWarnBtn')))
#   expect_true(!is.null(wait_for('#plotTypesCtrl .selectize-input:not(.has-items)')))
# })
# file.remove(custom_dataset_filepath)


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
