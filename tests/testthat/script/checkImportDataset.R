#### go to import tab ####
driver %>% getEl('a[data-value="importTab"]') %>% click()
rootEl <- waitFor('#fileInputSelectCtrl', driver, timeout = 3)
uplEl <- rootEl %>% getEl('input#file')


#### generate custom csv file ####
custom_dataset_filepath <- 'data/charData.csv'
if (!file.exists(custom_dataset_filepath)) {
  dir.create(dirname(custom_dataset_filepath))
  write.csv(esoph[1:50, 2:ncol(esoph)], file = custom_dataset_filepath, row.names = F)
}


#### upload custom dataset and go back to plot tab ####
uplEl$setElementAttribute('style', '')  # RSelenium's requirement
uplEl$sendKeysToElement(list(custom_dataset_filepath))
waitFor({ text(driver %>% getEl('#file_progress > .progress-bar')) == 'Upload complete' })
unlink(dirname(custom_dataset_filepath), T, T)
driver %>% getEl('#viewPlot') %>% click()
waitForPlotReady(driver)  # waitFor('li.active > a[data-value="plotTab"]')
