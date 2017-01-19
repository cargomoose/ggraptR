#### go to import tab ####
driver %>% getEl('a[data-value="importTab"]') %>% click()
rootEl <- waitFor('#fileInputSelectCtrl', driver, timeout = 3)
uplEl <- rootEl %>% getEl('input#file')


#### generate custom csv file ####
customDatasetFilepath <- 'data/charData.csv'
if (!file.exists(customDatasetFilepath)) {
  dir.create(dirname(customDatasetFilepath))
  write.csv(esoph[1:50, 2:ncol(esoph)], file = customDatasetFilepath, row.names = F)
}


#### upload custom dataset and go back to plot tab ####
uplEl$setElementAttribute('style', '')  # RSelenium's requirement
uplEl$sendKeysToElement(list(customDatasetFilepath))
waitFor({ text(driver %>% getEl('#file_progress > .progress-bar')) == 'Upload complete' })
unlink(dirname(customDatasetFilepath), T, T)
driver %>% getEl('#viewPlot') %>% click()
waitForPlotReady(driver)  # waitFor('li.active > a[data-value="plotTab"]')
