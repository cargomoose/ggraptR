# You can monitor the progress by screenshot names in tests/testthat/report/ folder

source('script/checkInitPlot.R')
source('script/checkReactiveSubmitReset.R')  # checks diamonds color as well
source('script/checkDownloadPlotModal.R')
source('script/checkAddDataset.R')
# switchToDataset(driver, 'esoph', init_plot = 'scatter')

source('script/checkTabCode.R')
source('script/checkTabTable.R')
source('script/checkTabPlot.R')

cat("\nRelease externals")
release_externals()
