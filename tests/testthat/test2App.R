# You can monitor the progress by screenshots in paste0(Sys.getenv('R_USER'), '/report')

source('script/checkInitPlot.R')
source('script/checkReactiveSubmitReset.R')  # checks diamonds color as well
source('script/checkDownloadPlotModal.R')
source('script/checkAddDataset.R')  # switchToDataset(driver,'esoph',init_plot='scatter')

source('script/checkTabCode.R')
source('script/checkTabTable.R')
source('script/checkTabPlot.R')

cat("\nRelease externals")
release_externals()
