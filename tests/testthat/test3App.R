# You can monitor the progress by screenshot names in tests/testthat/report/ folder

source('script/checkInitPlot.R')
source('script/checkReactiveSubmitReset.R')  # checks diamonds color as well
source('script/checkExportPlotGenerateCode.R')  # requires diamonds for genCode
source('script/checkImportDataset.R')
source('script/checkTable.R')

source('script/checkExtraBlocks.R')
source('script/checkAes.R')

cat("\nRelease externals")
release_externals()
