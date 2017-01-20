# You can monitor the progress by screenshot names in tests/testthat/report/ folder
context("Plot tab")

source('script/checkInitPlot.R')
# source('script/extraBlocks.R')
source('script/checkImportDataset.R')
source('script/checkReactiveSubmitReset.R')
source('script/checkExportPlotGenerateCode.R')

test_settings <<- list(only_pairs = F, 
                       dataset = sample(c(basename(customDatasetFilepath), 'esoph'), 1))
cat('\n  [tested dataset: ', test_settings$dataset, ']\n  ', sep = '')
source('script/checkAes.R')
release_externals()
