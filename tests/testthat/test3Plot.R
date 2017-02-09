# You can monitor the progress by screenshot names in tests/testthat/report/ folder
context("Plot tab")

source('script/checkInitPlot.R')
source('script/checkImportDataset.R')

test_settings <<- list(only_pairs = F, 
                       dataset = sample(c(basename(custom_dataset_filepath), 'esoph'), 1))
cat('\n  [tested dataset: ', test_settings$dataset, ']\n  ', sep = '')
switchToDataset(driver, test_settings$dataset, 
                init_plot = if (test_settings$only_pairs) 'pairs' else 'scatter')

source('script/checkReactiveSubmitReset.R')
source('script/checkExportPlotGenerateCode.R')
source('script/checkExtraBlocks.R')

source('script/checkAes.R')
release_externals()
