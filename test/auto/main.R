# Run this script with 'testthat::test_file(paste0(getwd(), '/test/auto/main.R'))'
# You can monitor the progress by the names of the screenshots in test/auto/report
# issues and new features https://github.com/cargomoose/ggraptR/issues/61

source('checkInitPlot.R')

#### check Export plot and Generate Plot Code
foo <- function(driver) {
invisible(apply(
  data.frame(modalBt=c('#exportPlot', '#generatePlotCode'),
             modalRoot=c('#modalExportOptions', '#modalCodeView'),
             dwnload=c('a#dlPlot', NA)), 1, 
  function(row) {
    driver %>% getEl(row['modalBt']) %>% click()
    root <- waitFor(paste0(row['modalRoot'], '[style="display: block;"]'), driver)
    
    test_that(paste(row['modalBt'], 'works fine'), {
      if (!is.na(row['dwnload'])) {
        expect_true(!is.null(waitFor('a#dlPlot', root)))
      } else {
        waitFor(quote(text(root %>% getEl('#generateCode')) != ''))
        expect_true(text(root %>% getEl('#generateCode')) == gsub(
          '\\n *', '', 'ggplot(diamonds, aes(y=price, x=carat)) + 
          geom_point(aes(colour=color), stat=\"identity\", position=\"jitter\", 
          alpha=0.5, size=3) + theme_grey() + theme(text=element_text(family=\"sans\", 
          face=\"plain\", color=\"#000000\", size=15, hjust=0.5, vjust=0.5)) + 
          scale_size(range=c(1, 3)) + xlab(\"carat\") + ylab(\"price\")'))
      }
    })
    root %>% getEl('button.close') %>% click()
    waitFor(paste0(row['modalRoot'], '[style="display: none;"]'), driver)
  }
))
}
foo(driver)

#### switches to light esoph dataset ####
datasetEls <- driver %>% getSelectOptions('dataset')
datasetEls %>% filterElByAttr('data-value', 'esoph') %>% click()

#### sophisticated wait for historgram plotType and then for null plotType ####
allPlotNames <- getAllPlotNames()
waitRes <- waitFor('#plotTypesCtrl .item[data-value="histogram"]', driver,
                   timeout=5, errorIfNot = F)
if (isWebElement(waitRes)) {
  if (!waitFor(quote(
      length(allPlotNames) == length(driver %>% getSelectOptions('plotTypes'))), 
      errorIfNot = F, catchStale=T)) {
    browser()
    stop()
  }
}

#### pick esoph's Scatter ####
getSelectOptions(driver, 'plotTypes')[[1]] %>% click()


#### test inputs ####
usedPlotNames <- if (exists('shortTestMode') && shortTestMode)
  setdiff(allPlotNames, 'Pairs') else c()
isLastIter <- F
while (!isLastIter) {
  waitForPlotReady(driver)
  plotNames <- getCurrentPlotNames(driver)
  
  test_that(sprintf('[%s] [default_inputs] work correct', pastePlus(plotNames)), 
            expect_true(has_shiny_correct_state(driver, plotNames,
                                                NULL, NULL, waitPlot=F)))
  
  for (inpId in getPlotInputIds(driver)) {
    inpType <- driver %>% getEl(c('#', inpId)) %>% attr('data-shinyjs-resettable-type')
    if (is.null(inpType)) {
      cat(pastePlus(plotNames), inpId, '[is hidden now, skipped]')
      next
    }
    
    test_that(sprintf('[%s] [%s] works correct', pastePlus(plotNames), inpId),
              expect_true(do.call(paste0('is', inpType, 'Correct'), 
                                  list(driver, inpId, plotNames))))
  }
  
  isNextPlotAdded <- tryAddNextPlot(driver)
  if (!isNextPlotAdded) {
    usedPlotNames <- append(usedPlotNames, plotNames)
    
    nextPlotTypes <- setdiff(allPlotNames, usedPlotNames)
    if (length(nextPlotTypes)) {
      eraseMultiSelectOpts(driver, 'plotTypes', length(plotNames))
      startNewPlotGroup(driver, sample(nextPlotTypes, size=1))
    } else {
      isLastIter <- T
    }
  }
}

stopExternals(driver, selServer)
