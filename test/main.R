# Run this script with 'testthat::test_file(paste0(getwd(), '/test/main.R'))'
# You can monitor the progress by the names of the screenshots in test/report
# issues and new features https://github.com/cargomoose/ggraptR/issues/61

source('script/commonBlock/checkInitPlot.R')


#### check reactive checkbox + submit button and reset button ####
driver %>% getEl('#reactive') %>% click()
submitBtn <- waitFor("#submit:not(disabled)", driver)

# > check for treatColorAsFactor
test_that('treatColorAsFactor hides correct', 
          expect_true(is.null(driver %>% getEl('#treatColorAsFactor'))))
caratColorOpt <- driver %>% getSelectOptions('color') %>% 
  Filter(function(x) text(x) == 'carat', .)
if (length(caratColorOpt) != 1) stop() else caratColorOpt[[1]] %>% click()
test_that('treatColorAsFactor appears correct', 
          expect_true(!is.null(waitFor('#treatColorAsFactor'))))
driver %>% getEl('#treatColorAsFactor') %>% click()
# <

test_that('Unreactive submit waits correct', 
          expect_false(waitForPlotReady(driver)))
driver %>% getEl('#submit') %>% click()
test_that('Unreactive submit works correct', 
          expect_true(driver %>% has_shiny_correct_state(
            pastePlus('^unreactive', 'colorFactors', shorten=F), NULL, NULL, 
            shortShotName=F)))

driver %>% getEl('#reactive') %>% click()  # turn reactive on
test_that('Submit button grays correct', {
  expect_true(!is.null(waitFor('#submit[disabled]')))
  driver %>% getEl('#reactive') %>% click()  # turn reactive off to check Reset inputs
  expect_true(!is.null(waitFor('#submit:not(disabled)')))
})

driver %>% getEl('#reset_input') %>% click()
test_that('Reset works correct', {
  expect_true(waitFor(quote(
    text(driver %>% getEl('#color option[selected="selected"]')) == 'color')))
  expect_true(waitFor(quote(is.null(driver %>% getEl('#treatColorAsFactor')))))
  expect_true(!is.null(waitFor('#submit[disabled]')))
  expect_true(has_shiny_correct_state(driver, '^reset', NULL, NULL, shortShotName=F))
})


#### check Export plot and Generate Plot Code
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


#### check inputs ####
switchToDataset(driver, 'esoph')
source('script/commonBlock/checkInputs.R')
stopExternals(driver, selServer)
