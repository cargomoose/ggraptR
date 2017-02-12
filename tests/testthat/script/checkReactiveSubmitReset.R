cat("\nReactive submit, reset and missing colorAsFactor")

driver %>% getEl('#reactive') %>% click()
submitBtn <- waitFor("#submit:not(disabled)", driver)

# > check for treatColorAsFactor
test_that('treatColorAsFactor hides correct', 
          expect_true(is.null(driver %>% getEl('#treatColorAsFactor'))))
caratColorOpt <- driver %>% getSelectOptions('color') %>% 
  Filter(function(x) text(x) == 'carat', .)
if (length(caratColorOpt) != 1) 
  stop_externals('Impossible') else caratColorOpt[[1]] %>% click()
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
  expect_true(waitFor({
    text(driver %>% getEl('#color option[selected="selected"]')) == 'color' }, 
    catchStale=T))
  expect_true(waitFor({ is.null(driver %>% getEl('#treatColorAsFactor')) }))
  expect_true(!is.null(waitFor('#submit[disabled]')))
  expect_true(has_shiny_correct_state(driver, '^reset', NULL, NULL, shortShotName=F))
})
