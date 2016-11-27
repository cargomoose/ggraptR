source(paste0(getwd(), '/auto/seleniumUtils.R'))

selServer <- startServer()
driver <- getDriver()
Sys.sleep(5)

ptCtrlEl <- getEl(driver, '//div[@id="plotTypesCtrl"]')
pTypesInputEl <- ptCtrlEl %>% getEl(byClass('selectize-input'))
pTypesInputEl %>% click  # adds selectable options to click

getTypesOptEls <- function() ptCtrlEl %>% getEls(byClass('option'))
stopifnot(length(getTypesOptEls()) == 5)
choosedTypes <- function() ptCtrlEl %>% getEls(byClass('item')) %>% text

for (i in 1:length(getTypesOptEls())) {
  driver$screenshot(T)
  print(choosedTypes())
  getTypesOptEls()[[1]] %>% click
  Sys.sleep(6)
}
driver$screenshot(T)

# stopExternals(driver, selServer)
