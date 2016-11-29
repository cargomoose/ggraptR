source(paste0(getwd(), '/auto/seleniumUtils.R'))

selServer <- startServer()
plotPic <- function() getEl(driver, '#plot img[src]') %>% attr('src')
waitPlotReady <- function() {
  # the first plot will be blank. We need to wait for the second
  # empty plot 'img' attribute contains many 'A' chars and has low nchar
  waitFor(quote({ pic <- plotPic(); grepl('A{1000,}', pic) && nchar(pic) < 1500 }))
  waitFor(quote({ pic <- plotPic(); !grepl('A{1000,}', pic) && nchar(pic) > 1500 }))
}

system.time({
  driver <- getDriver()
  waitFor('#plot img')
  waitPlotReady()
})

ptCtrlEl <- getEl(driver, '#plotTypesCtrl')
ptCtrlEl %>% getEl('.selectize-input') %>% click  # fills plot option list

choosedTypes <- function() ptCtrlEl %>% getEls('.item') %>% text
typesOptEls <- function() ptCtrlEl %>% getEls('.option')
stopifnot(length(waitFor(typesOptEls)) == 5)

while (length(opts <- typesOptEls())) {
  opts[[1]] %>% click
  waitPlotReady()
  driver$screenshot(T)
}

# stopExternals(driver, selServer)
