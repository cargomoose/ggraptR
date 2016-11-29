# not sure about non-gray themes. May not work
source(paste0(getwd(), '/auto/seleniumUtils.R'))

selServer <- startServer()
# plotPic <- function() getEl(driver, '#plot img') %>% attr('src')
waitPlotReady <- function() {
  # get the first blank and the second full plot images
  # waitFor(quote({ pic <- plotPic();  grepl('A{1000,}', pic) && nchar(pic) < 1500 }))
  # waitFor(quote({ pic <- plotPic(); !grepl('A{1000,}', pic) && nchar(pic) > 1500 }))
  
  # need to know approx count of empty value in blank plot. Depends on screen resolution
  stopifnot(driver$getWindowSize()$height== 1080 && driver$getWindowSize()$width == 1920)
  emptyPicContent <- paste0(rep('A', 1e3),collapse='')
  waitFor(sprintf('#plot img[src*="%s"]', emptyPicContent))  # blank plot
  waitFor(c(sprintf('#plot img:not([src*="%s"])', emptyPicContent), 
            '#plot.shiny-output-error'))  # normal plot or an error
}

system.time({
  driver <- getDriver()
  # print(html(x))
  # driver$screenshot(T)
  # browser()
  # waitFor('#plot img')
  waitPlotReady()
})

ptCtrlEl <- getEl(driver, '#plotTypesCtrl')
ptCtrlEl %>% getEl('.selectize-input') %>% click  # fills plot options list

choosedTypes <- function() ptCtrlEl %>% getEls('.item') %>% text
typesOptEls <- function() ptCtrlEl %>% getEls('.option')
stopifnot(length(waitFor(typesOptEls)) == 5)

while (length(opts <- typesOptEls())) {
  opts[[1]] %>% click
  waitPlotReady()
  driver$screenshot(T)
}

# stopExternals(driver, selServer)
