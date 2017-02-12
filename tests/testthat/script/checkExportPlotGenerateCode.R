cat("\nExport plot and generate code buttons")

invisible(apply(
  data.frame(modalBt=c('#exportPlot', '#generatePlotCode'),
             modalRoot=c('#modalExportOptions', '#modalCodeView'),
             dwnload=c('a#dlPlot', NA)), 1, 
  
  function(row) {
    driver %>% getEl(row['modalBt']) %>% click()
    root <- waitFor(paste0(row['modalRoot'], '[style="display: block;"]'), driver)
    
    test_that(paste(row['modalBt'], 'button works fine'), {
      if (!is.na(row['dwnload'])) {
        expect_true(!is.null(waitFor('a#dlPlot', root)))
      } else {
        waitFor({ text(root %>% getEl('#generateCode')) != '' })
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
