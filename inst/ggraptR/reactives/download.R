## CSV download handler
output$dlCSV <- downloadHandler(
  filename = function() { 
    ts <- gsub(' |-|:', '', as.character(Sys.time()))
    paste0('output_', ts, '.csv')
  },
  content = function(file) {
    write.csv(manAggDataset(), file, row.names=F)
  }
)

# file output
output$dlPlot <- downloadHandler(
  filename = function() {
    ts <- gsub(' |-|:', '', as.character(Sys.time()))
    paste0('output_', ts, input$fileType)
  },
  content = function(file) {
    # Prepare file parameters for download
    # Local variables assigned because updateNumericInput() execution takes places after
    # the completion of the calling function (ie. the updates will not be made in time for
    # the call to ggsave() and must be resolved locally)
    inputWidth <- input$fileWidth
    inputHeight <- input$fileHeight
    inputDPI <- input$fileDPI
    
    if (inputWidth < 0 || inputWidth > fileDefault$widthMax 
        || !is.numeric(inputWidth)) {
      updateNumericInput(session, "fileWidth", value=fileDefault$width)
      inputWidth <- fileDefault$width
    }
    if (inputHeight < 0 || inputHeight > fileDefault$heightMax 
        || !is.numeric(inputHeight)) {
      updateNumericInput(session, "fileHeight", value=fileDefault$height)
      inputHeight <- fileDefault$height
    }
    if (inputDPI < 0 || inputDPI > fileDefault$DPIMax || !is.numeric(inputDPI)) {
      updateNumericInput(session, "fileDPI", value=fileDefault$DPI)
      inputHeight <- fileDefault$DPI
    }
    
    if (plotTypes() == 'pairs') {
      dev <- ggplot2:::plot_dev(NULL, file, inputDPI)
      dev(file=file, width=inputWidth, height=inputHeight)
      print(buildPlot())
      dev.off()
    } else
      ggsave(file, plot=buildPlot(), width=inputWidth, height=inputHeight, 
           units="in", dpi=inputDPI)
  }
)

## register the final dataset to be used upon AJAX call from UI
action <- reactive({DT::dataTableAjax(session, aggDf())})

