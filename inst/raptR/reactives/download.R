## CSV download handler
output$dlCSV <- downloadHandler(
  filename = function() { 
    ts <- gsub(' |-|:', '', as.character(Sys.time()))
    paste0('output_', ts, '.csv') 
  },
  content = function(file) {
    write.csv(finalDF(), file, row.names=F)
    #write.csv(dataset(), file, row.names=F)
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
    inputWidth<-input$fileWidth
    inputHeight<-input$fileHeight
    inputDPI<-input$fileDPI
    
    if (inputWidth < 0 || inputWidth > gcnFileWidthMax || !is.numeric(inputWidth))
    {
      updateNumericInput(session, "fileWidth", value = gcnFileWidthDefault)
      inputWidth<-gcnFileWidthDefault
    }
    if (inputHeight < 0 || inputHeight > gcnFileHeightMax || !is.numeric(inputHeight))
    {
      updateNumericInput(session, "fileHeight", value = gcnFileHeightDefault)
      inputHeight<-gcnFileHeightDefault
    }
    if (inputDPI < 0 || inputDPI > gcnFileDPIMax || !is.numeric(inputDPI))
    {
      updateNumericInput(session, "fileDPI", value = gcnFileDPIDefault)
      inputHeight<-gcnFileDPIDefault
    }
    
    ggsave(file, plot = plotInput(), width=inputWidth, height=inputHeight, units="in", dpi=inputDPI)
  }
)

## register the final dataset to be used upon AJAX call from UI
action <- reactive({DT::dataTableAjax(session, finalDF())})

