output$datasetOptionsCtrl <- renderUI({
  bsButton("datasetOptionsBtn", label=NULL, type="action", icon=icon("cog"))
})

# file input select control
output$fileInputSelectCtrl <- renderUI({
  rawDataset()
  fileInput('file', 'Choose file to upload', 
            accept=c('text/csv', 'text/comma-separated-values', 
                     'text/tab-separated-values', 'text/plain', '.csv', '.tsv'))
})

# file input header control
output$fileInputHeaderCtrl <- renderUI({
  checkboxGroupInput('header', 'Header Options', choices=c('Header'=TRUE), selected=T)  
})

# file input quote control
output$fileInputQuoteCtrl <- renderUI({
  radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')  
})

# file input separator control
output$fileInputSepCtrl <- renderUI({
  radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ',')  
})

output$uploadDataCtrl <- renderUI({
  bsButton("uploadData", label=NULL, type="action", icon=icon("plus"))
})

# CSV download button (for UI)
output$dlBtnCSV <- renderUI({
  downloadButton('dlCSV', 'Download table')
})

# image download button (for UI)
output$dlBtnPlot <- renderUI({
  downloadButton('modalDlBtnPlot', 'Download')
})

output$downloadPlotCtrl <- renderUI({
  bsButton("downloadPlot", label=NULL, type="action", icon=icon("download"))
})

output$fileTypeCtrl <- renderUI({
  selectInput(inputId="fileType", label="File Type", 
              choices=c('PDF'='.pdf', 'PNG'='.png', 'JPG'='.jpg', 'SVG'='.svg'))
})

output$fileHeightCtrl <- renderUI({
  numericInput(inputId="fileHeight", label="Height (inches)", 
               value=getFileDefault()$width, min=0, max=getFileDefault()$heightMax)
})

output$fileWidthCtrl <- renderUI({
  numericInput(inputId="fileWidth", label="Width (inches)", 
              value=getFileDefault()$height, min=0, max=getFileDefault()$widthMax)
})

output$fileDPICtrl <- renderUI({
  numericInput(inputId="fileDPI", label="Dots Per Inch", 
               value=getFileDefault()$DPI, min=0, max=getFileDefault()$DPIMax)
})


output$dbDriverTypeCtrl <- renderUI({
  radioButtons('dbDriverTypeCtrl', 'Driver type', c('MySQL', 'PostgreSQL', 'SQLite'))
})

output$dbExecuteBtn <- renderUI({
  bsButton("dbExecuteBtn", label="Run", icon=icon("play-circle-o"), type="action")
})
