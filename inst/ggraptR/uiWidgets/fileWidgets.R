## file input select control
output$fileInputSelectCtrl <- renderUI({
  fileInput('file', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  )  
})

## file input header control
output$fileInputHeaderCtrl <- renderUI({
  checkboxGroupInput('header', 'Header Options', 
                     choices = c('Header'=TRUE),
                     selected = c(TRUE))  
})

## file input quote control
output$fileInputQuoteCtrl <- renderUI({
  radioButtons('quote', 'Quote',
               c(None='',
                 'Double Quote'='"',
                 'Single Quote'="'"),
               '"')  
})

## file input separator control
output$fileInputSepCtrl <- renderUI({
  radioButtons('sep', 'Separator',
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t'),
               ',')  
})

## CSV download button (for UI)
output$dlBtnCSV <- renderUI({
  downloadButton('dlCSV', 'Download')
})

# image download button (for UI)
output$dlBtnPlot <- renderUI({
  downloadButton('dlPlot', 'Download')
})

## user-defined factor variables control options
# output$usrDefFacVarsCtrl <- renderUI({
#   if (is.null(dataset()) | is.null(factorVars())) return()  
#   selectInput('usrDefFacVars', 'User-Defined Factor Variables',
#               choices=colnames(dataset()), 
#               selected=factorVars(),
#               multiple=T)
# })

output$exportPlotCtl <- renderUI({
  shinyBS::bsButton("exportPlot", label="Export Plot", type = "action", icon = icon("download"))
})

output$fileTypeCtl <- renderUI({
  selectInput(inputId = "fileType", label = "File Type", 
              choices = c('PDF'='.pdf',
                          'PNG'='.png',
                          'JPG'='.jpg',
                          'SVG'='.svg')
              )
})

output$fileHeightCtl <- renderUI({
  numericInput(inputId = "fileHeight", label = "Height (inches)", 
               value = gcnFileWidthDefault, min = 0, max = gcnFileHeightMax
               )
})

output$fileWidthCtl <- renderUI({
  numericInput(inputId = "fileWidth", label = "Width (inches)", 
              value = gcnFileHeightDefault, min = 0, max = gcnFileWidthMax
              )
})

output$fileDPICtl <- renderUI({
  numericInput(inputId = "fileDPI", label = "Dots Per Inch", 
               value = gcnFileDPIDefault, min = 0, max = gcnFileDPIMax
               )
})
