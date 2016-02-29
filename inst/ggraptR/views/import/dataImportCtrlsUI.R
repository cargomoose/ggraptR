verticalLayout(
  uiOutput('fileInputSelectCtrl'),
  
  fluidRow(
    column(4,
           uiOutput('fileInputHeaderCtrl')
    ),
    column(4,
           uiOutput('fileInputQuoteCtrl')
    ),
    column(4,
           uiOutput('fileInputSepCtrl')
    )
  )  # end of fluidRow for data import options   
)