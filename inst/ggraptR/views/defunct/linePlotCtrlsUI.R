verticalLayout(
  fluidRow(
    column(6,
           uiOutput('xCtrl')             
    ),
    column(6,
           uiOutput('yCtrl')
    )
  ),
  fluidRow(
    column(6,
           uiOutput('colCtrl')
    )
  ),
  
  source('./views/facetCtrlsUI.R', local=TRUE)$value,
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('coordFlipCtrl')  
)

