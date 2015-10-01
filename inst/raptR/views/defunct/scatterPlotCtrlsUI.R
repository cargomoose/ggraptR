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
  
  fluidRow(
    column(6, 
           uiOutput('sizeCtrl')
    ),
    column(6,
           uiOutput('shapeCtrl')
    )
  ),
  
  fluidRow(
    column(6, 
           uiOutput('jitCtrl')
    ),
    column(6,
           uiOutput('smthCtrl')
    )
  ),
  
  source('./views/facetCtrlsUI.R', local=TRUE)$value,
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('coordFlipCtrl')
)