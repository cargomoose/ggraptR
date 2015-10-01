verticalLayout(
  fluidRow(
    column(6,
           uiOutput('colCtrl')
    ),
    column(6,
           uiOutput('treatAsFacVarColCtrl')
    )
  ),
  
  fluidRow(
    column(6,
           uiOutput('fillCtrl')           
    ),
    column(6,
           uiOutput('posCtrl')
    )
  ),
  
  uiOutput('ptsOverlayCondCtrl'),
  
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
           uiOutput('smthCtrl')
    )
  ),
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('binWidthCtrl'),
  
  uiOutput('jitCtrl'),
  uiOutput('coordFlipCtrl'),
  uiOutput('densBlkLineCondCtrl')
)
