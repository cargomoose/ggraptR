verticalLayout(
  fluidRow(
    column(6, uiOutput('colCtrl')),
    column(6, uiOutput('treatAsFacVarColCtrl'))),
  
  fluidRow(
    column(6, uiOutput('fillCtrl')),
    column(6, uiOutput('posCtrl'))),
  
  uiOutput('ptsOverlayCondCtrl'),
  
  fluidRow(
    column(6, uiOutput('sizeCtrl')),
    column(6, uiOutput('shapeCtrl'))),
  
  fluidRow(
    column(6, uiOutput('smthCtrl'))),
  
  fluidRow(
    column(6, uiOutput('jitCtrl')),
    column(6, uiOutput('coordFlipCtrl'))),
  
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('binWidthCtrl'),
  
  uiOutput('densBlkLineCondCtrl'),
  
  uiOutput('ggpairsUpContCtrl'),
  uiOutput('ggpairsUpComboCtrl'),
  uiOutput('ggpairsUpDiscrCtrl'),
  uiOutput('ggpairsDiagContCtrl'),
  uiOutput('ggpairsDiagDiscrCtrl'),
  uiOutput('ggpairsLowContCtrl'),
  uiOutput('ggpairsLowComboCtrl'),
  uiOutput('ggpairsLowDiscrCtrl')
)
