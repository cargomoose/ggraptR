verticalLayout(
  
  conditionalPanel(condition = 'input.plotTypes == "pairs" && input.showAes',
    fluidRow(
      column(2, div()), 
      column(3, div(strong('Upper plots'))),
      column(3, div(strong('Lower'))), 
      column(3, div(strong('Diag plots')))),
    br(),
    
    fluidRow(
      column(2, div(strong('Continu'))), 
      column(3, uiOutput('pairsUpContCtrl')),
      column(3, uiOutput('pairsLowContCtrl')), 
      column(3, uiOutput('pairsDiagContCtrl'))),
      
    fluidRow(
      column(2, div(strong('Discrete'))), 
      column(3, uiOutput('pairsUpDiscrCtrl')),
      column(3, uiOutput('pairsLowDiscrCtrl')), 
      column(3, uiOutput('pairsDiagDiscrCtrl'))),
    
    fluidRow(
      column(2, div(strong('Combo'))), 
      column(3, uiOutput('pairsUpComboCtrl')),
      column(3, uiOutput('pairsLowComboCtrl'))),
    hr()),
    
  
  fluidRow(
    column(6, uiOutput('colorCtrl')),
    column(6, br(), uiOutput('treatColorAsFactorCtrl'))),
  
  fluidRow(
    column(6, uiOutput('fillCtrl')),
    column(6, uiOutput('positionCtrl'))),
  
  fluidRow(
    column(6, uiOutput('sizeCtrl')),
    column(6, uiOutput('shapeCtrl'))),
  
  fluidRow(
    column(6, uiOutput('smoothCtrl'))),
  
  fluidRow(
    column(6, uiOutput('coordFlipCtrl')),
    column(6, uiOutput('jitterCtrl'))),
  
  uiOutput('alphaCtrl'),
  uiOutput('sizeMagCtrl'),
  uiOutput('nBinsCtrl'),
  
  uiOutput('densBlackLineCtrl')
)
