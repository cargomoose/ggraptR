verticalLayout(
  uiOutput('plotTitleCtrl'),
  
  fluidRow(
    column(6,
           uiOutput('xLabelCtrl')
    ),
    column(6,
           uiOutput('yLabelCtrl')
    )
  ),
  
  fluidRow(
    column(6,
           uiOutput('labelFontFamilyCtrl')         
    ),
    column(6,
           uiOutput('labelFontFaceCtrl')
    )
  ),
  
  fluidRow(
    column(6,
           uiOutput('labelFontSizeCtrl')
    ),
    column(6,
           uiOutput('labelFontColorCtrl')
    )
  ), 
  
  fluidRow(
    column(6,
           uiOutput('hjustCtrl')
    ), 
    column(6,
           uiOutput('vjustCtrl')
    )
  ),
  
  uiOutput('plotThemeCtrl')
)

