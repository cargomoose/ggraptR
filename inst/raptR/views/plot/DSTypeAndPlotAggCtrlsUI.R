verticalLayout(
  fluidRow(
    column(6,
           uiOutput('rawVsManAggCtrl')
    ),
    column(6,
           uiOutput('plotAggMethCtrl')
    )
  ),
  
  uiOutput('plotAddAggByCtrl')
)
