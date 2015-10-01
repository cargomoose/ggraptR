## plot options
verticalLayout(
  uiOutput('plotTypeCtrl'),
  
  fluidRow(
    column(6,
           uiOutput('xCtrl')             
    ),
    column(6,
           uiOutput('yCtrl')
    )
  ),
  
  source('./views/plot/aesCtrlsUI.R', local=TRUE)$value,
  source('./views/plot/facetCtrlsUI.R', local=TRUE)$value,
  source('./views/plot/xyRangeCtrlsUI.R', local=TRUE)$value,
  source('./views/plot/DSTypeAndPlotAggCtrlsUI.R', local=TRUE)$value,
  source('./views/plot/labelAndStyleCtrlsUI.R', loca=TRUE)$value,
  
  ## widgets to show/hide advanced control widgets
  uiOutput('showAesWgtsCtrl'),
  uiOutput('showFacetWgtsCtrl'),  
  uiOutput('showXYRangeWgtsCtrl'),
  uiOutput('showThemeWgtsCtrl'),
  uiOutput('showDSTypeAndPlotAggWgtsCtrl')
  #uiOutput('showPlotAggWgtCtrl')
)