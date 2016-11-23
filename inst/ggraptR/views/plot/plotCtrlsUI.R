# plot options
verticalLayout(
  fluidRow(
    column(6, uiOutput('xCtrl')),
    column(6, uiOutput('yCtrl'))),
  
  uiOutput('columnsCtrl'),  # for ggpairs
  
  # widgets to show/hide advanced control widgets
  div(
    uiOutput('showAesCtrl'),
    source('./views/plot/aesCtrlsUI.R', local=TRUE)$value,
    class="widblock"),
  
  conditionalPanel('input.plotTypes != "pairs"',
    div(
      uiOutput('showFacetCtrl'),
      source('./views/plot/facetCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showXYRangeCtrl'),
      source('./views/plot/xyRangeCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showThemeCtrl'),
      source('./views/plot/labelAndStyleCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showDSTypeAndPlotAggCtrl'),
      source('./views/plot/DSTypeAndPlotAggCtrlsUI.R', local=TRUE)$value,
      class="widblock")
    
    #uiOutput('showPlotAggCtrl')
  )
)
