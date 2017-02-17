# plot options
verticalLayout(
  
  uiOutput('plotTypesCtrl'),
  
  fluidRow(
    column(6, uiOutput('xCtrl')),
    column(6, uiOutput('yCtrl'))),
  
  uiOutput('columnsCtrl'),  # for ggpairs
  
  # widgets to show/hide advanced control widgets
  div(
    uiOutput('showAesCtrl'),
    source('./views/plotBlock/aesCtrlsUI.R', local=TRUE)$value,
    class="widblock", style="margin-bottom: 0px;"),
  
  conditionalPanel('input.plotTypes != "pairs"',
    div(
      uiOutput('showFacetCtrl'),
      source('./views/plotBlock/facetCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showXYRangeCtrl'),
      source('./views/plotBlock/xyRangeCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showThemeCtrl'),
      source('./views/plotBlock/labelAndStyleCtrlsUI.R', local=TRUE)$value,
      class="widblock"),
    
    div(
      uiOutput('showDSTypeAndPlotAggCtrl'),
      source('./views/plotBlock/DSTypeAndPlotAggCtrlsUI.R', local=TRUE)$value,
      class="widblock")
    
    #uiOutput('showPlotAggCtrl')
  )
)
