# plot options
verticalLayout(
  div(
    uiOutput('plotTypesCtrl', 
             style = 'float: left; margin-right: 44px; width:-webkit-fill-available'),
    div(
      uiOutput('dlPlotOpenModalCtrl', style= 'padding-bottom: 5px;'), 
      uiOutput('pTypesWarnBtnCtrl'),  
      style='float: left; margin-left: -40px; padding-top: 25px; display: inline-block')),
  
  fluidRow(
    column(6, uiOutput('xCtrl')),
    column(6, uiOutput('yCtrl'))),
  
  uiOutput('columnsCtrl'),  # for ggpairs
  
  
  bsCollapse(id = "aesBlock", open = "Aesthetic",
             bsCollapsePanel("Aesthetic", 
                             source('./views/plotBlock/aesCtrlsUI.R', local=T)$value, 
                             style = "warning")),
  
  conditionalPanel(
    'input.plotTypes != "pairs"',
    bsCollapse(
      id = "extraPlotBlocks", open = c("Facet", "Theme", "Aggregation"), multiple=T,
      bsCollapsePanel(
        "Facet", style = "warning", 
        source('./views/plotBlock/facetCtrlsUI.R', local=T)$value),
      bsCollapsePanel(
        "Theme", style = "warning", 
        source('./views/plotBlock/labelAndStyleCtrlsUI.R', local=T)$value),
      bsCollapsePanel(
        "Aggregation", style = 'warning', 
        source('./views/plotBlock/DSTypeAndPlotAggCtrlsUI.R',local=T)$value)))
)
