# plot options
verticalLayout(
  
  uiOutput('plotTypesCtrl'),
  
  fluidRow(
    column(6, uiOutput('xCtrl')),
    column(6, uiOutput('yCtrl'))),
  
  uiOutput('columnsCtrl'),  # for ggpairs
  
 
  bsCollapse(id = "aesBlock", open = "Aesthetic",
    bsCollapsePanel("Aesthetic", 
                    source('./views/plotBlock/aesCtrlsUI.R', local=T)$value, 
                    style = "warning")),
  
  conditionalPanel('input.plotTypes != "pairs"',
    bsCollapse(
      id = "extraPlotBlocks", open = c("Facet", "Theme", "Aggregation"), multiple=T,
      bsCollapsePanel("Facet", 
                      source('./views/plotBlock/facetCtrlsUI.R', local=T)$value, 
                      style = "warning"),
      bsCollapsePanel("Theme", 
                      source('./views/plotBlock/labelAndStyleCtrlsUI.R', local=T)$value, 
                      style = "warning"),
      bsCollapsePanel("Aggregation", 
                      source('./views/plotBlock/DSTypeAndPlotAggCtrlsUI.R',local=T)$value,
                      style = 'warning'))
  )
)
