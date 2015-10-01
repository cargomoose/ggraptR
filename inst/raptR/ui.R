shinyUI(pageWithSidebar(
  
  headerPanel("Data Explorer"),
  sidebarPanel(
    
    ## use shinyjs to disable/enable buttons w/ JS
    shinyjs::useShinyjs(),
    
    ## reactive vs. upon-manual-submit calculations
    uiOutput('submitCtrl'),
    
    ## enable reactive option
    uiOutput('reactiveCtrl'),
    
    hr(),
    
    ## dataset selection
    uiOutput('datasetCtrl'),
    
    hr(),
    
    ## file input/upload panel
    conditionalPanel(
      condition = 'input.conditionedPanels=="importTab"',
      source('./views/import/dataImportCtrlsUI.R', local=TRUE)$value
    ),  # end of file input/upload panel
    
    ## aggregation options
    conditionalPanel(
      condition = 'input.conditionedPanels=="tableTab"',
      source('./views/table/manAggCtrlsUI.R', local=TRUE)$value
    ),  # end of conditionalPanel for aggregation options
    
    ## plot options
    conditionalPanel(
      condition = 'input.conditionedPanels=="plotTab"',
      source('./views/plot/plotCtrlsUI.R', local=TRUE)$value
    )  # end of conditionalPanel for plot options

  ),  # end of sidebarPanel
  
  mainPanel(
    #import modal panels
    source('./views/modals/modalPanels.R',local=TRUE)$value,
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot", 
                         br(),
                         uiOutput('exportPlotCtl'),
                         br(),
                         plotOutput("plot", brush=brushOpts(id="zoom_brush", resetOnNew=TRUE)),
                         value='plotTab'
                ),
                tabPanel("Table", 
                         br(),
                         uiOutput('dlBtnCSV'),
                         br(),
                         DT::dataTableOutput("displayTable"),
                         value='tableTab'
                         ),
                tabPanel('Import',
                         value='importTab'
                ),
                id = "conditionedPanels"
    )    
  )
))