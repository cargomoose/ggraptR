pacman::p_load(shinyBS, shinyjs, DT)

shinyUI(bootstrapPage(
  headerPanel(windowTitle="ggraptR", title=div()),
  tags$head(tags$style(
    type="text/css",
    "hr {margin-top: 15px; margin-bottom: 15px; border-color: #d9d9d9}",
    ".sbs-panel-group {margin-bottom: 5px;}",
    # ".shiny-split-layout #datasetNameCtrl { overflow: visible; }",
    "#rappy img {max-width: 100%;}"  # "#react_row {max-width: 100%;}"
  )),
  
  sidebarPanel(
    splitLayout(
      cellWidths = c("25%", "75%"),
      imageOutput("rappy", height = "100%"),
      div(
        # use shinyjs to disable/enable buttons w/ JS
        useShinyjs(),
        
        div(
          uiOutput(
            'datasetNameCtrl', style = paste(
              'float: left; margin-right: 83px; width:-webkit-fill-available;
              white-space: nowrap;')),
          div(
            uiOutput('uploadDataCtrl', inline = T),
            uiOutput('datasetOptionsCtrl', inline = T),
            style='float: left; margin-left: -79px; padding-top: 25px')),
        
        conditionalPanel(
          condition = 'input.conditionedPanels == "tableTab"',
          uiOutput('dlBtnCSV')),
        
        conditionalPanel(
          condition = 'input.conditionedPanels != "codeTab"',
          hr(),
          fluidRow(
            column(6, uiOutput('submitCtrl', style='overflow:hidden')), 
            column(6, uiOutput('reactiveCtrl')),
            id='react_row'), 
          actionButton("reset_input", "Reset inputs", icon=icon("refresh"), width = "100%")),
        
        conditionalPanel(
          condition = 'input.conditionedPanels == "codeTab"',
          div(br(), style='padding-bottom: 120px')),
        
        style='padding-left: 10px; padding-right: 4px; overflow: -webkit-paged-x; white-space: normal')),
    hr(),
    
    #### left controls ####
    conditionalPanel(
      condition = 'input.conditionedPanels == "plotTab"',
      source('./views/plotCtrlsUI.R', local=TRUE)$value),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == "tableTab"',
      source('./views/tableCtrlsUI.R', local=TRUE)$value),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == "codeTab"',
      source('./views/codeCtrlsUI.R', local=TRUE)$value)),
  
  #### right view ####
  mainPanel(
    source('./views/modalPanels.R',local=TRUE)$value,
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot", br(), br(),
        plotOutput("plot", brush=brushOpts(id="zoom_brush",resetOnNew=T)),
        uiOutput("itersToDrawCtrl", 
                 style="opacity: 0; pointer-events: none"),
        value='plotTab'), 
      
      tabPanel(
        "Table", br(), br(),
        DT::dataTableOutput("displayTable"),
        value='tableTab'),
      
      tabPanel(
        'Code', br(),
        strong('Plot log (latest at the top):'),
        br(), br(),
        htmlOutput('plotLog'),
        value='codeTab'),
      id = "conditionedPanels"))))
