shinyUI(bootstrapPage(
  
  headerPanel(windowTitle="ggraptR", title=div()),
  
  tags$head(tags$style(
    type="text/css",
    "#rappy img {max-width: 100%;}",
    "#react_row {max-width: 100%;}",
    ".widblock {background-color: #F9F9F9; padding: 2px 10px; margin:5px}")),
  
  sidebarPanel(
    splitLayout(
      cellWidths = c("25%", "75%"),
      imageOutput("rappy", height = "100%"),
      div(
          # use shinyjs to disable/enable buttons w/ JS
          shinyjs::useShinyjs(),
          uiOutput('resetable_input'),
          actionButton("reset_input", "Reset inputs", width = "100%"),
          br(), br(),
          # reactive vs. upon-manual-submit calculations
          
          fluidRow(
            column(6, uiOutput('submitCtrl')), 
            column(6, uiOutput('reactiveCtrl')),
            id='react_row'))),
    br(),
    
    conditionalPanel(
      condition = 'input.conditionedPanels != "logTab"',
      fluidRow(column(6, uiOutput('datasetCtrl')),
               conditionalPanel('input.conditionedPanels == "plotTab"',
                                column(6, uiOutput('plotTypesCtrl'))))),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == "plotTab"',
      source('./views/plot/plotCtrlsUI.R', local=TRUE)$value),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == "tableTab"',
      source('./views/table/manAggCtrlsUI.R', local=TRUE)$value),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == "importTab"',
      actionButton("viewPlot", label = "View Plot"),
      hr(),
      source('./views/import/dataImportCtrlsUI.R', local=TRUE)$value)),
  
  mainPanel(
    #import modal panels
    source('./views/modals/modalPanels.R',local=TRUE)$value,
    
    tabsetPanel(type = "tabs",
      tabPanel("Plot", 
        br(),
        fluidRow(column(2, uiOutput('exportPlotCtl')),
                 column(2, uiOutput('generatePlotCodeCtl'))),
        br(),
        plotOutput("plot", brush=brushOpts(id="zoom_brush", resetOnNew=T)),
        uiOutput("itersToDrawCtrl", style="opacity: 0; pointer-events: none"),
        value='plotTab'), 
      
      tabPanel("Table",
               br(),
               uiOutput('dlBtnCSV'),
               br(),
               DT::dataTableOutput("displayTable"),
               value='tableTab'),
      
      tabPanel('Import',
               tags$head(tags$script(
                  'Shiny.addCustomMessageHandler("myCallbackHandler",
                    function(typeMessage) {
                      if(typeMessage == 1){
                        $("a:contains(Plot)").click();
                      } 
                    });')),
               value='importTab'),
      
      tabPanel('Log',
               br(),
               htmlOutput('plotLog'),
               value='logTab'),
      id = "conditionedPanels"))))
