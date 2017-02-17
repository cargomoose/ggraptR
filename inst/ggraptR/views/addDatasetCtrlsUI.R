tabsetPanel(
  type = "tabs",
  tabPanel(
    "File", 
    verticalLayout(
      br(),
      uiOutput('fileInputSelectCtrl'),
      
      fluidRow(
        column(4, uiOutput('fileInputHeaderCtrl')),
        column(4, uiOutput('fileInputQuoteCtrl')),
        column(4, uiOutput('fileInputSepCtrl')))),

    value='addFileTab'),
  
  tabPanel(
    "Database",
    br(),
    splitLayout(
      cellWidths = c("25%", "75%"),
      uiOutput('dbDriverTypeCtrl'),
     
      div(
        fluidRow(
          column(8, textInput('dbHost', 'Host', 'db4free.net')),
          column(4, textInput('dbPort', 'Port', '3307'))),
       
        fluidRow(
          column(6, textInput('dbUser', 'User', 'gray')),
          column(6, passwordInput('dbPass', 'Password', '12348888'))),
       
          textInput('dbName', 'Db name', 'ggtest'),
        
        style='padding-right: 15px;')),
    
    br(),
    fluidRow(
      column(9, textAreaInput('dbSqlQuery', 'Sql query', 'select * from iris_short',
                              width='430px')),
      column(3, uiOutput('dbExecuteBtn'), style='padding-top:25px')),
    
    value='addDbTab')
)
