div(
  br(),
  fluidRow(
    column(8, textInput('console', NULL, placeholder = 'Enter R command here')),
    column(3, uiOutput('evalConsoleBtn'))),
  
  div(textOutput('consoleCtrl'), style="padding-left: 10px; padding-right: 40px;")
)
