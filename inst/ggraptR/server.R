pacman::p_load(
  colourpicker, dplyr,DT, futile.logger, GGally, ggplot2, ggthemes, jsonlite, 
  purrr, shiny, shinyBS, shinyjs, stringr, svglite)

# by default, the file size limit is 5MB. 1 Gb = 1024^3 b
options(shiny.maxRequestSize = 10 * 1024^3)

source('globals.R')
source('debug/debug.R', local=T)  # set debug logs
source('functions/helper.R')
sourceAllInDir('functions', except='helper.R')

serverVals <- reactiveValues(nRunnedSessions=0)  # server globals
  
shinyServer(function(input, output, session) {
  reactVals <- reactiveValues(log=NULL, readyToDraw=F, plotState=list()) # session globals
  
  sourceAllInDir('reactives', local=T)  # reactive variables
  sourceAllInDir('uiWidgets', local=T)  # UI controls
  
  output$rappy <- renderImage({
    list(src = "www/RAPPY.png",
         contentType = "image/png", alt = "ggraptR")
    }, deleteFile = FALSE)  
  
  source('observeEvents.R', local=TRUE)
  
  isolate(serverVals$nRunnedSessions <- serverVals$nRunnedSessions + 1)

  session$onSessionEnded(function() {
    isolate({
      serverVals$nRunnedSessions <- serverVals$nRunnedSessions - 1
      if (!serverVals$nRunnedSessions) {
        stopApp()
      }
    })
  })
})
