library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)
library(shinyBS)
library(shinyjs)
library(colourpicker)
library(ggthemes)
library(jsonlite)
library(svglite)
library(futile.logger)
library(GGally)

options(shiny.maxRequestSize = 10000 * 1024^2)  # by default, the file size limit is 5MB

source('globals.R')
source('debug/debug.R', local=T)  # set debug logs
source('functions/helper.R')
sourceAllInDir('functions', except='helper.R')

shinyServer(function(input, output, session) {
  reactVals <- reactiveValues(log=NULL, readyToDraw=F, plotState=list())  # like globals
  
  sourceAllInDir('reactives', local=T)  # reactive variables
  sourceAllInDir('uiWidgets', local=T)  # UI controls
  
  output$rappy <- renderImage({
    list(src = "www/RAPPY.png", height = "140px", width = "120px",
      contentType = "image/png", alt = "ggraptR")}, deleteFile = FALSE)  
  
  source('observeEvents.R', local=TRUE)
})
