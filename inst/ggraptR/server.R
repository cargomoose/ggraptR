## import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)
library(shinyBS)
library(shinyjs)
library(ggthemes)
library(jsonlite)
library(svglite)
library(futile.logger)
library(GGally)

## import global variables
source('./global_variables.R', local=T)

## set debug logs
source('./debug/debug.R', local=T)

## import functions
source('./functions/helper.R')
source('./functions/ggplots.R')
source('./functions/aggregate.R')
source('./functions/ggraptplot.R')

options(shiny.maxRequestSize = 10000 * 1024^2)  # by default, the file size limit is 5MB

shinyServer(function(input, output, session) {
  ## reactive variables
  source('./reactives/reactives.R', local=TRUE)  # general/miscellaneous
  source('./reactives/dataset.R', local=TRUE)
  source('./reactives/plotWidgetVals.R', local=TRUE)
  source('./reactives/plotWidgetsDisplayCond.R', local=TRUE)
  source('./reactives/plot.R', local=TRUE)
  
  ## UI controls
  source('./uiWidgets/generalWidgets.R', local=TRUE)
  source('./uiWidgets/fileWidgets.R', local=TRUE)
  source('./uiWidgets/manAggWidgets.R', local=TRUE)
  source('./uiWidgets/plotWidgets.R', local=TRUE)
  
  output$rappy <- renderImage({
    list(src = "www/RAPPY.png", height = "140px", width = "120px",
      contentType = "image/png", alt = "ggraptR")}, deleteFile = FALSE)  
  
  source('./reactives/download.R', local=TRUE)
  source('./observeEvents.R', local=TRUE)
})
