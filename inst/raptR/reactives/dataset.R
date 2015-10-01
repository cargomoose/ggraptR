## reactive variable for custom (uploadable) dataset file info
customDatasetFileInfo <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  fileInfo <- input$file
  fileInfo  
})

## reactive variable for custom (uploaded) dataset
customDataset <- reactive({
  fileInfo <- customDatasetFileInfo()
  if (is.null(fileInfo)) return()
  if (is.null(input$header) | is.null(input$sep) | is.null(input$quote)) return()
  read.csv(fileInfo$datapath, header = as.logical(input$header),
           sep = input$sep, quote = input$quote)
})

## reactive variable for custom dataset name
customDatasetName <- reactive({
  customDatasetFileInfo()$name
})

## reactive variable for raw dataset names
rawDatasetNames <- reactive({
  c("diamonds", "mtcars", "rock", 
    customDatasetName(),
    getLoadedDataFrameNames())
})

## reactive variable for raw dataset
rawDataset <- reactive({
  ## from dataset selection drop-down
  if (is.null(input$dataset)) return() 
  
  ## if no custom dataset was uploaded, then set one of the preloaded datasets as raw dataset
  if (is.null(input$file)) {
    get(input$dataset)
  }
  
  ## if custom dataset was uploaded
  else {
    ## if custom dataset was selected, then set it as raw dataset
    if (input$dataset == customDatasetName()) {
      customDataset()      
    } 
    
    ## if custom dataset was not selected, then set one of the preloaded datasets as raw dataset
    else {
      get(input$dataset)
    }
  }
})

## manually aggregated dataset
manAggDataset <- reactive({
  ## if all fields for manual aggregation are filled in
  if (!is.null(input$aggBy) & !is.null(input$aggTarget) & !is.null(input$aggMeth)) {
    ## return manually aggregated dataset
    df <- aggregate(rawDataset(), input$aggBy, input$aggTarget, input$aggMeth)
  } 
  
  ## else, return raw dataset  
  else {
    df <- rawDataset()
  }

  df
})

## raw or aggregated dataset
dataset <- reactive({

  #if (is.null(input$rawVsManAgg)) return()
  if (is.null(input$rawVsManAgg)) return(rawDataset())

  ## raw dataset
  if (input$rawVsManAgg == 'raw') {
    dataset <- rawDataset()
  } 

  ## aggregated dataset
  else if (input$rawVsManAgg=='manAgg') {
    dataset <- manAggDataset()
  }

  dataset
})


## reactive for base semi-automatic aggregate-by fields 
## (that are NOT in the "additional aggregate-by" fields)
plotSemiAutoAggByBase <- reactive({
  aggBy <- c(input$x, input$color, input$size, input$shape, input$fill, input$facetRow, input$facetCol, input$facetWrap)
  aggBy <- cleanPlotAggBy(input$x, input$y, aggBy)
  aggBy
})

## reactive for semi-automatic aggregate by
## base + additional aggregate-by fields
plotSemiAutoAggBy <- reactive({  
  if (is.null(dataset())) return()
  aggBy <- c(plotSemiAutoAggByBase(), input$plotAddAggBy)
  aggBy <- cleanPlotAggBy(input$x, input$y, aggBy)
  aggBy <- rmElemsNotInDatasetCols(aggBy, dataset())
  aggBy
})

## reactive for semi-automatic aggregated dataset
semiAutoAggDF <- reactive({
  if (is.null(semiAutoAggOn())) return()
  if (is.null(dataset())) return()

  ## if plot aggregation is specified (e.g. sum, mean, max, min)  
  if (semiAutoAggOn()) {
    if (is.null(plotSemiAutoAggBy())) return()
    
    aggBy <- plotSemiAutoAggBy()
    aggTarget <- input$y
    aggMeth <- input$plotAggMeth
    
    vars <- c(aggBy, aggTarget)
    if (all(vars %in% colnames(dataset()))) {
      semiAutoAggDF <- aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
      semiAutoAggDF 
    }
  } 
})

## reactive variable for final dataset
finalDF <- reactive({
  if (is.null(dataset())) return()
  if (is.null(semiAutoAggOn())) return(dataset())
  
  ## semi-automatic aggregation (if enabled)
  if (semiAutoAggOn())
    semiAutoAggDF()

  ## natural dataset (raw or manually aggregated dataset)
  else
    dataset()
})


## reactive dataset used for plotting 
## (filtered version of finalDF(), using xlim and ylim)
plotDF <- reactive({
  dataset <- finalDF(); if (is.null(dataset)) return()
  
  ## subset with xlim filter (if applicable)
  if (!is.null(xlim())) {
    x <- input$x; if(is.null(x)) return()
    if (is.null(xType())) return()
    if (xType()=='continuous')
      dataset <- dataset[dataset[[x]] >= xlim()[1] & dataset[[x]] <= xlim()[2], ]
    else if (xType()=='discrete')
      dataset <- dataset[dataset[[x]] %in% xlim(), ]
  }
  
  ## subset with ylim filter (if applicable)
  if (!is.null(ylim())) {
    if (isXYCtrlPlot()) {
      y <- y(); if (is.null(y)) return()
      if (is.null(yType())) return()
      if (yType()=='continuous')
        dataset <- dataset[dataset[[y]] >= ylim()[1] & dataset[[y]] <= ylim()[2], ]
      else if (yType()=='discrete')
        dataset <- dataset[dataset[[y]] %in% ylim(), ]
    }
  }
  
  return(dataset)
})

