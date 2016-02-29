## reactive variable for custom (uploadable) dataset file info
customDatasetFileInfo <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  flog.debug("dataset::customDatasetFileInfo() - Begin", name='all')
  fileInfo <- input$file
  flog.debug("dataset::customDatasetFileInfo() - End", name='all')
  fileInfo
})

## reactive variable for custom (uploaded) dataset
customDataset <- reactive({
  
  flog.debug("dataset::customDataset() - Begin", name='all')
  
  fileInfo <- customDatasetFileInfo()
  if (is.null(fileInfo)){
    flog.debug("dataset::customDataset() - is.null(fileInfo) - End", name='all')
    return()
  }
  if (is.null(input$header) | is.null(input$sep) | is.null(input$quote)){
    flog.debug("dataset::customDataset() - is.null(input$header) | is.null(input$sep) | is.null(input$quote) - End", name='all')
    return()
  }
  
  flog.debug("dataset::customDataset() - End", name='all')
  
  read.csv(fileInfo$datapath, header = as.logical(input$header),
           sep = input$sep, quote = input$quote)
})

## reactive variable for custom dataset name
customDatasetName <- reactive({
  
  flog.debug("dataset::customDatasetName() - Begin", name='all')
  
  flog.debug("dataset::customDatasetName() - End", name='all')
  
  customDatasetFileInfo()$name
})

## reactive variable for raw dataset names
rawDatasetNames <- reactive({
  
  flog.debug("dataset::rawDatasetNames() - Begin", name='all')
  
  flog.debug("dataset::rawDatasetNames() - End", name='all')
  
  c("diamonds", "mtcars", "rock", 
    customDatasetName(),
    getLoadedDataFrameNames())
})

## reactive variable for raw dataset
rawDataset <- reactive({
  
  flog.debug("dataset::rawDataset() - Begin", name='all')
  
  ## from dataset selection drop-down
  if (is.null(input$dataset)){
    flog.debug("dataset::rawDataset() - is.null(input$dataset) - End", name='all')
    return()
  }
  
  ## if no custom dataset was uploaded, then set one of the preloaded datasets as raw dataset
  if (is.null(input$file)) {
    flog.debug("dataset::rawDataset() - is.null(input$file) - End", name='all')
    get(input$dataset)
  }
  
  ## if custom dataset was uploaded
  else {
    ## if custom dataset was selected, then set it as raw dataset
    if (input$dataset == customDatasetName()) {
      flog.debug("dataset::rawDataset() - input$dataset == customDatasetName() - End", name='all')
      customDataset()      
    } 
    
    ## if custom dataset was not selected, then set one of the preloaded datasets as raw dataset
    else {
      flog.debug("dataset::rawDataset() - !(input$dataset == customDatasetName()) - End", name='all')
      get(input$dataset)
    }
  }
})

## manually aggregated dataset
manAggDataset <- reactive({
  
  flog.debug("dataset::manAggDataset() - Begin", name='all')
  
  ## if all fields for manual aggregation are filled in
  if (!is.null(input$aggBy) & !is.null(input$aggTarget) & !is.null(input$aggMeth)) {
    ## return manually aggregated dataset
    flog.debug("dataset::manAggDataset() - !is.null(input$aggBy) & !is.null(input$aggTarget) & !is.null(input$aggMeth)", name='all')
    df <- aggregate(rawDataset(), input$aggBy, input$aggTarget, input$aggMeth)
  }
  
  ## else, return raw dataset  
  else {
    flog.debug("dataset::manAggDataset() - !(!is.null(input$aggBy) & !is.null(input$aggTarget) & !is.null(input$aggMeth))", name='all')
    df <- rawDataset()
  }

  flog.debug("dataset::manAggDataset() - End", name='all')
  df
})

## raw or aggregated dataset
dataset <- reactive({
  
  flog.debug("dataset::dataset() - Begin", name='all')

  #if (is.null(input$rawVsManAgg)) return()
  if (is.null(input$rawVsManAgg)){
    flog.debug("dataset::dataset() - is.null(input$rawVsManAgg) - End", name='all')
    return(rawDataset())
  }

  ## raw dataset
  if (input$rawVsManAgg == 'raw') {
    flog.debug("dataset::dataset() - input$rawVsManAgg == 'raw' - End", name='all')
    dataset <- rawDataset()
  } 

  ## aggregated dataset
  else if (input$rawVsManAgg=='manAgg') {
    flog.debug("dataset::dataset() - input$rawVsManAgg=='manAgg' - End", name='all')
    dataset <- manAggDataset()
  }
  
  flog.debug("dataset::dataset() - End", name='all')

  dataset
})


## reactive for base semi-automatic aggregate-by fields 
## (that are NOT in the "additional aggregate-by" fields)
plotSemiAutoAggByBase <- reactive({
  flog.debug("dataset::plotSemiAutoAggByBase() - Begin", name='all')
  aggBy <- c(input$x, input$color, input$size, input$shape, input$fill, input$facetRow, input$facetCol, input$facetWrap)
  aggBy <- cleanPlotAggBy(input$x, input$y, aggBy)
  flog.debug("dataset::plotSemiAutoAggByBase() - End", name='all')
  aggBy
})

## reactive for semi-automatic aggregate by
## base + additional aggregate-by fields
plotSemiAutoAggBy <- reactive({
  
  flog.debug("dataset::plotSemiAutoAggBy() - Begin", name='all')
  
  if (is.null(dataset())){
    flog.debug("dataset::plotSemiAutoAggBy() - is.null(dataset()) - End", name='all')
    return()
  }
  aggBy <- c(plotSemiAutoAggByBase(), input$plotAddAggBy)
  aggBy <- cleanPlotAggBy(input$x, input$y, aggBy)
  aggBy <- rmElemsNotInDatasetCols(aggBy, dataset())
  
  flog.debug("dataset::plotSemiAutoAggBy() - End", name='all')
  
  aggBy
})

## reactive for semi-automatic aggregated dataset
semiAutoAggDF <- reactive({
  
  flog.debug("dataset::semiAutoAggDF() - Begin", name='all')
  
  if (is.null(semiAutoAggOn())){
    flog.debug("dataset::semiAutoAggDF() - is.null(semiAutoAggOn()) - End", name='all')
    return()
  }
  if (is.null(dataset())){
    flog.debug("dataset::semiAutoAggDF() - is.null(dataset()) - End", name='all')
    return()
  }
  ## if plot aggregation is specified (e.g. sum, mean, max, min)  
  if (semiAutoAggOn()) {
    flog.debug("dataset::semiAutoAggDF() - semiAutoAggOn()", name='all')
    if (is.null(plotSemiAutoAggBy())){
      flog.debug("dataset::semiAutoAggDF() - is.null(plotSemiAutoAggBy()) - End", name='all')
      return()
    }
    
    aggBy <- plotSemiAutoAggBy()
    aggTarget <- input$y
    aggMeth <- input$plotAggMeth
    
    vars <- c(aggBy, aggTarget)
    if (all(vars %in% colnames(dataset()))) {
      semiAutoAggDF <- aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
      flog.debug("dataset::semiAutoAggDF() - all(vars %in% colnames(dataset()) - End", name='all')
      semiAutoAggDF 
    }
  } 
})

## reactive variable for final dataset
finalDF <- reactive({
  
  flog.debug("dataset::finalDF() - Begin", name='all')
  
  if (is.null(dataset())){
    flog.debug("dataset::finalDF() - is.null(dataset()) - End", name='all')
    return()
  }
  if (is.null(semiAutoAggOn())){
    flog.debug("dataset::finalDF() - is.null(semiAutoAggOn()) - End", name='all')
    return(dataset())
  }
  
  ## semi-automatic aggregation (if enabled)
  if (semiAutoAggOn()){
    flog.debug("dataset::finalDF() - semiAutoAggOn() - End", name='all')
    semiAutoAggDF()
  }
  ## natural dataset (raw or manually aggregated dataset)
  else{
    flog.debug("dataset::finalDF() - !(semiAutoAggOn()) - End", name='all')
    dataset()
  }
})


## reactive dataset used for plotting 
## (filtered version of finalDF(), using xlim and ylim)
plotDF <- reactive({
  
  flog.debug("dataset::plotDF() - Begin", name='all')
  
  dataset <- finalDF()
  if (is.null(dataset)){
    flog.debug("dataset::plotDF() - is.null(dataset) - End", name='all')
    return()
  }
  
  ## subset with xlim filter (if applicable)
  if (!is.null(xlim())) {
    x <- input$x
    if(is.null(x)){
      flog.debug("dataset::plotDF() - is.null(x) - End", name='all')
      return()
    }
    if (is.null(xType())){
      flog.debug("dataset::plotDF() - is.null(xType()) - End", name='all')
      return()
    }
    if (xType()=='continuous'){
      flog.debug("dataset::plotDF() - xType()=='continuous'", name='all')
      dataset <- dataset[dataset[[x]] >= xlim()[1] & dataset[[x]] <= xlim()[2], ]
    }
    else if (xType()=='discrete'){
      flog.debug("dataset::plotDF() - xType()=='discrete'", name='all')
      dataset <- dataset[dataset[[x]] %in% xlim(), ]
    }
  }
  
  ## subset with ylim filter (if applicable)
  if (!is.null(ylim())) {
    flog.debug("dataset::plotDF() - !is.null(ylim())", name='all')
    if (isXYCtrlPlot()) {
      flog.debug("dataset::plotDF() - isXYCtrlPlot()", name='all')
      y <- y()
      if (is.null(y)){
        flog.debug("dataset::plotDF() - is.null(y) - End)", name='all')
        return()
      }
      if (is.null(yType())){
        flog.debug("dataset::plotDF() - is.null(yType()) - End)", name='all')
        return()
      }
      if (yType()=='continuous'){
        flog.debug("dataset::plotDF() - yType()=='continuous'", name='all')
        dataset <- dataset[dataset[[y]] >= ylim()[1] & dataset[[y]] <= ylim()[2], ]
      }
      else if (yType()=='discrete'){
        flog.debug("dataset::plotDF() - yType()=='discrete'", name='all')
        dataset <- dataset[dataset[[y]] %in% ylim(), ]
      }
    }
  }
  
  flog.debug("dataset::plotDF() - End", name='all')
  
  return(dataset)
})

