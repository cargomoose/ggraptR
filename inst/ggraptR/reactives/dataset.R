# reactive variable for custom (uploadable) dataset file info
uploadedDfFileInfo <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  input$file
})

# reactive variable for custom (uploaded) dataset
uploadedDf <- reactive({
  fileInfo <- uploadedDfFileInfo()
  if (is.null(fileInfo) || is.null(input$header) || is.null(input$sep) 
      || is.null(input$quote)) return()
  
  read.csv(fileInfo$datapath, header = as.logical(input$header),
           sep = input$sep, quote = input$quote)
})

# reactive variable for custom dataset name
uploadedDfName <- reactive({
  uploadedDfFileInfo()$name
})

# reactive variable for raw dataset names
rawDatasetNames <- reactive({
  unique(c("diamonds", "mtcars", "rock", "iris", "esoph",
           uploadedDfName(), getPreloadedEnvDfNames()))
})

# reactive variable for raw dataset
rawDataset <- reactive({
  # from dataset selection drop-down
  if (is.null(input$dataset)) return()
  
  isolate(reactVals$plotState$dataset_name <- input$dataset)
  
  # if no custom dataset was uploaded, then set one of the preloaded datasets as raw
  if (is.null(input$file)) {
    get(input$dataset)
  } else { # if custom dataset was uploaded
    # if custom dataset was selected, then set it as raw dataset
    if (input$dataset == uploadedDfName()) {
      uploadedDf()      
    } else {
      # if custom dataset was not selected, then set one of the preloaded datasets as raw
      get(input$dataset)
    }
  }
})

# manually aggregated dataset
manAggDataset <- reactive({
  # if all fields for manual aggregation are filled in
  if (!is.null(input$aggBy) && !is.null(input$aggTarget) && !is.null(input$aggMeth)) {
    # return manually aggregated dataset
    aggregate(rawDataset(), input$aggBy, input$aggTarget, input$aggMeth)
  } else {  # return raw dataset  
    rawDataset()
  }
})

# raw or aggregated dataset
dataset <- reactive({
  #if (is.null(input$rawVsManAgg)) return()
  if (is.null(input$rawVsManAgg)) {
    return(rawDataset())
  }

  # raw dataset
  if (input$rawVsManAgg == 'raw') {
    rawDataset()
  } else if (input$rawVsManAgg == 'manAgg') {  # aggregated dataset
    manAggDataset()
  }
})


# reactive for base semi-automatic aggregate-by fields 
# (that are NOT in the "additional aggregate-by" fields)
plotSemiAutoAggByBase <- reactive({
  aggBy <- c(input$x, input$color, input$size, input$shape, input$fill, input$facetRow, 
             input$facetCol, input$facetWrap)
  cleanPlotAggBy(input$x, input$y, aggBy)
})

# reactive for semi-automatic aggregate by
# base + additional aggregate-by fields
plotSemiAutoAggBy <- reactive({
  if (is.null(dataset())) return()
  aggBy <- c(plotSemiAutoAggByBase(), plotAddAggBy())
  aggBy <- cleanPlotAggBy(input$x, input$y, aggBy)
  aggBy[aggBy %in% colnames(dataset())]
})

# reactive for semi-automatic aggregated dataset
semiAutoAggDF <- reactive({
  if (is.null(semiAutoAggOn()) || is.null(dataset())) return()
  
  # if plot aggregation is specified (e.g. sum, mean, max, min)  
  if (semiAutoAggOn()) {
    if (is.null(plotSemiAutoAggBy())) {
      return()
    }
    
    aggBy <- plotSemiAutoAggBy()
    aggTarget <- input$y
    aggMeth <- input$plotAggMeth
    
    vars <- c(aggBy, aggTarget)
    if (all(vars %in% colnames(dataset()))) {
      aggregate(dataset(), aggBy=aggBy, aggTarget=input$y, aggMeth=input$plotAggMeth)
    }
  } 
})

# reactive variable for final dataset
aggDf <- reactive({
  if (is.null(dataset())) return()
  if (is.null(semiAutoAggOn())) {
    return(dataset())
  }
  
  # semi-automatic aggregation (if enabled)
  if (semiAutoAggOn()) {
    semiAutoAggDF()
  } else{  # natural dataset (raw or manually aggregated dataset)
    dataset()
  }
})


# reactive dataset used for plotting 
# (filtered version of aggDf(), using xlim and ylim)
aggLimDf <- reactive({
  dataset <- aggDf()
  if (is.null(dataset)) return()
  
  # subset with xlim filter (if applicable)
  if (!is.null(xlim())) {
    x <- input$x
    if (is.null(x) || is.null(xType())) {
      return()
    }
    if (xType() == 'continuous') {
      dataset <- dataset[dataset[[x]] >= xlim()[1] & dataset[[x]] <= xlim()[2], ]
    } else if (xType() == 'discrete') {
      dataset <- dataset[dataset[[x]] %in% xlim(), ]
    }
    isolate({
      reactVals$plotState$lim_range$x$val <- xlim()
      reactVals$plotState$lim_range$x$type <- xType()
    })
  }
  
  # subset with ylim filter (if applicable)
  if (!is.null(ylim())) {
    flog.debug("dataset::aggLimDf() - !is.null(ylim())", name='all')
    if ('y' %in% plotInputs()) {
      y <- y()
      if (is.null(y) || is.null(yType())) {
        return()
      }
      if (yType() == 'continuous') {
        dataset <- dataset[dataset[[y]] >= ylim()[1] & dataset[[y]] <= ylim()[2], ]
      } else if (yType() == 'discrete') {
        dataset <- dataset[dataset[[y]] %in% ylim(), ]
      }
    }
    isolate({
      reactVals$plotState$lim_range$y$val <- ylim()
      reactVals$plotState$lim_range$y$type <- yType()
    })
  }
  dataset
})

