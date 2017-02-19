dbUploadedDf <- reactive({
  eval_btn <- input$dbExecuteBtn  # trigger
  
  isolate({
    driver_name <- input$dbDriverTypeCtrl
    if (is.null(driver_name) || !eval_btn) return()
    package_for_driver <- paste0('R', driver_name)
    if (!require(package_for_driver, character.only = T)) {
      install.packages(package_for_driver)
      library(package_for_driver, character.only = T)
    }
    
    # con <- dbConnect(dbDriver('MySQL'), user='gray', host='db4free.net',
    #                  password='12348888', dbname='ggtest', port=3307)
    
    con <- dbConnect(dbDriver(driver_name), 
                     host=input$dbHost, port=as.integer(input$dbPort),
                     user=input$dbUser, password=input$dbPass, 
                     dbname=input$dbName)
    
    query <- input$dbSqlQuery
    
    df <- dbGetQuery(con, query)
    df <- df[, setdiff(names(df), 'row_names')]
    dbDisconnect(con)
    
    reactVals$is_db_upload <- T
  })
  
  df
})

# reactive variable for custom (uploaded) dataset
uploadedDf <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  input$file  # trigger
  db_df <- dbUploadedDf()
  
  isolate({
    if (not_null_true(reactVals$is_db_upload)) return(db_df)
    if (anyNull(input$file, input$header, input$sep, input$quote)) return()
    
    read.csv(input$file$datapath, header = as.logical(input$header),
             sep = input$sep, quote = input$quote)
  })
})

# reactive variable for custom dataset name
uploadedDfName <- reactive({
  if (is.null(uploadedDf())) return()
  
  isolate({
    if (not_null_true(reactVals$is_db_upload)) {
      reactVals$is_db_upload <- NULL
      
      df_name <- input$dbSqlQuery %>% 
        gsub(' +', ' ',.) %>% 
        stringr::str_extract('(?i)(?<=from )\\S+')
      
      if (is.na(df_name)) 'db_uploaded' else df_name
    } else {
      input$file$name
    }
  })
})


# reactive variable for raw dataset names
rawDatasetNames <- reactive({
  unique(c(uploadedDfName(), getPreloadedEnvDfNames(), getDefaultPlots()))
})


# reactive variable for raw dataset
rawDataset <- reactive({
  cur_name <- datasetName()  # trigger
  if (is.null(cur_name)) return()  # for initial input$datasetName
  
  isolate({
    reactVals$plotState$dataset_name <- cur_name
    df <- if (!is.null(uploadedDfName()) && cur_name == uploadedDfName()) 
      uploadedDf() else get(cur_name)
    data.frame(lapply(df, function(x) if (is.character(x)) as.factor(x) else x))
  })
})

# manually aggregated dataset
manAggDataset <- reactive({
  # if all fields for manual aggregation are filled in
  if (notNulls(input$aggBy, input$aggTarget, input$aggMeth)) {
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
  }
  
  isolate({
    if (all(is.null(xlim()), is.null(ylim()))) {
      reactVals$plotState$lim_range <- NULL
    } else {
      reactVals$plotState$lim_range$x$val <- xlim()
      reactVals$plotState$lim_range$x$type <- xType()
      reactVals$plotState$lim_range$y$val <- ylim()
      reactVals$plotState$lim_range$y$type <- yType()
    }
  })
  
  dataset
})

