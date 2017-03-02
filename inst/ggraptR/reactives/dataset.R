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
    # to add all dwnload input names at once
    # prevents premature closing of the modal window
    if (sum(c('dbExecuteBtn', 'file') %in% names(input)) == 1) {
      updateTabsetPanel(session, 'addDatasetTabset', 'addFileTab')
    }
    
    close_modal_if_opened <- function() {
      if (input$modalUploadOptions) {
        toggleModal(session, "modalUploadOptions", toggle = "close")
      }
    }
    
    if (not_null_true(reactVals$is_db_upload)) {
      close_modal_if_opened()
      return(db_df)
    }
    if (anyNull(input$file, input$header, input$sep, input$quote)) return()
    
    close_modal_if_opened()
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

# filtered version of aggDf() for plotting
# plotAggMeth() -> semiAutoAggOn() -> aggDf() -> .
aggLimDf <- reactive({
  df <- aggDf()
  search_columns <- input$displayTable_search_columns  # input$displayTable_rows_all
  if (is.null(df)) return()
  
  isolate({
    fill_idxs <- if (!is.null(search_columns))
      1:length(search_columns) %>% Filter(function(i) search_columns[i] != '',.) else c()
    if (!length(fill_idxs)) {
      reactVals$plotState$filter <- NULL
      return(df)
    }
    if (tolower(plotAggMeth()) != 'none') {
      stop('Can not choose between aggregated and limited view')
    }
    
    filter_keys <- names(isolate(manAggDataset()))[fill_idxs]
    # search_columns[fill_idxs] ex: "[\"25-34\",\"35-44\"]" "18.18 ... 60.00"
    filter_vals <- lapply(search_columns[fill_idxs], function(x) 
      (if (startsWith(x, '[')) strsplit(substring(x, 2, nchar(x) - 1), ',') else 
        strsplit(x, ' \\.{3} ')) %>% `[[`(1))
    
    reactVals$plotState$filter$keys <- filter_keys
    reactVals$plotState$filter$vals <- filter_vals
    
    df_expr <- applied_filters_expr(df, datasetName(), filter_keys, filter_vals)
    res <- eval(parse(text=df_expr))
    if (!setequal(colnames(df), colnames(res))) {
      stop('Looks like an aggregated and limited view collision')
    }
    res
  })
})

