# note: the functions below will find year (YYYY), month (YYYY-MM), 
# and date (YYYY-MM-DD) between 1800-01-01 and 2099-12-31
# this functions determines the name of the year column (in YYYY format)
getYearColumnName <- function(df) {
  firstRow <- df[1, ]
  yearPtrn <- '^(18|19|20)[0-9]{2}$'
  potentialYearCol <- colnames(df)[grepl(yearPtrn, firstRow)]
  yearCol <- c()
  for (col in potentialYearCol) {
    if (all(grepl(yearPtrn, df[[col]]))) {
      yearCol <- c(yearCol, col)
    }
  }
  yearCol
}

# determines the name of the month column (in YYYY-MM format)
getMonthColumnName <- function(df) {
  firstRow <- df[1, ]
  yearMonthPtrn <- '^(18|19|20)[0-9]{2}[- /.](0[1-9]|1[012])$'
  potentialYearMonthCol <- colnames(df)[grepl(yearMonthPtrn, firstRow)]
  yearMonthCol <- c()
  for (col in potentialYearMonthCol) {
    if (all(grepl(yearMonthPtrn, df[[col]]))) {
      yearMonthCol <- c(yearMonthCol, col)
    }
  }
  yearMonthCol
}

# determines the name of the day column (in YYYY-MM-DD format)
getDateColumnName <- function(df) {
  firstRow <- as.character(df[1, ])
  datePtrn <- '^(18|19|20)[0-9]{2}[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$'
  potentialDateCol <- colnames(df)[grepl(datePtrn, firstRow)]
  dateCol <- c()  
  for (col in potentialDateCol) {
    if (all(grepl(datePtrn, as.character(df[[col]])))) {
      dateCol <- c(dateCol, col)
    }
  }
  dateCol
}

# gets all variable names of data frame objects that are loaded into memory
getPreloadedEnvDfNames <- function(env=.GlobalEnv) {
  objNames <- ls(env)
  dfNames <- c()
  for (objName in objNames) {
    obj <- get(objName)
    if (any(class(obj)=='data.frame')) {
      dfNames <- c(dfNames, objName)
    }
  }
  dfNames
}

# function for cleaning (removing duplicates or "None" values, etc.)
cleanPlotAggBy <- function(x, y, aggBy) {
  aggBy <- c(x, aggBy)
  aggBy <- unique(aggBy)
  nonAggBy <- c('None', 'none', '.')
  
  # remove nonAggBy from aggBy
  aggBy <- setdiff(aggBy, nonAggBy)
  
  if (x != y)
    aggBy <- setdiff(aggBy, y)
  
  aggBy
}

# ensures correct plot inputs for an updated dataset
ensureCorrectPlotInputs <- function(plotInputsList, colnames) {
  for (name in names(plotInputsList)) {
    if (!is.null(plotInputsList[[name]])) {
      if (name %in% c('x', 'y', 'facetRow', 'facetCol', 'facetWrap') &&
          !(plotInputsList[[name]] %in% colnames)) { 
        # problem here can be produced by staled aggLimDf() %>% names
        # warning('Incorrect plot input has been nulled: ', name)
        plotInputsList[name] <- list(NULL)
      } else if (name %in% c('color', 'size', 'shape') &&
                 !(plotInputsList[[name]] %in% colnames)) {
        asFactorName <- paste0(name, 'AsFactor')
        plotInputsList[name] <- plotInputsList[asFactorName] <- list(NULL)
      }
    }
  }
  plotInputsList
}

# modifies and ensures proper variable name
# for semi-automatic aggregation dataset column names
ensureProperVarName <- function(colnames, var, aggMeth, semiAutoAggOn) {
  if (anyNull(colnames, var, aggMeth, semiAutoAggOn)) return()
  if (tolower(var) %in% c('none', '.')) return(var)
  
  # only if original variable name is not found in dataset's column names
  if (!(var %in% colnames)) {
    # if semi-automatic aggregation is turned on
    if (semiAutoAggOn) {
      return(if (aggMeth=='count') 'count' else paste0(var, '_', aggMeth))
    }
  } else {  # if original variable name is found in dataset's column names
    varAgg <- paste0(var, '_', aggMeth)
    if (varAgg %in% colnames) {
      return(varAgg)
    }
  }
  var
}


# functions for plotType options
# makes structured list like one in globals.R simple flat list with vectors like elements
flattenList <- function(lst) {
  res <- list()
  for (lsti in 1:length(lst)) {
    el <- lst[lsti]
    if (is.list(el[[1]])) {
      i <- (1:length(el[[1]])) + length(res)
      res[i] <- el[[1]]
      names(res)[i] <- names(el[[1]])
    } else {
      i <- length(res) + 1
      res[i] <- el
      names(res)[i] <- names(el)
    }
  }
  res
}

# preserves list structure and extracts only list elements names
getStructListNames <- function(lst) {
  if (is.null(lst)) return()
  res <- list()
  for (i in 1:length(lst)) {
    el <- lst[i]
    if (is.list(el[[1]])) {
      res[[length(res) + 1]] <- c(names(el[[1]]))
    } else {
      res[[i]] <- names(el)
    }
  }
  res
}

getPlotTypeOpts <- function(selectedOpts, n_num, n_cat) {
  defInputs <- getDefinedPlotInputs(n_num, n_cat)
  if (is.null(defInputs)) return()
  plotTypeRelations <- getStructListNames(defInputs)
  opts <- unlist(plotTypeRelations[
    if (is.null(selectedOpts)) T else {
      sapply(plotTypeRelations, function(el) unlist(selectedOpts)[1] %in% el)
    }])
  names(opts) <- sapply(opts, function(x) capitalize(x) %>% gsub('(\\d)', ' \\1', .))
  opts
}


needCatX <- function(plotTypes) {
  any(c('violin', 'box', 'bar') %in% plotTypes)
}

# override GGally:::print.ggmatrix to prevent messages about binwdth
print.ggmatrix <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  suppressMessages(GGally:::print.ggmatrix(x, newpage, vp, list(...)))
}

sourceAllInDir <- function(folder, local=F, except=NULL) {
  for (fl in dir(folder)) {
    if (!fl %in% except) {
      # 'envir' to set up environment - server() call - which contains 'output' variable
      do.call(source, list(paste(folder, fl, sep = '/'), local), envir = sys.frame(-1))
    }
  }
}


# common functions
getIsFactorVarNames <- function(df) {
  colnames(df)[sapply(df, is.factor)]
}

getIsNumericVarNames <- function(df) {
  colnames(df)[sapply(df, is.numeric)]
}

# grabs the names variables of whose number of unique values does not exceed 
# a specified threshold (LOE: less than or equal to)
getVarNamesUniqValsCntLOEN <- function(df, nCatUniqVals) {
  colnames(df)[sapply(df, function(x) length(unique(x)) <= nCatUniqVals)]
}

convertNoneToNULL <- function(var) {
  if (!is.null(var) && tolower(var) != 'none' && nchar(var)) var  # otherwise NULL
}

asFactor <- function(var) {
  if (!is.null(var)) paste0('as.factor(', var, ')')
}

notNulls <- function(...) {  # effective lazy implementation
  for (el in list(...)) if (is.null(el)) return(FALSE)
  TRUE
}

anyNull <- function(...) {
  !notNulls(...)
}

not_null_true <- function(x) !is.null(x) && x
not_null_false <- function(x) !is.null(x) && !x

na_omit <- function(lst) Filter(function(x) !is.null(x) && length(x), lst)

trimList <- function(...) na_omit(list(...))

applied_filters_expr <- function(df, df_name, filter_keys, filter_vals) {
  stopifnot(notNulls(lst, filter_keys, filter_vals),
            length(lst) > 0, length(filter_keys) > 0,
            length(filter_keys) == length(filter_vals))
  
  filter_mask_txt <- sapply(1:length(filter_keys), function(i) {
    col_name <- filter_keys[i]
    vals <- filter_vals[[i]]
    is_num <- is.numeric(df[[col_name]])
    
    if ((is_num && length(vals) != 2) || length(vals) == 0) {
      stop('df_name: ', df_name, 'col_name: ', col_name,
           'is_num: ', is_num, 'length(vals): ', length(vals))
    }
    
    filter_cond <- paste0(vals, collapse=', ')
    sprintf(if (is_num) 'between(%s, %s)' else '%s %%in%% c(%s)', 
            col_name, filter_cond)
  })
  
  sprintf('%s[with(%s, %s), ]', 
          df_name, df_name, paste(filter_mask_txt, collapse = ' & '))
}

# 'foo' -> 'Foo'
capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

do.call.pasted <- function(..., args=list()) {
  do.call(paste(trimList(...), collapse=''), args, envir=parent.env(parent.frame()))
}

getFirstNonNull <- function(...) {
  for (el in list(...)) if (!is.null(el)) return(el)
  stop()
}

getInitialArg <- function(argName) {
  # a workaround for rsconnect::deployApp('inst/ggraptR')
  ggraptrFormals <- try(names(formals(eval(parse(text="ggraptR::ggraptR")))), silent = T)
  if (class(ggraptrFormals) == 'try-error') return(NULL)
  ggraptrFormals <- setdiff(ggraptrFormals, '...')

  envIds <- which(
    sapply(0:length(sys.frames()), function(ienv) 
      !length(setdiff(ggraptrFormals, ls(envir=sys.frame(ienv)))))) - 1
  
  tryCatch(get(argName, envir=sys.frame(envIds[1])), error=function(e){})
}
