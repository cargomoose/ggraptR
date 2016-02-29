
## note: the functions below will find year (YYYY), month (YYYY-MM), 
## and date (YYYY-MM-DD) between 1800-01-01 and 2099-12-31

## this functions determines the name of the year column (in YYYY format)
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
  return(yearCol)
}


## this function determines the name of the month column (in YYYY-MM format)
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
  return(yearMonthCol)
}


## this function determines the name of the day column (in YYYY-MM-DD format)
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
  return(dateCol)
}


## this function grabs the names of factor variables
getFactorVarNames <- function(df) {
  factorVars <- colnames(df)[sapply(df, is.factor)]
  return(factorVars)
}


## this function grabs the names of numeric variables
getNumericVarNames <- function(df) {
  numericVars <- colnames(df)[sapply(df, is.numeric)]
  return(numericVars)
}


## this function grabs the names variables of whose number of unique values does not exceed 
## a specified threshold (LOE: less than or equal to)
getVarNamesUniqValsCntLOEN <- function(df, n=100) {
  vars <- colnames(df)[sapply(df, function(x) {length(unique(x)) <= n})]
  return(vars)
}


## this function gets all variable names of data frame objects that are loaded into memory
getLoadedDataFrameNames <- function(env=.GlobalEnv) {
  objNames <- ls(env)
  dfNames <- c()
  for (objName in objNames) {
    obj <- get(objName)
    if(class(obj)=='data.frame') {
      dfNames <- c(dfNames, objName)
    } 
  }
  return(dfNames)
}


## this function modifies and ensures proper variable name
## for semi-automatic aggregation dataset column names
ensureProperVarName <- function(colnames, var, aggMeth, semiAutoAggOn) {
  if (tolower(var) %in% c('none', '.')) return(var)
  
  ## only if original variable name is not found in dataset's column names
  if (!(var %in% colnames)) {
    ## if semi-automatic aggregation is turned on
    if (semiAutoAggOn) {
      if (aggMeth=='count')
        return('count')
      else
        return(paste0(var, '_', aggMeth))
    }
  } 

  ## if original variable name is found in dataset's column names
  else {
    varAgg <- paste0(var, '_', aggMeth)
    if (varAgg %in% colnames)
      return(varAgg)
  }
  
  return(var)
}




## function to convert 'None' to NULL
convertNoneToNULL <- function(var) {
  if (tolower(var)=='none') {var <- NULL}; return(var)
}


## 
varNameAsFactorOrNULL <- function(var) {
  if (!is.null(var)) 
    ret <- paste0('as.factor(', var, ')')
  else
    ret <- NULL
  return(ret)
}


## function to check if specified widgets are loaded on shiny UI
checkWidgetsLoaded <- function(input, widgets) {
  for (widget in widgets) {
    if (is.null(input[[widget]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}


## function for cleaning (removing duplicates or "None" values, etc.)
cleanPlotAggBy <- function(x, y, aggBy) {
  aggBy <- c(x, aggBy)
  aggBy <- unique(aggBy)
  nonAggBy <- c('None', 'none', '.')
  
  ## remove nonAggBy from aggBy
  aggBy <- setdiff(aggBy, nonAggBy)
  
  if (x != y)
    aggBy <- setdiff(aggBy, y)
  
  return(aggBy)
}


## this function takes two numeric ranges and returns TRUE if the two ranges overlap;
## it is used to ensure that numeric xlim range has been updated for new dataset and x variables
## when plot type is set to histogram (to prevent an error message)
checkTwoRangesOverlap <- function(range1, range2) {
  lowerRange1 <- range1[1]
  upperRange1 <- range1[2]
  lowerRange2 <- range2[1]
  upperRange2 <- range2[2]
  return(upperRange1 >= lowerRange2 & lowerRange1 <= upperRange2)
}


## this function ensures correct plot inputs for an updated dataset
ensureCorrectPlotInputs <- function(plotInputsList, colnames) {
  flog.debug("helper::ensureCorrectPlotInputs() - Begin", name='all')
  for (name in names(plotInputsList)) {
    if (!is.null(plotInputsList[[name]])) {
      if (any(name %in% c('x', 'y', 'facetRow', 'facetCol', 'facetWrap'))) {
        if (!(plotInputsList[[name]] %in% colnames)) {
          plotInputsList[name] <- list(NULL)
        }
      } else if (any(name %in% c('color', 'size', 'shape'))) {
        if (!(plotInputsList[[name]] %in% colnames)) {
          asFactorName <- paste0(name, 'AsFactor')
          plotInputsList[name] <- plotInputsList[asFactorName] <- list(NULL)
        }
      }
    }
  }
  flog.debug("helper::ensureCorrectPlotInputs() - End", name='all')
  return(plotInputsList)
}


## this function removes elements that are not part of a dataset's column variables
rmElemsNotInDatasetCols <- function(elems, dataset) {
  validElems <- elems[elems %in% colnames(dataset)]
  return(validElems)
}


## this function takes a dataset, variable name, and variable's limit (e.g. x and xlim)
## and returns TRUE if that they are compatible;
## for e.g. if x is a continuous variable, then xlim should be a numeric range;
## for e.g. if y is a factor or character variable, then ylim should be a vector of discrete values;
## MODIFY THIS FUNCTION FOR CASES WHEN VARIABLE IS LOGICAL!!!
# checkVarAndLimCompatible <- function(dataset, var, lim) {
#   varType <- class(dataset[[var]])
#   limType <- class(lim)
#   compatCond <- FALSE
#   
#   if (any(varType %in% 'numeric')) {
#     if (limType=='numeric') {
#       compatCond <- TRUE
#     }
#   } else if (any(varType %in% c('factor', 'character'))) {
#     if (limType=='character') {
#       compatCond <- TRUE
#     }
#   }
#   
#   return(compatCond)
# }



