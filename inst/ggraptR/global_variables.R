state <- sys.frames()[[1]]
stopifnot(!is.null(state$initialDf))
fileDefault <- state$fileDefault
