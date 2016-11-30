# from find.package('RSelenium')/examples/serverUtils/*.R
startServer <- function(dir = NULL, args = NULL, javaargs = NULL, log = TRUE,  ...) {
  library(XML)
  suppressWarnings(system('taskkill /f /im java.exe', show.output.on.console = F))
  suppressWarnings(system('taskkill /f /im phantomjs.exe', show.output.on.console = F))
  selDIR <-  ifelse(is.null(dir), file.path(find.package("RSelenium"), "bin"), dir)
  selFILE <- file.path(selDIR, "selenium-server-standalone.jar")
  if (!file.exists(selFILE)) {
    possFiles <- list.files(selDIR, "selenium-server-standalone")
    if (length(possFiles) == 0) {
      stop("No Selenium Server binary exists. Run checkForServer or start 
           server manually.")
    }
    # pick most recent driver
    selFILE <- possFiles[order(gsub(".*-(.*).jar$", "\\1", possFiles), 
                               decreasing = TRUE)][1]
    selFILE <- file.path(selDIR, selFILE)
    }
  
  logFILE <- file.path(selDIR, "sellog.txt")
  selArgs <- c(paste("-jar", shQuote(selFILE)))
  if (log) {
    write("", logFILE)
    selArgs <- c(selArgs, paste("-log", shQuote(logFILE)))
  }
  
  selArgs <- c(javaargs, selArgs, args)
  userArgs <- list(...)
  if (.Platform$OS.type == "unix") {
    initArgs <- list(command = "java", args = selArgs, wait = FALSE, 
                     stdout = FALSE, stderr = FALSE)
  } else {
    initArgs <- list(command = "java",args = selArgs, wait = FALSE, 
                     invisible = TRUE)
  }
  initArgs[names(userArgs)] <- userArgs 
  do.call(system2, initArgs)
  
  if (.Platform$OS.type == "windows") {
    wmicOut <- tryCatch({
      system2("wmic", args = c("path win32_process get Caption,Processid,Commandline",
                               "/format:htable"), stdout=TRUE, stderr=NULL)
    }, error = function(e)e)
    
    selPID <- if (inherits(wmicOut, "error")) {
      wmicArgs <- paste0(c("path win32_process where \"commandline like '%",
                           selFILE, "%'\" get Processid"))
      wmicOut <- system2("wmic", args = wmicArgs, stdout = TRUE)
      as.integer(gsub("\r", "", wmicOut[2]))
    } else {
      wmicOut <- readHTMLTable(htmlParse(wmicOut), header = TRUE, 
                               stringsAsFactors = FALSE)[[1]]
      wmicOut[["ProcessId"]] <- as.integer(wmicOut[["ProcessId"]])
      idx <- grepl(selFILE, wmicOut$CommandLine)
      if (!any(idx)) stop("Selenium binary error: Unable to start Selenium 
                          binary. Check if java is installed.")
      wmicOut[idx,"ProcessId"]
    }
  } else {
    if (Sys.info()["sysname"] == "Darwin") {
      sPids <- system('ps -Ao"pid"', intern = TRUE)
      sArgs <- system('ps -Ao"args"', intern = TRUE)
    } else {
      sPids <- system('ps -Ao"%p"', intern = TRUE)
      sArgs <- system('ps -Ao"%a"', intern = TRUE)
    }
    idx <- grepl(selFILE, sArgs)
    if (!any(idx)) stop("Selenium binary error: Unable to start Selenium 
                        binary. Check if java is installed.")
    selPID <- as.integer(sPids[idx])
  }
  
  system('chcp 65001', show.output.on.console = F)  # for windows non-english encoding
  system('tasklist /fi "imagename eq java.exe"')
  # system('taskkill /f /pid 4668')
  
  list(
    stop = function() tools::pskill(selPID),
    getPID = function() return(selPID))
}

getDriver <- function(port=6012) {
  driver <- remoteDriver(
    browserName = "phantomjs", extraCapabilities = 
      list(phantomjs.binary.path = paste0(getwd(), "/auto/resources/phantomjs.exe")))
  driver$open(silent = T)  # == capture.output(driver$open(), file='NUL')
  driver$navigate(sprintf("http://127.0.0.1:%s/", port))
  driver$setWindowSize(1920, 1080)
  # stopifnot(driver$getTitle()[[1]] == 'ggraptR')
  driver
}

openPageInBrowser <- function(driver) {
  tmpFileName <- paste0(tempfile(), '.html')
  write(driver$getPageSource()[[1]], file = tmpFileName)
  browseURL(tmpFileName)
}

stopExternals <- function(driver, selServer) {
  driver$close()
  selServer$stop()
}

getEls <- function(source, query, directChildren=F) {
  if (length(query) > 1) query <- paste0(query, collapse='')
  #grepl('#|\\.\\w|>',query)
  how <- if (grepl('/|@|\\.\\W', query)) 'xpath' else  'css selector'
  res <- if (class(source) == 'remoteDriver') {
    source$findElements(how, query)
  } else if (class(source) == 'webElement') {
    if (how == 'xpath') {
      stopifnot(!grepl('^[\\./]', query))  # starts with neither . nor /
      query <- paste0('./', if (!directChildren) '/', query)
    }
    source$findChildElements(how, query)
  } else {
    stop()
  }
  # if (!length(res)) warning('>> empty')
  res
}

getEl <- function(source, query, directChildren=F) {
  res <- getEls(source, query, directChildren)
  if (length(res) > 1) {
    print(html(res))
    stop(sprintf('\nElements found: %s', length(res)))
  }
  if (length(res)) res[[1]]
}

html <- function(el) {
  stopifnot(!is.null(el))
  if (is.list(el)) {
    unlist(sapply(el, function(x) x$getElementAttribute('outerHTML'))) 
  } else {
    el$getElementAttribute('outerHTML')[[1]]
  }
}

text <- function(el) {
  stopifnot(!is.null(el))
  if (is.list(el)) {
    unlist(sapply(el, function(x) x$getElementAttribute('outerText'))) 
  } else {
    el$getElementAttribute('outerText')[[1]]
  }
}

attr <- function(el, attrName) {
  if (!class(el) == 'webElement') stop('Wrong input class: ', class(el))
  res <- el$getElementAttribute(attrName)
  if (length(res) == 1) res[[1]] else res
}

click <- function(el) {
  stopifnot(class(el) == 'webElement')
  el$clickElement()
}

waitFor <- function(target, source=driver, timeout=10) {
  nChecks <- 2 * timeout
  durCheck <- timeout / nChecks
  targetFun <- if (is.function(target)) target else 
    if (is.character(target)) function() sapply(target, function(x) getEls(source, x)) else
      if (is.call(target)) function() eval(target) else
        stop(sprintf('Not implemented for target class [%s]', class(target)))
  for (i in 1:nChecks) {
    res <- suppressWarnings(targetFun())
    if (is.list(res)) {
      if (length(target) == 1) {
        if (length(res)) {
          return(invisible(if (length(res) > 1) res else res[[1]]))
        }
      } else {
        if (length(Filter(length, res)) == 1) {
          res <- Filter(length, res)[[1]]
          return(invisible(if (length(res) > 1) res else res[[1]]))
        }
      }
    } else if (is.logical(res) && res) {
      return(res)
    }
    Sys.sleep(durCheck)
  }
  driver$screenshot(T)
  stop('Could not wait')
}
