library(RSelenium)
library(dplyr)
library(data.table)

startServer <- function(dir = NULL, args = NULL, javaargs = NULL, log = TRUE,  ...) {
  library(XML)
  system('taskkill /f /im java.exe')
  system('taskkill /f /im phantomjs.exe')
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
  
  system('chcp 65001')  # for windows non-english encoding
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
  capture.output(driver$open(), file='NUL')
  driver$navigate(sprintf("http://127.0.0.1:%s/", port))
  stopifnot(driver$getTitle()[[1]] == 'ggraptR')
  driver
}

makePageVisible <- function(driver) {
  tmpFileName <- paste0(tempfile(), '.html')
  write(driver$getPageSource()[[1]], file = tmpFileName)
  browseURL(tmpFileName)
}

stopExternals <- function(driver, selServer) {
  driver$close()
  selServer$stop()
}

assert <- function (expr, msg) {
  if (!expr) stop(msg, call. = FALSE)
}

getEls <- function(source, query, how='xpath', directChildren=F) {
  if (length(query) > 1) query <- paste0(query, collapse='')
  res <- if (class(source) == 'remoteDriver') {
    source$findElements(how, query)
  } else if (class(source) == 'webElement') {
    stopifnot(!grepl('^[\\./]', query))  # not starts with . nor /
    source$findChildElements(how, paste0('./', if (!directChildren) '/', query))
  } else {
    stop()
  }
  if (!length(res)) warning('>> empty')
  res
}

getEl <- function(source, query, how='xpath', directChildren=F) {
  res <- getEls(source, query, how, directChildren)
  if (length(res) > 1) {
    print(html(res))
    stop(sprintf('\nElements found: %s', length(res)))
  }
  if (length(res)) res[[1]]
}

byClass <- function(className, tag='*') {
  sprintf('%s[contains(concat(" ",normalize-space(@class)," "), " %s ")]', tag,className)
}

html <- function(el) {
  if (is.list(el)) {
    unlist(sapply(el, function(x) x$getElementAttribute('outerHTML'))) 
  } else {
    el$getElementAttribute('outerHTML')[[1]]
  }
}

text <- function(el) {
  if (is.list(el)) {
    unlist(sapply(el, function(x) x$getElementAttribute('outerText'))) 
  } else {
    el$getElementAttribute('outerText')[[1]]
  }
}

click <- function(el) {
  stopifnot(class(el) == 'webElement')
  el$clickElement()
}

# 
# print (remDr$executeScript("return document.readyState;")[[1]])
# while (remDr$executeScript("return document.readyState;")[[1]]!= "complete"
# && totalwait<10) {
#   Sys.sleep(timeout)
# }
