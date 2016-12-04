# from find.package('RSelenium')/examples/serverUtils/*.R

startSelServer <- function() {
  library(XML)
  suppressWarnings(system('taskkill /f /im java.exe', show.output.on.console = F))
  suppressWarnings(system('taskkill /f /im phantomjs.exe', show.output.on.console = F))
  source(paste0(find.package('RSelenium'), '/examples/serverUtils/startServer.R'))
  
  for (i in 1:2) {
    res <- tryCatch(startServer(), 
                    error=function(e) grepl('Run checkForServer', e$message))
    if (is.logical(res) && res) {
      source(paste0(find.package('RSelenium'), '/examples/serverUtils/checkForServer.R'))
      checkForServer()
    } else {
      return(res)
    }
  }
  stop()
  # system('chcp 65001', show.output.on.console = F)  # for windows non-english encoding
  # system('tasklist /fi "imagename eq java.exe"')
  # system('taskkill /f /pid 4668')
}

getDriver <- function(port=6012) {
  phantomJsFile <- paste0(getwd(), "/auto/resources/phantomjs.exe")
  if (!file.exists(phantomJsFile)) {
    stop('Please download the latest version of phantomjs.exe from 
         http://phantomjs.org/download.html to /auto/resources/')
  }
  
  driver <- remoteDriver(
    browserName = "phantomjs", extraCapabilities = 
      list(phantomjs.binary.path = phantomJsFile))
  driver$open(silent = T)  # == capture.output(driver$open(), file='NUL')
  driver$navigate(sprintf("http://127.0.0.1:%s/", port))
  driver$setWindowSize(1920, 1080)
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

attr <- function(el, attrName) {
  if (!length(el)) return(el)
  if (class(if (is.list(el)) el[[1]] else el) != 'webElement') {
    stop('Wrong input class: ', class(el))
  }
  if (!is.list(el)) el <- list(el)
  
  unlist(lapply(el, function(x) {
    res <- x$getElementAttribute(attrName)
    if (length(res) == 1) res[[1]] else if (length(res) > 1) res
  }))
}

html <- function(el) attr(el, 'outerHTML')

text <- function(el) attr(el, 'outerText')

click <- function(el) {
  if (class(el) != 'webElement') stop('Input element class: ', class(el))
  el$clickElement()
}

filterElByAttr <- function(els, attrKey, attrVal) {
  stopifnot(is.list(els))
  res <- Filter(function(x) attr(x, attrKey) == attrVal, els)
  stopifnot(length(res) == 1)
  res[[1]]
}


waitFor <- function(target, source=driver, timeout=10, errorIfNot=T) {
  nChecks <- 2 * timeout
  oneWaitDur <- timeout / nChecks
  
  targetFun <- 
    if (is.function(target)) {
      target 
    } else if (is.character(target)) {
      function() unlist(lapply(target, function(x) getEls(source, x)))
    } else if (is.call(target)) {
      function() eval.in.any.env(target)
    } else {
      stop(sprintf('Not implemented for target class [%s]', class(target)))
    }
  
  for (i in 1:nChecks) {
    res <- suppressWarnings(targetFun())  # , silent = T)
    # if (is.error(res)) browser()
    # prevElHtml <- driver %>% getEl('#plotTypesCtrl .selectize-input') %>% html
    
    if (is.list(res)) {
      if (length(target) == 1) {
        if (length(res)) {
          return(invisible(if (length(res) > 1) res else res[[1]]))
        }
      } else {
        if (length(Filter(length, res)) == 1) {
          return(Filter(length, res)[[1]])
        }
      }
    } else if (is.logical(res) && res) {
      return(res)
    }
    Sys.sleep(oneWaitDur)
  }
  if (errorIfNot) stop('Could not wait') else F
}
