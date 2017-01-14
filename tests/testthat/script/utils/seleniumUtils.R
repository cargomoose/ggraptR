EXTERN_LOG_NAME <- 'rcmd.log'

# target - charater of css/xpath query, function returning T/F, expression like {a+2==5}
# source - webDriver or webElement
waitFor <- function(target, source=driver, timeout=10, errorIfNot=T, catchStale=F) {
  nChecks <- 2 * timeout
  oneWaitDur <- timeout / nChecks
  
  targetFun <- 
    if (class(substitute(target)) == '{') {  # if quoted expression
      call_obj <- as.call(substitute(target))
      function(placeholder) eval.in.any.env(call_obj)
    } else if (is.function(target)) {
      target
    } else if (is.character(target)) {
      function(x) unlist(lapply(target, function(el) getEls(x, el)))
    } else {
      stop(sprintf('Not implemented for target class [%s]', class(target)))
    }
  
  for (i in 1:nChecks) {
    res <- suppressMessages(tryCatch(
      targetFun(source),
      error=function(e) {
        if (is_unknown_exception(e) || (catchStale && isStaleException(e))) {
          FALSE
        } else {
          browser()
          stop(e$message)
        }
      }))
    
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
      return(TRUE)
    }
    Sys.sleep(oneWaitDur)
  }
  
  if (errorIfNot) {
    browser()
    stop('Could not wait')
  } 
  FALSE
}

# from find.package('RSelenium')/examples/serverUtils/*.R
startSelServer <- function() {
  library(XML)
  if (Sys.info()['sysname'] == 'Windows') {
    suppressWarnings(system('taskkill /f /im java.exe', show.output.on.console = F))
    suppressWarnings(system('taskkill /f /im phantomjs.exe', show.output.on.console = F))
  }
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
  # system('chcp 65001')  # for windows non-english encoding
  # system('tasklist /fi "imagename eq java.exe"')
  # system('taskkill /f /pid 4668')
}

getDriver <- function(url='http://127.0.0.1', port=6012) {
  phantomJsFile <- paste0(find.package('RSelenium'), '/bin/phantomjs',
                          if (Sys.info()['sysname'] == 'Windows') '.exe' else '')
  if (!file.exists(phantomJsFile)) {
    stop('Please download the latest version of phantomjs executable from 
          http://phantomjs.org/download.html to [', dirname(phantomJsFile), ']')
  }
  
  driver <- remoteDriver(
    browserName = "phantomjs", extraCapabilities = 
      list(phantomjs.binary.path = phantomJsFile))
  driver$open(silent = T)  # == capture.output(driver$open(), file='NUL')
  driver$navigate(paste0(url, if (!is.null(port)) paste0(':', port) else ''))
  driver$setWindowSize(1920, 1080)
  driver
}

openPageInBrowser <- function(driver) {
  tmpFileName <- paste0(tempfile(), '.html')
  write(driver$getPageSource()[[1]], file = tmpFileName)
  browseURL(tmpFileName)
}

as_string <- function(x) {
  lst <- Map(function(el) if (is.null(el)) 'NULL' else el, 
             if (is.list(x)) x else list(x))  # list(x) prevents coertions
  
  1:length(lst) %>% 
    vapply(function(i) {
      keys <- names(lst)
      val <- if (is.character(lst[[i]])) sprintf("'%s'", lst[[i]]) else lst[[i]]
      if (is.null(keys) || keys[i] == '') val else paste(keys[i], val, sep='=')
    }, FUN.VALUE='') %>% 
    paste(collapse=', ')
}
  
run_external_ggraptR <- function(...) {
  ggraptrArgsLst <- Filter(function(el) !is.null(el), 
                           if (length(list(...)) == 0 || # when '...' is not passed
                               !is.list(...)) list(...) else list(...)[[1]])
  if (!'port' %in% names(ggraptrArgsLst)) ggraptrArgsLst$port <- 5050
  ggraptrArgsLst$launch.browser <- F
  lst <- ggraptrArgsLst
  ggArgsStr <- as_string(lst)
  
  cmds <- c('Sys.getpid()',
            'suppressPackageStartupMessages(library(ggraptR))',
            sprintf('suppressPackageStartupMessages(ggraptR(%s))', ggArgsStr))
  
  # pipe does not like ';' in "R -e .." that's why created generate_r_cmd() exists
  # selPipe <- pipe(generate_r_cmd(cmds), open='r')  # system(cmd, wait=F)
  # selPid <- gsub('\\[1\\] ', '', readLines(selPipe, 2)[2])
  system(generate_r_cmd(cmds, EXTERN_LOG_NAME), wait=F)
  selServer <- startSelServer()
  driver <- getDriver(port=ggraptrArgsLst$port)
  
  if (driver$getTitle()[[1]] != 'ggraptR') {
    # if hangs before the next message [>5 sec] close the process manually
    # cat('\nTrying to check the reason why [driver$getTitle()[[1]] != "ggraptR"]', fill=T)
    # errMsg <- head(suppressWarnings(system(generate_r_cmd(cmds), intern=T)), -2)
    errMsg <- readLines(EXTERN_LOG_NAME) %>% head(-1) %>% tail(-3) %>% paste(collapse='\n')
    stop_externals(paste('>>', errMsg))
  }
  
  list(driver=driver, selServer=selServer)  # selPipe=selPipe, selPid=selPid
}

killExternalRprocess <- function(silent=T) {
  try({
    selPid <- gsub('\\[1\\] ', '', readLines(EXTERN_LOG_NAME, 2)[2])
    suppressWarnings(system(paste('taskkill /f /pid', selPid), show.output = F))
  }, silent = silent)
}

release_externals <- function() {
  eval.in.any.env({ driver$close(); suppressWarnings(rm(driver)) })
  eval.in.any.env({ selServer$stop(); suppressWarnings(rm(selServer)) })
  killExternalRprocess()
  closeAllConnections()
  if (nrow(showConnections())) stop('Can not close all connections')
}

stop_externals <- function(msgForError=NULL) {
  release_externals()
  stop(if (is.null(msgForError)) '' else msgForError, '\n')
}

getEls <- function(source, query, directChildren=F) {
  if (length(query) > 1) query <- paste0(query, collapse='')
  #grepl('#|\\.\\w|>',query)
  how <- if (grepl('/|@|\\.\\W', query)) 'xpath' else 'css selector'
  res <- if (class(source) == 'remoteDriver') {
    source$findElements(how, query)
  } else if (class(source) == 'webElement') {
    if (how == 'xpath') {
      if (grepl('^[\\./]', query)) stop_externals('Wrong query')  # starts with neither . nor /
      query <- paste0('./', if (!directChildren) '/', query)
    }
    source$findChildElements(how, query)
  } else {
    stop_externals('Wrong class of "source"')
  }
  # if (!length(res)) warning('>> empty')
  res
}

getEl <- function(source, query, directChildren=F) {
  res <- getEls(source, query, directChildren)
  if (length(res) > 1) {
    print(html(res))
    stop_externals(sprintf('\nElements found: %s', length(res)))
  }
  if (length(res) == 1) res[[1]]
}

isWebElement <- function(obj) class(obj) == 'webElement'

stopIfNotWebElement <- function(obj) {
  if (!isWebElement(obj)) stop_externals('Input element class: ', class(obj))
}

attr <- function(el, attrName) {
  if (!length(el)) return(el)
  stopIfNotWebElement(if (is.list(el)) el[[1]] else el)
  if (!is.list(el)) el <- list(el)
  
  unlist(lapply(el, function(x) {
    res <- x$getElementAttribute(attrName)
    if (length(res) == 1) res[[1]] else if (length(res) > 1) res
  }))
}

html <- function(el) attr(el, 'outerHTML')

text <- function(el) attr(el, 'outerText')

isVisible <- function(el) {
  stopIfNotWebElement(el)
  el$isElementDisplayed()[[1]]
}

click <- function(el) {
  stopIfNotWebElement(el)
  if (!isVisible(el)) {
    browser()
    stop_externals('Input element is invisible: ', html(el))
  }
  el$clickElement()
}

filterElByAttr <- function(els, attrKey, attrVal) {
  if (!is.list(els)) stop_externals('Wrong "els" class')
  res <- Filter(function(x) attr(x, attrKey) == attrVal, els)
  if (length(res) != 1) stop_externals()
  res[[1]]
}

moveSlider <- function(driver, dotEl, pos) {
  driver$mouseMoveToLocation(webElement = dotEl)
  driver$buttondown()
  driver$mouseMoveToLocation(x = pos - dotEl$getElementLocation()$x, y = -1L)
  driver$buttonup()
}
