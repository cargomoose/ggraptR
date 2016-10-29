getPlotInputs <- reactive({
  pType <- isolate(plotType())
  inputNames <- plotInputsRegister()[[pType]]
  # we need to isolate x(). It will effect using y()
  subscribedInputNames <- setdiff(inputNames, c('x', 'xAsFactor'))
  
  inputs <- lapply(subscribedInputNames, do.call, args=list(), envir=environment())
  names(inputs) <- subscribedInputNames  # results list(x=x(), y=y()...)
  
  if (pType != 'pairs') ggpairsLowDiscr() else c(alpha())  # make dependency
  if ('x' %in% inputNames) inputs$x <- isolate(x())
  if ('xAsFactor' %in% inputNames) inputs$xAsFactor <- isolate(xAsFactor())
  ensureCorrectPlotInputs(inputs, colnames(isolate(plotDF())))
})

getBasePlot <- function(plotDF, pType, inputs) {
  pTypeCapit <- paste0(toupper(substr(pType, 1, 1)), substr(pType, 2, nchar(pType)))
  p <- do.call.pasted('plot', pTypeCapit, args=list(plotDF, inputs))
  
  if (pType %in% c('line', 'path')) {
    overlayWidgets <- do.call.pasted(pType, 'PtsOverlayWidgets')
    areLoaded <- checkWidgetsLoaded(input, overlayWidgets)
    if (areLoaded && pointsOverlay()) {
      overlayInputs <- do.call.pasted(pType, 'PtsOverlayInputs')
      p <- plotPointsOverlay(p, overlayInputs)
    }
  }
  p
}

buildPlot <- reactive({
  flog.debug("plot::buildPlot() - Begin", name='all')
  flog.debug("systime - begin", name='all')
  start.time <- Sys.time()
  flog.debug(start.time, name='all')
  
  pType <- isolate(plotType())
  if (is.null(pType) && is.null(y())) return()  # y() subscription for the first run
  # if (pType == 'violin') browser()
  inputs <- getPlotInputs()  # subscription must be before first return(NULL)
  isolate({
    plotDF <- plotDF()
    plotWidgets <- do.call.pasted(pType, 'Widgets')
    arePlotWidgetsLoaded <- !is.null(plotWidgets) &&
      checkWidgetsLoaded(input, c('plotType', plotWidgets))
  })
  
  # if (pType == 'violin') browser()
  if (is.null(plotDF) || !arePlotWidgetsLoaded) return()
  p <- getBasePlot(plotDF, pType, inputs)
  if (is.null(p)) return()
  
  if (pType != 'pairs') {
    if (!noFacetSelected()) {
      if (facetGridSelected()) {
        p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
      } else if (facetWrapSelected()) {  ## facet wrap
        p <- p + facet_wrap(facets=facetWrapFix(), scales=facetScale())
      }
    }
    
    p <- p + if (coordFlip()) coord_flip()
    
    ## plot labels 
    p <- p + if (!is.null(plotTitle()) && plotTitle() != '') ggtitle(plotTitle())
    p <- p + if (!is.null(xLabel()) && xLabel() != '') xlab(xLabel())
    p <- p + if (!is.null(yLabel()) && yLabel() != '') ylab(yLabel())
    
    ## plot themes
    if (!is.null(plotTheme())) {
      p <- p + do.call(plotTheme(), list())
      
      theme_name <- rev(unlist(str_split(plotTheme(), '_')))[1]
      
      state$theme_name <- theme_name
      isColorTypeDiscr <- isolate(colorType()) == 'discrete'  ########
      if (!theme_name %in% c('grey', 'bw', 'economist')) {
        scale_color_name <- sprintf('scale_colour_%s', theme_name)
        if (theme_name == 'calc') {
          scale_color_name <- sub('u', '', scale_color_name)
        }
        p <- p + if (isColorTypeDiscr && theme_name != 'tufte') 
          do.call(scale_color_name, list())
      } else if (theme_name == 'economist') {
        p <- p + scale_colour_economist() + if (isColorTypeDiscr) scale_color_calc()
      }
    }
    
    ## plot label styles
    state$theme_attrs <- list(family = labelFontFamily(),
                              face = labelFontFace(),
                              color = labelFontColor(),
                              size = labelFontSize(),
                              hjust = hjust(),
                              vjust = vjust())
    p <- p + theme(text=do.call(element_text, state$theme_attrs))
  }
  
  flog.debug("plot::buildPlot() - End", name='all')
  flog.debug("proctime - end", name='all')
  end.time <- Sys.time()
  flog.debug(end.time, name='all')
  time.taken <- end.time - start.time
  flog.debug("time.taken", name='all')
  flog.debug(time.taken , name='all')
  
  # add plot history entry
  if (!is.null(p)) {
    logEntry <- generateCode(p)
    curLog <- isolate(log$plot)
    isFirstEntry <- is.null(curLog)
    
    if (isFirstEntry || curLog[[1]] != logEntry) {
      log$plot <- if (isFirstEntry) logEntry else c(logEntry, curLog)
    }
  }
  
  p
})
