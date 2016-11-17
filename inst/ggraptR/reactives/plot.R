getPlotInputVals <- function(aggLimDf, inputNames=NULL) {
  if (is.null(inputNames)) inputNames <- isolate(plotInputs())
  inputs <- lapply(inputNames, do.call, args=list(), envir=environment())
  names(inputs) <- inputNames  # results list(x=eval(x()), y=eval(y())...)
  ensureCorrectPlotInputs(inputs, colnames(aggLimDf))
}

buildPlot <- reactive({
  flog.debug("plot::buildPlot() - Begin", name='all')
  flog.debug("systime - begin", name='all')
  start.time <- Sys.time()
  flog.debug(start.time, name='all')
  
  # this block waits for controls and prevents premature plot drawing
  if (isolate(reactVals$readyToDraw)) {
    isolate(reactVals$readyToDraw <- F)
  } else {
    isolate({reactVals$itersToDraw <- 5})
    reactVals$readyToDraw  # the only dep if exits here
    return()
  }
  return()
  
  pTypes <- plotTypes()
  inputs <- getPlotInputVals(aggLimDf())
  p <- do.call.pasted('plot', if (pTypes == 'pairs') 'Pairs' else 'Ggplot', 
                      args=list(aggLimDf(), inputs, pTypes))
  
  if (any(pTypes != 'pairs')) {
    if (isFacetSelected()) {
      if (facetGridSelected()) {
        p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
      } else if (facetWrapSelected()) {
        p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
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
      isColorTypeDiscr <- isolate(colorType()) == 'discrete' #colorType() bases on color()
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
  
  # add plot history entry. It will show at Log tab
  if (!is.null(p)) {
    logEntry <- generateCode(p)
    curLog <- isolate(reactVals$log)
    isFirstEntry <- is.null(curLog)
    
    if (isFirstEntry || curLog[[1]] != logEntry) {
      reactVals$log <- if (isFirstEntry) logEntry else c(logEntry, curLog)
    }
  }
  
  p
})
