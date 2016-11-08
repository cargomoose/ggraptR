getPlotInputVals <- function(plotDF, inputNames=NULL) {
  if (is.null(inputNames)) inputNames <- isolate(plotInputs())
  inputs <- lapply(inputNames, do.call, args=list(), envir=environment())
  names(inputs) <- inputNames  # results list(x=eval(x()), y=eval(y())...)
  ensureCorrectPlotInputs(inputs, colnames(plotDF))
}

getBasePlot <- function(pType, plotDF) {
  inputs <- getPlotInputVals(plotDF)
  for (axes in c('x', 'y')) if (!is.null(input[[axes]]) && input[[axes]] == '') return()
  pTypeCapit <- paste0(toupper(substr(pType, 1, 1)), substr(pType, 2, nchar(pType)))
  p <- do.call.pasted('plot', pTypeCapit, args=list(plotDF, inputs))  # scatterPlot(args)
  
  if ('pointsOverlay' %in% isolate(plotInputs()) && pointsOverlay()) {
    overInputs <- getPlotInputVals(plotDF, pointsOverlayInputs())
    p <- fillPlotWithPointsOverlay(p, overInputs)
  }
  p
}

buildPlot <- reactive({
  flog.debug("plot::buildPlot() - Begin", name='all')
  flog.debug("systime - begin", name='all')
  start.time <- Sys.time()
  flog.debug(start.time, name='all')
  
  if (isolate(controlsLoading$ready)) {
    isolate(controlsLoading$ready <- F)
  } else {
    isolate({controlsLoading$itersToDrawPlot <- 5})
    controlsLoading$ready  # the only dep if exits here
    return()
  }
  
  # if (!controlsLoading$ready) return()  
  pType <- plotType()
  p <- getBasePlot(pType, isolate(plotDF()))
  
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
