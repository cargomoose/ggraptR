getPlotInputVals <- function(structInpNames, df) {
  inputs <- lapply(structInpNames, function(onePlotTypeNames) {
    inp <- lapply(onePlotTypeNames, do.call, args=list(), envir=environment())
    names(inp) <- onePlotTypeNames
    ensureCorrectPlotInputs(inp, colnames(df))
  })
  names(inputs) <- names(structInpNames)
  inputs
}

buildPlot <- reactive({

  # this block waits for controls and prevents premature plot drawing
  if (isolate(reactVals$readyToDraw)) {
    isolate(reactVals$readyToDraw <- F)
  } else {
    isolate({reactVals$itersToDraw <- 5})
    reactVals$readyToDraw  # the only dep if exits here
    return()
  }
  
  pTypes <- plotTypes()
  if (is.null(pTypes)) return()
  df <- aggLimDf()
  isPairsPlot <- all('pairs' == pTypes)
  input_plot_pars <- getPlotInputVals(separatePlotInputs(), df)
  attr(input_plot_pars, 'extra') <- list(nCatUniqVals=isolate(nCatUniqVals()))
  
  if (isPairsPlot) {
    if (length(input_plot_pars[[1]]$columns) == 0) return()
  } else {
    stopifnot(length(input_plot_pars[[1]]$x) > 0)
  }
  
  p <- do.call.pasted('plot', if (isPairsPlot) 'Pairs' else 'Ggplot', 
                      args=list(df, input_plot_pars))
  stopifnot(isPairsPlot || length(p$mapping) > 0)
  if (!is.null(p$pairsAes)) {
    isolate(reactVals$plotState$pairs <- p$pairsAes)
  }
  
  if (!isPairsPlot) {
    if (isFacetSelected()) {
      if (facetGridSelected()) {
        p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
      } else if (facetWrapSelected()) {
        p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
      }
    }
    
    p <- p + if (coordFlip()) coord_flip()
    
    # plot labels 
    p <- p + if (!is.null(plotTitle()) && plotTitle() != '') ggtitle(plotTitle())
    p <- p + if (!is.null(xLabel()) && xLabel() != '') xlab(xLabel())
    p <- p + if (!is.null(yLabel()) && yLabel() != '') ylab(yLabel())
    
    # plot themes
    if (!is.null(plotTheme())) {
      p <- p + do.call(plotTheme(), list())
      
      theme_name <- rev(unlist(str_split(plotTheme(), '_')))[1]
      
      isolate(reactVals$plotState$theme_name <- theme_name)
      isColorTypeDiscr <- colorType() == 'discrete' #colorType() bases on color()
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
    
    # plot label styles
    theme_attrs <- list(family = labelFontFamily(),
                        face = labelFontFace(),
                        color = labelFontColor(),
                        size = labelFontSize(),
                        hjust = hjust(),
                        vjust = vjust())
    isolate(reactVals$plotState$theme_attrs <- theme_attrs)
    p <- p + theme(text=do.call(element_text, theme_attrs))
  }
  
  # add plot history entry. It will be shown at Log tab
  isolate({
    if (!is.null(p)) {
      logEntry <- generateCode(p, df, datasetName(), reactVals$plotState)
      curLog <- reactVals$log
      isFirstEntry <- is.null(curLog)
      
      if (isFirstEntry || curLog[[1]] != logEntry) {
        reactVals$log <- if (isFirstEntry) logEntry else c(logEntry, curLog)
      }
    }
  })
  
  p
})
