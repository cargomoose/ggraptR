## scatter plot inputs
scatterPlotInputs <- reactive({
  flog.debug("plot::scatterPlotInputs() - Begin", name='all')
  if (is.null(plotDF())){
    flog.debug("plot::scatterPlotInputs() - (is.null(plotDF()) - End", name='all')
    return()
  }
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), treatAsFacVarCol=treatAsFacVarCol(), 
              shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), jitter=jitter(), 
              alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::scatterPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line plot inputs
linePlotInputs <- reactive({
  flog.debug("plot::linePlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
  flog.debug("plot::linePlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line points overlay inputs
linePtsOverlayInputs <- reactive({
  flog.debug("plot::linePtsOverlayInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), 
              smooth=smooth(), jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::linePtsOverlayInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})


## bar plot inputs
barPlotInputs <- reactive({
  flog.debug("plot::barPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), alpha=alpha())
  flog.debug("plot::barPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## histogram inputs
histogramInputs <- reactive({
  flog.debug("plot::histogramInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), 
              binWidth=binWidth(), alpha=alpha())
  flog.debug("plot::histogramInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## density plot inputs
densityPlotInputs <- reactive({
  flog.debug("plot::densityPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), 
              densBlkLineCond=densBlkLineCond(), alpha=alpha())
  flog.debug("plot::densityPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## box plot inputs
boxPlotInputs <- reactive({
  flog.debug("plot::boxPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), alpha=alpha())
  flog.debug("plot::boxPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path plot inputs
pathPlotInputs <- reactive({
  flog.debug("plot::pathPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
  flog.debug("plot::pathPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path points overlay inputs
pathPtsOverlayInputs <- reactive({
  flog.debug("plot::pathPtsOverlayInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), 
              jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::pathPtsOverlayInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})




## scatter plot
scatterPlot <- reactive({
  flog.debug("plot::scatterPlot() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (is.null(scatterWidgetsLoaded())) return()
  if (!scatterWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  flog.debug("plot::scatterPlot() - End", name='all')
  plotScatter(dataset, scatterPlotInputs())
})

## line plot
linePlot <- reactive({
  flog.debug("plot::linePlot() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!lineWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  p <- plotLine(dataset, linePlotInputs())
  
  ## line plot with points overlay
  if (!linePtsOverlayWidgetsLoaded()) return(p)
  if (input$ptsOverlayCond) 
    p <- plotPointsOverlay(p, linePtsOverlayInputs())
  
  flog.debug("plot::linePlot() - End", name='all')
  p
})

## bar plot
barPlot <- reactive({
  flog.debug("plot::barPlot() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!barWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  flog.debug("plot::barPlot() - End", name='all')
  plotBar(dataset, barPlotInputs())
})

## histogram
histogram <- reactive({
  flog.debug("plot::histogram() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!histogramWidgetsLoaded()) return()
  flog.debug("plot::histogram() - End", name='all')
  plotHistogram(dataset, histogramInputs())
})

## density plot
densityPlot <- reactive({
  flog.debug("plot::densityPlot() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!densityWidgetsLoaded()) return()
  flog.debug("plot::densityPlot() - End", name='all')
  plotDensity(dataset, densityPlotInputs())
})

## box plot
boxPlot <- reactive({
  flog.debug("plot::boxPlot() - Begin", name='all')
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (is.null(boxPlotInputs())) return()
  if (!boxWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  flog.debug("plot::boxPlot() - End", name='all')
  plotBox(dataset, boxPlotInputs())
})

## path plot 
pathPlot <- reactive({
  flog.debug("plot::pathPlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || !pathWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  p <- plotPath(dataset, pathPlotInputs())
  
  ## path plot with points overlay
  if (!pathPtsOverlayWidgetsLoaded()) return(p)
  if (!is.null(input$ptsOverlayCond) && input$ptsOverlayCond) {
    p <- plotPointsOverlay(p, pathPtsOverlayInputs())
  }
  
  flog.debug("plot::pathPlot() - End", name='all')
  p
})

## plot reactive
plotInput <- reactive({
  flog.debug("plot::plotInput() - Begin", name='all')
  flog.debug("systime - begin", name='all')
  start.time <- Sys.time()
  flog.debug(start.time, name='all')
  
  ## don't plot anything if any of universal control widgets is not loaded
  if (!universalPlotWidgetsLoaded()) return()
  
  ## don't plot anything if x hasn't been updated according to new dataset
  if (!(x() %in% xOpts())) return()
  
  p <- do.call(paste0(plotType(), 
                      if (plotType() == 'histogram') '' else 'Plot'), list())
    
  ## plot facet controls
  if (!noFacetSelected()) {
    ## facet grids
    if (facetGridSelected()) {
      p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
    } else if (facetWrapSelected()) {  ## facet wrap
      p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
    }
  }
  
  ## plot coord flip control 
  p <- p + if (coordFlip()) coord_flip()

  ## plot labels 
  p <- p + if (!is.null(plotTitle()) && plotTitle() != '') ggtitle(plotTitle())
  p <- p + if (!is.null(xLabel()) && xLabel() != '') xlab(xLabel())
  p <- p + if (!is.null(yLabel()) && yLabel() != '') ylab(yLabel())
  
  ## plot themes
  if (!is.null(plotTheme())) {
    p <- p + do.call(plotTheme(), list())
    
    theme_name <- rev(unlist(str_split(plotTheme(), '_')))[1]
    p$rappy$theme_name <- theme_name
    color_type_flag <- !is.null(colorType()) && colorType() == 'discrete'
    if (!theme_name %in% c('grey', 'bw', 'economist')) {
      scale_color_name <- sprintf('scale_colour_%s()', theme_name)
      if (theme_name == 'calc') {
        scale_color_name <- sub('u', '', scale_color_name)
      }
      p <- p + if (color_type_flag) do.call(scale_color_name, list())
    } else if (theme_name == 'economist') {
      p <- p + scale_colour_economist() + if (color_type_flag) scale_color_calc()
    }
  }
    
  ## plot label styles
  p$rappy$theme_attrs <- list(family = labelFontFamily(),
                              face = labelFontFace(),
                              color = labelFontColor(),
                              size = labelFontSize(),
                              hjust = hjust(),
                              vjust = vjust())
  p <- p + theme(text=do.call(element_text, p$rappy$theme_attrs))
    
  flog.debug("plot::plotInput() - End", name='all')
  flog.debug("proctime - end", name='all')
  end.time <- Sys.time()
  flog.debug(end.time, name='all')
  time.taken <- end.time - start.time
  flog.debug("time.taken", name='all')
  flog.debug(time.taken , name='all')
  
  if (!is.null(p)) {
    logEntry <- generateCode(p)
    curLog <- isolate(log$plot)
    isFirstEntry <- is.null(curLog)
    
    if (isFirstEntry || curLog[[length(curLog)]] != logEntry) {
      log$plot <- if (isFirstEntry) logEntry else c(logEntry, curLog)
    }
  }

  p
})

generateCodeReactive <- reactive({
  flog.debug("plot::generateCode() - Begin", name='all')
  res <- generateCode(plotInput())
  flog.debug("plot::generateCode() - res", name='all')
  flog.debug(res, name='all')
  flog.debug("plot::generateCode() - End", name='all')
  res
})


