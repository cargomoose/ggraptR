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
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!pathWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  p <- plotPath(dataset, pathPlotInputs())
  
  ## path plot with points overlay
  if (!pathPtsOverlayWidgetsLoaded()) return(p)
  if (input$ptsOverlayCond)
    p <- plotPointsOverlay(p, pathPtsOverlayInputs())
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
  
  ## scatter plot
  if (plotType()=='scatter')  {
    p <- scatterPlot()
  }

  ## line plot
  else if (plotType()=='line') {
    p <- linePlot()
  }
  
  ## bar plot
  else if (plotType()=='bar') {
    p <- barPlot()
  }
  
  ## histogram
  else if (plotType()=='histogram') {
    p <- histogram()
  }

  ## density plot
  else if (plotType()=='density') {
    p <- densityPlot()
  }
  
  ## box plot
  else if (plotType()=='box') {
    p <- boxPlot()
  }
  
  ## path plot
  else if (plotType()=='path') {
    p <- pathPlot()
  }
  
  ## plot facet controls
  if (!noFacetSelected()) {

    ## facet grids
    if (facetGridSelected()) {
      p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
    } 
    
    ## facet wrap
    else if (facetWrapSelected()) {
      p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
    }
  }
  
  ## plot coord flip control 
  if (coordFlip()) 
    p <- p + coord_flip()

  ## plot labels 
  if (!is.null(plotTitle()))
    if (plotTitle() != '')
      p <- p + ggtitle(plotTitle())
  if (!is.null(xLabel()))
    if (xLabel() != '')
      p <- p + xlab(xLabel())
  if (!is.null(yLabel())) 
    if (yLabel() != '')
      p <- p + ylab(yLabel())
  
  ## plot themes
  if (!is.null(plotTheme())) {
    if (plotTheme()=='theme_grey')
      p <- p + theme_grey()
    else if (plotTheme()=='theme_bw') 
      p <- p + theme_bw()
    else if (plotTheme()=='theme_calc') {
      p <- p + theme_calc()
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_color_calc()
    }
    else if (plotTheme()=='theme_economist') {
      p <- p + theme_economist() + scale_colour_economist()
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_color_calc()
    }
    else if (plotTheme()=='theme_few') {
      p <- p + theme_few()
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_few()      
    }
    else if (plotTheme()=='theme_fivethirtyeight') {
      p <- p + theme_fivethirtyeight() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_fivethirtyeight()      
    }
    else if (plotTheme()=='theme_gdocs') {
      p <- p + theme_gdocs() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_gdocs()
    }
    else if (plotTheme()=='theme_hc') {
      p <- p + theme_hc() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_hc()
    }
    else if (plotTheme()=='theme_pander') {
      p <- p + theme_pander() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_pander()      
    } 
    else if (plotTheme()=='theme_solarized') {
      p <- p + theme_solarized() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_solarized()      
    }
    else if (plotTheme()=='theme_stata') {
      p <- p + theme_stata() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_stata()      
    }
    else if (plotTheme()=='theme_tufte') {
      p <- p + theme_tufte() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_tufte()      
    }
    else if (plotTheme()=='theme_wsj') {
      p <- p + theme_wsj() 
      if (!is.null(colorType()))
        if (colorType()=='discrete')
          p <- p + scale_colour_wsj()      
    }
  }
    
    ## plot label styles
    p <- p + 
      theme(text=element_text(family = labelFontFamily(),
                              face = labelFontFace(),
                              color = labelFontColor(),
                              size = labelFontSize(),
                              hjust = hjust(),
                              vjust = vjust()))
    
  flog.debug("plot::plotInput() - End", name='all')

  flog.debug("proctime - end", name='all')
  end.time <- Sys.time()
  flog.debug(end.time, name='all')
  time.taken <- end.time - start.time
  flog.debug("time.taken", name='all')
  flog.debug(time.taken , name='all')

  ## return
  p
})

