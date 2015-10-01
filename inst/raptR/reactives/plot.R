## scatter plot inputs

## scatter plot inputs
scatterPlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), treatAsFacVarCol=treatAsFacVarCol(), 
              shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), jitter=jitter(), 
              alpha=alpha(), sizeMag=sizeMag())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line plot inputs
linePlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line points overlay inputs
linePtsOverlayInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), 
              smooth=smooth(), jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})


## bar plot inputs
barPlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## histogram inputs
histogramInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), 
              binWidth=binWidth(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## density plot inputs
densityPlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), 
              densBlkLineCond=densBlkLineCond(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## box plot inputs
boxPlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path plot inputs
pathPlotInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), alpha=alpha())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path points overlay inputs
pathPtsOverlayInputs <- reactive({
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), 
              jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})




## scatter plot
scatterPlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (is.null(scatterWidgetsLoaded())) return()
  if (!scatterWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  plotScatter(dataset, scatterPlotInputs())
})

## line plot
linePlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!lineWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  p <- plotLine(dataset, linePlotInputs())
  
  ## line plot with points overlay
  if (!linePtsOverlayWidgetsLoaded()) return()
  if (ptsOverlayCond) 
    p <- plotPointsOverlay(p, linePtsOverlayInputs())
  
  p
})

## bar plot
barPlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!barWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  plotBar(dataset, barPlotInputs())
})

## histogram
histogram <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!histogramWidgetsLoaded()) return()
  plotHistogram(dataset, histogramInputs())
})

## density plot
densityPlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!densityWidgetsLoaded()) return()
  plotDensity(dataset, densityPlotInputs())
})

## box plot
boxPlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (is.null(boxPlotInputs())) return()
  if (!boxWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  plotBox(dataset, boxPlotInputs())
})

## path plot 
pathPlot <- reactive({
  dataset <- plotDF(); if (is.null(dataset)) return()
  if (!pathWidgetsLoaded()) return()
  if (!(y() %in% finalDFVars())) return()
  p <- plotPath(dataset, pathPlotInputs())
  
  ## path plot with points overlay
  if (!pathPtsOverlayWidgetsLoaded()) return()
  if (ptsOverlayCond)
    p <- plotPointsOverlay(p, pathPtsOverlayInputs())
  p
})

## plot reactive
plotInput <- reactive({

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
    
    
  
  ## return
  p
})

