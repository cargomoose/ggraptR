## scatter plot
scatterPlot <- reactive({
  flog.debug("plot::scatterPlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || is.null(scatterWidgetsLoaded()) || !scatterWidgetsLoaded() 
      || !(y() %in% finalDFVars())) return()
  flog.debug("plot::scatterPlot() - End", name='all')
  plotScatter(dataset, scatterPlotInputs())
})

## line plot
linePlot <- reactive({
  flog.debug("plot::linePlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || !lineWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  p <- plotLine(dataset, linePlotInputs())
  
  ## line plot with points overlay
  if (!linePtsOverlayWidgetsLoaded()) return(p)
  if (input$ptsOverlayCond) {
    p <- plotPointsOverlay(p, linePtsOverlayInputs())
  }
  
  flog.debug("plot::linePlot() - End", name='all')
  p
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

## histogram
histogram <- reactive({
  flog.debug("plot::histogram() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || !histogramWidgetsLoaded()) return()
  flog.debug("plot::histogram() - End", name='all')
  plotHistogram(dataset, histogramInputs())
})

## density plot
densityPlot <- reactive({
  flog.debug("plot::densityPlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || !densityWidgetsLoaded()) return()
  flog.debug("plot::densityPlot() - End", name='all')
  plotDensity(dataset, densityPlotInputs())
})

## box plot
boxPlot <- reactive({
  flog.debug("plot::boxPlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || is.null(boxPlotInputs()) || !boxWidgetsLoaded() 
      || !(y() %in% finalDFVars())) return()
  flog.debug("plot::boxPlot() - End", name='all')
  plotBox(dataset, boxPlotInputs())
})

## bar plot
barPlot <- reactive({
  flog.debug("plot::barPlot() - Begin", name='all')
  dataset <- plotDF()
  if (is.null(dataset) || !barWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  flog.debug("plot::barPlot() - End", name='all')
  plotBar(dataset, barPlotInputs())
})

pairsPlot <- reactive({
  flog.debug("plot::pairsPlot() - Begin", name='all')
  dataset <- plotDF()
  # browser()
  if (is.null(dataset) || is.null(pairsPlotInputs()) || !pairsWidgetsLoaded()) return()
  p <- plotPairs(dataset, pairsPlotInputs())
  
  flog.debug("plot::pairsPlot() - End", name='all')
  p
})

