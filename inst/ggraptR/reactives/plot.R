scatterPlot <- reactive({
  flog.debug("plot::scatterPlot()", name='all')
  if (is.null(plotDF()) || is.null(scatterWidgetsLoaded()) || !scatterWidgetsLoaded()
      || !(y() %in% finalDFVars())) return()
  plotScatter(plotDF(), scatterPlotInputs())
})

linePlot <- reactive({
  flog.debug("plot::linePlot()", name='all')
  if (is.null(plotDF()) || !lineWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  p <- plotLine(plotDF(), linePlotInputs())
  if (linePtsOverlayWidgetsLoaded() && input$ptsOverlayCond) 
    plotPointsOverlay(p, linePtsOverlayInputs()) else p
})

pathPlot <- reactive({
  flog.debug("plot::pathPlot()", name='all')
  if (is.null(plotDF()) || !pathWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  p <- plotPath(plotDF(), pathPlotInputs())
  
  if (pathPtsOverlayWidgetsLoaded() &&!is.null(input$ptsOverlayCond) 
      && input$ptsOverlayCond) 
    plotPointsOverlay(p, pathPtsOverlayInputs()) else p
})

histogram <- reactive({
  flog.debug("plot::histogram()", name='all')
  if (is.null(plotDF()) || !histogramWidgetsLoaded()) return()
  plotHistogram(plotDF(), histogramInputs())
})

densityPlot <- reactive({
  flog.debug("plot::densityPlot()", name='all')
  if (is.null(plotDF()) || !densityWidgetsLoaded()) return()
  plotDensity(plotDF(), densityPlotInputs())
})

boxPlot <- reactive({
  flog.debug("plot::boxPlot()", name='all')
  if (is.null(plotDF()) || is.null(boxPlotInputs()) || !boxWidgetsLoaded() 
      || !(y() %in% finalDFVars())) return()
  plotBox(plotDF(), boxPlotInputs())
})

barPlot <- reactive({
  flog.debug("plot::barPlot()", name='all')
  if (is.null(plotDF()) || !barWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  plotBar(plotDF(), barPlotInputs())
})

violinPlot <- reactive({
  flog.debug("plot::violinPlot()", name='all')
  if (is.null(plotDF()) || !violinWidgetsLoaded() || !(y() %in% finalDFVars())) return()
  plotViolin(plotDF(), violinInputs())
})

pairsPlot <- reactive({
  flog.debug("plot::pairsPlot()", name='all')
  if (is.null(plotDF()) || is.null(pairsPlotInputs()) || !pairsWidgetsLoaded()) return()
  plotPairs(plotDF(), pairsPlotInputs())
})

