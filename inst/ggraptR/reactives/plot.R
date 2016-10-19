scatterPlot <- reactive({
  flog.debug("plot::scatterPlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, scatterWidgets()) || 
      !(y() %in% finalDFVars())) return()
  plotScatter(plotDF(), scatterPlotInputs())
})

linePlot <- reactive({
  flog.debug("plot::linePlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, lineWidgets()) || 
      !(y() %in% finalDFVars())) return()
  p <- plotLine(plotDF(), linePlotInputs())
  if (checkWidgetsLoaded(input, linePtsOverlayWidgets()) && input$ptsOverlayCond) 
    plotPointsOverlay(p, linePtsOverlayInputs()) else p
})

pathPlot <- reactive({
  flog.debug("plot::pathPlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, pathWidgets()) || 
      !(y() %in% finalDFVars())) return()
  p <- plotPath(plotDF(), pathPlotInputs())
  
  if (checkWidgetsLoaded(input, pathPtsOverlayWidgets()) &&!is.null(input$ptsOverlayCond) 
      && input$ptsOverlayCond) 
    plotPointsOverlay(p, pathPtsOverlayInputs()) else p
})

histogram <- reactive({
  flog.debug("plot::histogram()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, histogramWidgets())) return()
  plotHistogram(plotDF(), histogramInputs())
})

densityPlot <- reactive({
  flog.debug("plot::densityPlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, densityWidgets())) return()
  plotDensity(plotDF(), densityPlotInputs())
})

boxPlot <- reactive({
  flog.debug("plot::boxPlot()", name='all')
  if (is.null(plotDF()) || is.null(boxPlotInputs()) || 
      !checkWidgetsLoaded(input, boxPlotWidgets()) || !(y() %in% finalDFVars())) return()
  plotBox(plotDF(), boxPlotInputs())
})

barPlot <- reactive({
  flog.debug("plot::barPlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, barPlotWidgets()) || 
      !(y() %in% finalDFVars())) return()
  plotBar(plotDF(), barPlotInputs())
})

violinPlot <- reactive({
  flog.debug("plot::violinPlot()", name='all')
  if (is.null(plotDF()) || !checkWidgetsLoaded(input, violinWidgets()) || 
      !(y() %in% finalDFVars())) return()
  plotViolin(plotDF(), violinInputs())
})

pairsPlot <- reactive({
  flog.debug("plot::pairsPlot()", name='all')
  if (is.null(plotDF()) || is.null(pairsPlotInputs()) || 
      !checkWidgetsLoaded(input, pairsWidgets())) return()
  plotPairs(plotDF(), pairsPlotInputs())
})

