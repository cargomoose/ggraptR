universalPlotWidgetsLoaded <- reactive({
  universalPlotWidgets <- c('plotType', 'x')
  checkWidgetsLoaded(input, universalPlotWidgets)
})

scatterWidgetsLoaded <- reactive({
  if (!is.null(scatterWidgets())) {
    checkWidgetsLoaded(input, scatterWidgets())
  }
})

lineWidgetsLoaded <- reactive({
  if (!is.null(lineWidgets())) {
    checkWidgetsLoaded(input, lineWidgets())
  }
})

## line plot points overlay
linePtsOverlayWidgetsLoaded <- reactive({
  if (!is.null(linePtsOverlayWidgets())) {
    checkWidgetsLoaded(input, linePtsOverlayWidgets())
  }
})

barWidgetsLoaded <- reactive({
  if (!is.null(barWidgets())) {
    checkWidgetsLoaded(input, barWidgets())
  }
})

histogramWidgetsLoaded <- reactive({
  if (!is.null(histogramWidgets())) {
    checkWidgetsLoaded(input, histogramWidgets())
  }
})

densityWidgetsLoaded <- reactive({
  if (!is.null(densityWidgets())) {
    checkWidgetsLoaded(input, densityWidgets())
  }
})

boxWidgetsLoaded <- reactive({
  if (!is.null(boxWidgets())) {
    checkWidgetsLoaded(input, boxWidgets())
  }
})

pathWidgetsLoaded <- reactive({
  if (!is.null(pathWidgets())) {
    checkWidgetsLoaded(input, pathWidgets())
  }
})

pathPtsOverlayWidgetsLoaded <- reactive({
  if (!is.null(pathPtsOverlayWidgets())) {
    checkWidgetsLoaded(input, pathPtsOverlayWidgets())
  }
})

violinWidgetsLoaded <- reactive({
  if (!is.null(violinWidgets())) {
    checkWidgetsLoaded(input, violinWidgets())
  }
})

pairsWidgetsLoaded <- reactive({
  if (!is.null(pairsWidgets())) {
    checkWidgetsLoaded(input, pairsWidgets())
  }
})

themeWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, c('plotTitle', 'xLabel', 'yLabel', 'hjust', 'vjust',
                              'labelFontFamily', 'labelFontFace', 
                              'labelFontColor', 'labelFontSize'))
})
