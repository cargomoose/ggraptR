#### widgets loaded conditional reactives

## universal plot widgets loaded
universalPlotWidgetsLoaded <- reactive({
  universalPlotWidgets <- c('plotType', 'x')
  checkWidgetsLoaded(input, universalPlotWidgets)
})

## scatter plot widgets loaded
scatterWidgetsLoaded <- reactive({
  if (!is.null(scatterWidgets())) {
    checkWidgetsLoaded(input, scatterWidgets())
  }
})

## line widgets loaded
lineWidgetsLoaded <- reactive({
  if (!is.null(lineWidgets())) {
    checkWidgetsLoaded(input, lineWidgets())
  }
})

## line plot points overlay widgets loaded
linePtsOverlayWidgetsLoaded <- reactive({
  if (!is.null(linePtsOverlayWidgets())) {
    checkWidgetsLoaded(input, linePtsOverlayWidgets())
  }
})

## bar plot widgets loaded
barWidgetsLoaded <- reactive({
  if (!is.null(barWidgets())) {
    checkWidgetsLoaded(input, barWidgets())
  }
})

## histogram widgets loaded
histogramWidgetsLoaded <- reactive({
  if (!is.null(histogramWidgets())) {
    checkWidgetsLoaded(input, histogramWidgets())
  }
})

## density plot widgets loaded
densityWidgetsLoaded <- reactive({
  if (!is.null(densityWidgets())) {
    checkWidgetsLoaded(input, densityWidgets())
  }
})

## box plot widgets loaded
boxWidgetsLoaded <- reactive({
  if (!is.null(boxWidgets())) {
    checkWidgetsLoaded(input, boxWidgets())
  }
})

## path plot widgets loaded
pathWidgetsLoaded <- reactive({
  if (!is.null(pathWidgets())) {
    checkWidgetsLoaded(input, pathWidgets())
  }
})

## path plot points overlay widgets loaded
pathPtsOverlayWidgetsLoaded <- reactive({
  if (!is.null(pathPtsOverlayWidgets())) {
    checkWidgetsLoaded(input, pathPtsOverlayWidgets())
  }
})

pairsWidgetsLoaded <- reactive({
  if (!is.null(pairsWidgets())) {
    checkWidgetsLoaded(input, pairsWidgets())
  }
})

## theme widgets loaded
themeWidgetsLoaded <- reactive({
  checkWidgetsLoaded(input, c('plotTitle', 'xLabel', 'yLabel', 'hjust', 'vjust',
                              'labelFontFamily', 'labelFontFace', 
                              'labelFontColor', 'labelFontSize'))
})
