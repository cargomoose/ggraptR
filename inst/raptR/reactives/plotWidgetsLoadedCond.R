
#### widgets loaded conditional reactives

## universal plot widgets loaded
universalPlotWidgetsLoaded <- reactive({
  universalPlotWidgets <- c('plotType', 'x')
  checkWidgetsLoaded(input, universalPlotWidgets)
})

## scatter plot widgets loaded
scatterWidgetsLoaded <- reactive({
  if (is.null(scatterWidgets())) return()
  checkWidgetsLoaded(input, scatterWidgets())
})

## line widgets loaded
lineWidgetsLoaded <- reactive({
  if (is.null(lineWidgets())) return()
  checkWidgetsLoaded(input, lineWidgets())
})

## line plot points overlay widgets loaded
linePtsOverlayWidgetsLoaded <- reactive({
  if (is.null(linePtsOverlayWidgets())) return()
  checkWidgetsLoaded(input, linePtsOverlayWidgets())
})

## bar plot widgets loaded
barWidgetsLoaded <- reactive({
  if (is.null(barWidgets())) return()
  checkWidgetsLoaded(input, barWidgets())
})

## histogram widgets loaded
histogramWidgetsLoaded <- reactive({
  if (is.null(histogramWidgets())) return()
  checkWidgetsLoaded(input, histogramWidgets())
})

## density plot widgets loaded
densityWidgetsLoaded <- reactive({
  if (is.null(densityWidgets())) return()
  checkWidgetsLoaded(input, densityWidgets())
})

## box plot widgets loaded
boxWidgetsLoaded <- reactive({
  if (is.null(boxWidgets())) return()
  checkWidgetsLoaded(input, boxWidgets())
})

## path plot widgets loaded
pathWidgetsLoaded <- reactive({
  if (is.null(pathWidgets())) return()
  checkWidgetsLoaded(input, pathWidgets())
})

## path plot points overlay widgets loaded
pathPtsOverlayWidgetsLoaded <- reactive({
  if (is.null(pathPtsOverlayWidgets())) return()
  checkWidgetsLoaded(input, pathPtsOverlayWidgets())
})


## theme widgets loaded
themeWidgetsLoaded <- reactive({
  wgts <- c('plotTitle', 'xLabel', 'yLabel', 'hjust', 'vjust',
            'labelFontFamily', 'labelFontFace', 'labelFontColor', 'labelFontSize')
  checkWidgetsLoaded(input, wgts)
})