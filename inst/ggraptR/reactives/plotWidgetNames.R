facetWidgets <- reactive({ 
  if (showFacetWgts()) c('facetRow', 'facetCol', 'facetWrap', 'facetScale')
})
XYRangeWidgets <- reactive({ 
  if (showXYRangeWgts()) c('xlim', 'ylim')
})


scatterWidgets <- reactive({
  flog.debug("plotWidgetNames::scatterWidgets()", name='all')
  if (is.null(showAesWgts())) return()
  c('plotType', 'x', 'y', XYRangeWidgets(), facetWidgets(),
    if (showAesWgts()) c('treatAsFacVarCol', 'shape', 'size', 'smooth', 
                         'jitter' , 'sizeMag', 'color'))
})

lineWidgets <- reactive({
  flog.debug("plotWidgetNames::lineWidgets() - Start", name='all')
  c('plotType', 'x', 'y', XYRangeWidgets(), facetWidgets(),
    if (showAesWgts()) 'color')
})

## lineplot points
linePtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::linePtsOverlayWidgets()", name='all')
  c('pointsOverlay', 
    if (showAesWgts()) c('x', 'shape', 'size', 'smooth', 'jitter', 'sizeMag'))
})

pathWidgets <- reactive({
  flog.debug("plotWidgetNames::pathWidgets()", name='all')
  c('plotType', 'x', 'y', XYRangeWidgets(), facetWidgets())
})

pathPtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::pathPtsOverlayWidgets()", name='all')
  c('pointsOverlay', if (showAesWgts()) c('shape', 'size'))
})

histogramWidgets <- reactive({
  flog.debug("plotWidgetNames::histogramWidgets()", name='all')
  c('plotType', 'x', facetWidgets(), if (showXYRangeWgts()) 'xlim'
    if (showAesWgts()) c('fill', 'position', 'binWidth'))
})

densityWidgets <- reactive({
  flog.debug("plotWidgetNames::densityWidgets()", name='all')
  c('plotType', 'x', facetWidgets(), if (showXYRangeWgts()) 'xlim')
})

boxWidgets <- reactive({
  flog.debug("plotWidgetNames::boxWidgets()", name='all')
  c('plotType', 'x', 'y', XYRangeWidgets(), facetWidgets(), if (showAesWgts()) 'fill')
})

barWidgets <- reactive({
  flog.debug("plotWidgetNames::barWidgets()", name='all')
  c('plotType', 'x', 'y', XYRangeWidgets(), facetWidgets(), 
    if (showAesWgts()) c('fill', 'position'))
})

violinWidgets <- reactive({
  flog.debug("plotWidgetNames::violinWidgets()", name='all')
  c('plotType', 'x', 'y', facetWidgets(), if (showAesWgts()) c('fill'))
})

pairsWidgets <- reactive({
  flog.debug("plotWidgetNames::pairshWidgets()", name='all')
  c('plotType', 'columns', if (showAesWgts()) c('fill', 'color'))
})

themeWidgets <- reactive({
  flog.debug("plotWidgetNames::themeWidgets()", name='all')
  c('plotTitle', 'xLabel', 'yLabel', 'hjust', 'vjust',
    'labelFontFamily', 'labelFontFace', 
    'labelFontColor', 'labelFontSize')
})
