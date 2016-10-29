facetWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale')

scatterWidgets <- reactive({
  flog.debug("plotWidgetNames::scatterWidgets()", name='all')
  if (is.null(input$showAesWgts)) return()
  c('plotType', 'x', 'y',
    if (input$showAesWgts) c('treatAsFacVarCol', 'shape', 'size', 'smooth', 
                             'jitter' , 'sizeMag', 'color'),
    if (showXYRangeWgts()) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

lineWidgets <- reactive({
  flog.debug("plotWidgetNames::lineWidgets() - Start", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) 'color',
    if (showXYRangeWgts()) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

## lineplot points
linePtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::linePtsOverlayWidgets()", name='all')
  c('pointsOverlay', 
    if (input$showAesWgts) c('x', 'shape', 'size', 'smooth', 'jitter', 'sizeMag'))
})

pathWidgets <- reactive({
  flog.debug("plotWidgetNames::pathWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (showXYRangeWgts()) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

pathPtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::pathPtsOverlayWidgets()", name='all')
  c('pointsOverlay',
    if (showAesWgts()) c('shape', 'size'))
})

histogramWidgets <- reactive({
  flog.debug("plotWidgetNames::histogramWidgets()", name='all')
  c('plotType', 'x', 
    if (input$showAesWgts) c('fill', 'position', 'binWidth'),
    if (showXYRangeWgts()) 'xlim',
    if (input$showFacetWgts) facetWidgets)
})

densityWidgets <- reactive({
  flog.debug("plotWidgetNames::densityWidgets()", name='all')
  c('plotType', 'x', 
    if (showXYRangeWgts()) 'xlim',
    if (input$showFacetWgts) facetWidgets)
})

boxWidgets <- reactive({
  flog.debug("plotWidgetNames::boxWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) 'fill',
    if (showXYRangeWgts()) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

barWidgets <- reactive({
  flog.debug("plotWidgetNames::barWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) c('fill', 'position'),
    if (showXYRangeWgts()) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

violinWidgets <- reactive({
  flog.debug("plotWidgetNames::violinWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) c('fill'),
    if (input$showFacetWgts) facetWidgets)
})

pairsWidgets <- reactive({
  flog.debug("plotWidgetNames::pairshWidgets()", name='all')
  c('plotType', 'columns', if (input$showAesWgts) c('fill', 'color'))
})

themeWidgets <- reactive({
  flog.debug("plotWidgetNames::themeWidgets()", name='all')
  c('plotTitle', 'xLabel', 'yLabel', 'hjust', 'vjust',
    'labelFontFamily', 'labelFontFace', 
    'labelFontColor', 'labelFontSize')
})
