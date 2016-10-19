# widgets required by plot type
facetWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale')

scatterWidgets <- reactive({
  flog.debug("plotWidgetNames::scatterWidgets()", name='all')
  
  if (is.null(input$showAesWgts)){
    flog.debug("plotWidgetNames::scatterWidgets() - is.null(input$showAesWgts) - End", 
               name='all')
    return()
  }
  if (is.null(input$showXYRangeWgts)){
    flog.debug("plotWidgetNames::scatterWidgets() - is.null(input$showXYRangeWgts) - End", 
               name='all')
    return()
  }

  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts){
    flog.debug("plotWidgetNames::scatterWidgets() - input$showAesWgts", name='all')
    wgts <- c(wgts, 'treatAsFacVarCol', 'shape', 'size', 'smooth', 'jitter' , 'sizeMag', 
              'color')
  }
  if (input$showXYRangeWgts){
    flog.debug("plotWidgetNames::scatterWidgets() - input$showXYRangeWgts", name='all')
    wgts <- c(wgts, 'xlim', 'ylim')
  }
  if (input$showFacetWgts){
    flog.debug("plotWidgetNames::scatterWidgets() - input$showFacetWgts", name='all')
    wgts <- c(wgts, facetWidgets)
  }
  
  flog.debug("plotWidgetNames::scatterWidgets() - End", name='all')
  wgts
})

lineWidgets <- reactive({
  flog.debug("plotWidgetNames::lineWidgets() - Start", name='all')

  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts){
    flog.debug("plotWidgetNames::lineWidgets() - input$showAesWgts", name='all')
    wgts <- c(wgts, 'color')
  }
  if (input$showXYRangeWgts){
    flog.debug("plotWidgetNames::lineWidgets() - input$showXYRangeWgts", name='all')
    wgts <- c(wgts, 'xlim', 'ylim')
  }
  if (input$showFacetWgts){
    flog.debug("plotWidgetNames::lineWidgets() - input$showFacetWgts", name='all')
    wgts <- c(wgts, facetWidgets)
  }
  
  flog.debug("plotWidgetNames::lineWidgets() - End", name='all')
  wgts
})

## lineplot points
linePtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::linePtsOverlayWidgets()", name='all')
  wgts <- c('ptsOverlayCond')
  if (input$showAesWgts){
    flog.debug("plotWidgetNames::linePtsOverlayWidgets() - input$showAesWgts", name='all')
    wgts <- c(wgts, 'x', 'shape', 'size', 'smooth', 'jitter', 'sizeMag')
  }
  wgts
})

pathWidgets <- reactive({
  flog.debug("plotWidgetNames::pathWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showXYRangeWgts) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

pathPtsOverlayWidgets <- reactive({
  flog.debug("plotWidgetNames::pathPtsOverlayWidgets()", name='all')
  c('ptsOverlayCond',
    if (input$showAesWgts) c('shape', 'size'))
})

histogramWidgets <- reactive({
  flog.debug("plotWidgetNames::histogramWidgets()", name='all')
  c('plotType', 'x', 
    if (input$showAesWgts) c('fill', 'position', 'binWidth'),
    if (input$showXYRangeWgts) 'xlim',
    if (input$showFacetWgts) facetWidgets)
})

densityWidgets <- reactive({
  flog.debug("plotWidgetNames::densityWidgets()", name='all')
  c('plotType', 'x', 
    if (input$showXYRangeWgts) 'xlim',
    if (input$showFacetWgts) facetWidgets)
})

boxWidgets <- reactive({
  flog.debug("plotWidgetNames::boxWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) 'fill',
    if (input$showXYRangeWgts) c('xlim', 'ylim'),
    if (input$showFacetWgts) facetWidgets)
})

barWidgets <- reactive({
  flog.debug("plotWidgetNames::barWidgets()", name='all')
  c('plotType', 'x', 'y',
    if (input$showAesWgts) c('fill', 'position'),
    if (input$showXYRangeWgts) c('xlim', 'ylim'),
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


