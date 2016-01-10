#### widgets required by plot type

## facet widgets
facetWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale')

## scatter plot widgets
scatterWidgets <- reactive({
  
  flog.debug("plotWidgetNames::scatterWidgets() - Begin", name='all')
  
  if (is.null(input$showAesWgts)){
    flog.debug("plotWidgetNames::scatterWidgets() - is.null(input$showAesWgts) - End", name='all')
    return()
  }
  
  if (is.null(input$showXYRangeWgts)){
    flog.debug("plotWidgetNames::scatterWidgets() - is.null(input$showXYRangeWgts) - End", name='all')
    return()
  }
  
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts){
    flog.debug("plotWidgetNames::scatterWidgets() - input$showAesWgts", name='all')
    wgts <- c(wgts, 'treatAsFacVarCol', 'shape', 'size', 'smooth', 'jitter' , 'sizeMag', 'color')
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

## line plot widgets
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

## line plot points overlay widgets
linePtsOverlayWidgets <- reactive({
  
  flog.debug("plotWidgetNames::linePtsOverlayWidgets() - Begin", name='all')
  
  wgts<-c('ptsOverlayCond')
  if (input$showAesWgts){
    flog.debug("plotWidgetNames::linePtsOverlayWidgets() - input$showAesWgts", name='all')
    wgts <- c(wgts, 'x', 'shape', 'size', 'smooth', 'jitter', 'sizeMag')
  }
  
  flog.debug("plotWidgetNames::linePtsOverlayWidgets() - End", name='all')
  
  wgts
})

## bar plot widgets
barWidgets <- reactive({
  
  flog.debug("plotWidgetNames::barWidgets() - Start", name='all')
  
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill', 'position')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  
  flog.debug("plotWidgetNames::barWidgets() - End", name='all')
  
  wgts
})

# ## histogram widgets
histogramWidgets <- reactive({
  
  flog.debug("plotWidgetNames::histogramWidgets() - Start", name='all')
  
  wgts <- c('plotType', 'x')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill', 'position', 'binWidth')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  
  flog.debug("plotWidgetNames::histogramWidgets() - End", name='all')
  
  wgts
})

## density plot widgets
densityWidgets <- reactive({
  
  flog.debug("plotWidgetNames::densityWidgets() - Start", name='all')
  
  wgts <- c('plotType', 'x')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  
  flog.debug("plotWidgetNames::densityWidgets() - End", name='all')
  
  wgts
})

## box plot widgets
boxWidgets <- reactive({
  
  flog.debug("plotWidgetNames::boxWidgets() - Start", name='all')
  
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  
  flog.debug("plotWidgetNames::boxWidgets() - End", name='all')
  
  wgts
})

## path plot widgets
pathWidgets <- reactive({
  
  flog.debug("plotWidgetNames::pathWidgets() - Start", name='all')
  
  wgts <- c('plotType', 'x', 'y')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  
  flog.debug("plotWidgetNames::pathWidgets() - End", name='all')
  
  wgts
})

## path plot points overlay widgets loaded
pathPtsOverlayWidgets <- reactive({
  
  flog.debug("plotWidgetNames::pathPtsOverlayWidgets() - Start", name='all')
  
  wgts <- c('ptsOverlayCond')
  if (input$showAesWgts) 
    wgts <- c('shape', 'size')
  
  flog.debug("plotWidgetNames::pathPtsOverlayWidgets() - End", name='all')
  
  wgts
})