#### widgets required by plot type

## facet widgets
facetWidgets <- c('facetRow', 'facetCol', 'facetWrap', 'facetScale')

## scatter plot widgets
scatterWidgets <- reactive({
  if (is.null(input$showAesWgts)) return()
  if (is.null(input$showXYRangeWgts)) return()
  
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts) 
    wgts <- c(wgts, 'color', 'treatAsFacVarCol', 'shape', 'size', 'smooth', 'jitter', 'sizeMag')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## line plot widgets
lineWidgets <- reactive({
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts)
    wgts <- c(wgts, 'color')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## line plot points overlay widgets
linePtsOverlayWidgets <- reactive({
  wgts <- c('shape', 'size', 'smooth', 'jitter', 'ptsOverlayCond')
  wgts
})

## bar plot widgets
barWidgets <- reactive({
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill', 'position')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

# ## histogram widgets
histogramWidgets <- reactive({
  wgts <- c('plotType', 'x')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill', 'position', 'binWidth')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## density plot widgets
densityWidgets <- reactive({
  wgts <- c('plotType', 'x')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill', 'color', 'densBlkLineCond')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## box plot widgets
boxWidgets <- reactive({
  wgts <- c('plotType', 'x', 'y')
  if (input$showAesWgts)
    wgts <- c(wgts, 'fill')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## path plot widgets
pathWidgets <- reactive({
  wgts <- c('plotType', 'x', 'y')
  if (input$showXYRangeWgts)
    wgts <- c(wgts, 'xlim', 'ylim')
  if (input$showFacetWgts)
    wgts <- c(wgts, facetWidgets)
  wgts
})

## path plot points overlay widgets loaded
pathPtsOverlayWidgets <- reactive({
  wgts <- c('shape', 'size', 'ptsOverlayCond')
  wgts
})