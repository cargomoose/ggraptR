# if we abandon the approach and keep the default first value of selectInput list
# then, e.g., we will lost any input state after collapsing aestetic elements block

color_sel <- reactive({
  if (!is.null(colorOrig()) && !is.null(colOpts()) && colorOrig() %in% colOpts()) {
      colorOrig()
  }
})

fill_sel <- reactive({
  if (!is.null(fillOrig()) && !is.null(fillOpts()) && fillOrig() %in% fillOpts()) {
      fillOrig()
  }
})

size_sel <- reactive({
  if (!is.null(sizeOrig()) && !is.null(sizeOpts()) && sizeOrig() %in% sizeOpts()) {
      sizeOrig()
  }
})

shape_sel <- reactive({
  if (!is.null(shapeOrig()) && !is.null(shapeOpts()) && shapeOrig() %in% shapeOpts()) {
      shapeOrig()
  }
})

facetRow_sel <- reactive({
  if (!is.null(facetRowOrig()) && !is.null(facetOpts()) && facetRowOrig() %in% facetOpts()) {
    facetRowOrig()
  }
})

facetCol_sel <- reactive({
  if (!is.null(facetColOrig()) && !is.null(facetOpts()) && facetColOrig() %in% facetOpts()) {
      facetColOrig()
  }
})

facetWrap_sel <- reactive({
  if (!is.null(facetWrapOrig()) && !is.null(facetOpts()) && facetWrapOrig() %in% facetOpts()) {
      facetWrapOrig()
  }
})

plotAddAggBy_sel <- reactive({
  if (!is.null(plotAddAggBy()) && !is.null(plotAddAggByOpts()) 
      && all(plotAddAggBy() %in% plotAddAggByOpts())) {
      plotAddAggBy()
  }
})
