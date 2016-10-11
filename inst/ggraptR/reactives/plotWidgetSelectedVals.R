## x selected
x_sel <- reactive({
  if (is.null(input$dataset)) return()
  
  if (!is.null(x()) && !is.null(colnamesOpts()) && x() %in% colnamesOpts()) {
      return(x())
  }
  
  default_choice <- list(diamonds='carat',
                         mtcars='mpg',
                         rock='area',
                         iris='Pental.Length')
  return(default_choice[[input$dataset]])
})

## y selected
y_sel <- reactive({
  if (is.null(input$dataset)) return()
  if (!is.null(yOrig()) && !is.null(colnamesOpts()) && yOrig() %in% colnamesOpts()) {
      return(yOrig())
  }
      
  default_choice <- list(diamonds='price',
                         mtcars='hp',
                         rock='peri',
                         iris='Sepal.Width')
  return(default_choice[[input$dataset]])
})

## pairsPlot columns selected
columns_sel <- reactive({
  if (is.null(input$dataset)) return()
  if (!is.null(columns()) && !is.null(colnamesOpts()) && columns() %in% colnamesOpts()) {
      return(columns())
  }
  
  default_choice <- list(diamonds=c('cut', 'price'),
                         mtcars=c('hp', 'mpg', 'cyl'),
                         rock=c("peri", "shape", "area", "perm"),
                         iris=c('Sepal.Length', 'Species'))
  return(default_choice[[input$dataset]])
})

## color selected
color_sel <- reactive({
  if (!is.null(colorOrig()) && !is.null(colOpts()) && colorOrig() %in% colOpts()) {
      colorOrig()
  }
})

# ## treat-as-a-factor-variable option for color
# output$treatAsFacVarColCtrl <- renderUI({
#   if (is.null(displayTreatAsFacVarColCond())) return()
#   if (displayTreatAsFacVarColCond()) {
#     checkboxInput('treatAsFacVarCol', 'Treat as a factor variable', value=treatAsFacVarColOrig())
#   }
# })

## fill selected
fill_sel <- reactive({
  if (!is.null(fillOrig()) && !is.null(fillOpts()) && fillOrig() %in% fillOpts()) {
      fillOrig()
  }
})

## size selected
size_sel <- reactive({
  if (!is.null(sizeOrig()) && !is.null(sizeOpts()) && sizeOrig() %in% sizeOpts()) {
      sizeOrig()
  }
})

## shape selected
shape_sel <- reactive({
  if (!is.null(shapeOrig()) && !is.null(shapeOpts()) && shapeOrig() %in% shapeOpts()) {
      shapeOrig()
  }
})

## facet row selected
facetRow_sel <- reactive({
  if (!is.null(facetRowOrig()) && !is.null(facetOpts()) && facetRowOrig() %in% facetOpts()) {
    facetRowOrig()
  }
})

## facet col selected
facetCol_sel <- reactive({
  if (!is.null(facetColOrig()) && !is.null(facetOpts()) && facetColOrig() %in% facetOpts()) {
      facetColOrig()
  }
})

## facet wrap selected
facetWrap_sel <- reactive({
  if (!is.null(facetWrapOrig()) && !is.null(facetOpts()) && facetWrapOrig() %in% facetOpts()) {
      facetWrapOrig()
  }
})

## plot addition aggregation-by
plotAddAggBy_sel <- reactive({
  if (!is.null(plotAddAggBy()) && !is.null(plotAddAggByOpts()) 
      && all(plotAddAggBy() %in% plotAddAggByOpts())) {
      plotAddAggBy()
  }
})
