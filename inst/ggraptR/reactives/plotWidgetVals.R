#### current widget values

## dataset name
# datasetName <- reactive({
#   if (is.null(input$dataset)) return()
#   input$datasetName
# })

## plot type 
plotType <- reactive({
  if (!is.null(input$plotType)) {
    input$plotType
  }
})

## x
x <- reactive({
  if (!is.null(input$x)) {
    input$x
  }
})

## y original 
yOrig <- reactive({
  if (!is.null(input$y)) {
    input$y
  }
})

## y original 
columns <- reactive({
  if (!is.null(input$columns)) {
    input$columns
  }
})

## y 
y <- reactive({
  if (!is.null(input$y) && !is.null(plotAggMeth()) && !is.null(finalDF())
      && !is.null(semiAutoAggOn())) {
    ensureProperVarName(colnames=colnames(finalDF()), var=input$y, 
                        aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  }
})

## color original 
colorOrig <- reactive({
  if (!is.null(input$color)) {
    input$color
  }
})

## color 
color <- reactive({
  if (!is.null(plotDF()) && !is.null(input$color) && !is.null(plotAggMeth())
      && !is.null(semiAutoAggOn())) {
    col <- ensureProperVarName(colnames=colnames(plotDF()), var=input$color, 
                               aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())  
    convertNoneToNULL(col)
  }
})

## color as factor
colorAsFactor <- reactive({
  if (!is.null(color())) {
    varNameAsFactorOrNULL(color())
  }
})

## treat as factor variable (for color)
treatAsFacVarCol <- reactive({
  if (is.null(input$treatAsFacVarCol)) FALSE else input$treatAsFacVarCol
})

## size original 
sizeOrig <- reactive({
  if (!is.null(input$size)) {
    input$size
  }
})

## size 
size <- reactive({
  if (!is.null(plotDF()) && !is.null(input$size) && !is.null(plotAggMeth())
      && !is.null(semiAutoAggOn())) {
    sz <- ensureProperVarName(colnames=colnames(plotDF()), var=input$size, 
                              aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
    convertNoneToNULL(sz)
  }
})

## fill original
fillOrig <- reactive({
  if (!is.null(input$fill)) {
    input$fill
  }
})

## fill 
fill <- reactive({
  if (!is.null(input$fill)) {
    convertNoneToNULL(input$fill)
  }
})

## fill as factor
fillAsFactor <- reactive({
  if (!is.null(fill())) {
    varNameAsFactorOrNULL(fill())  
  }
})

## position 
position <- reactive({
  if (!is.null(input$position)) {
    convertNoneToNULL(input$position)
  }
})

## jitter original
jitterOrig <- reactive({
  if (!is.null(input$jitter)) {
    input$jitter
  }
})

## jitter
jitter <- reactive({
  if (!is.null(input$jitter) && input$jitter) 'jitter' else 'identity'
})

## alpha original 
alphaOrig <- reactive({
  if (!is.null(input$alpha)) {
    input$alpha
  }
})

## alpha 
alpha <- reactive({
  if (is.null(input$alpha)) 1 else input$alpha
})

## size magnifier original
sizeMagOrig <- reactive({
  if (!is.null(input$sizeMag)) {
    input$sizeMag
  }
})

## size magnifier 
sizeMag <- reactive({
  if (is.null(input$sizeMag)) 4 else input$sizeMag
})

## density black line condition
densBlkLineCond <- reactive({
  if (is.null(input$densBlkLineCond)) FALSE else input$densBlkLineCond
})

## shape original 
shapeOrig <- reactive({
  if (!is.null(input$shape)) {
    input$shape
  }
})

## shape
shape <- reactive({
  if (!is.null(input$shape)) {
    convertNoneToNULL(input$shape)
  }
})

## shape as factor
shapeAsFactor <- reactive({
  if (!is.null(shape())) {
    varNameAsFactorOrNULL(shape())
  }
})

## smooth original 
smoothOrig <- reactive({
  if (!is.null(input$smooth)) {
    input$smooth
  }
})

## smooth
smooth <- reactive({
  if (!is.null(input$smooth)) {
    convertNoneToNULL(input$smooth)
  }
})

## coordinate flip
coordFlip <- reactive({
  if (is.null(input$coordFlip)) FALSE else input$coordFlip
})

## bin width
binWidth <- reactive({
  if (!is.null(input$binWidth)) {
    input$binWidth
  }
})

## points overlay condition
ptsOverlayCond <- reactive({
  if (is.null(input$ptsOverlayCond)) FALSE else input$ptsOverlayCond
})

## facet row original 
facetRowOrig <- reactive({
  if (!is.null(input$facetRow)) {
    input$facetRow
  }
})

## facet row
facetRow <- reactive({
  if (is.null(plotDF()) || is.null(input$facetRow)) return('.')
  fr <- ifelse(input$facetRow=='None', '.', input$facetRow)
  if (fr != '.' && fr %in% colnames(plotDF())) fr else '.'
})

## facet col original 
facetColOrig <- reactive({
  if (!is.null(input$facetCol)) {
    input$facetCol
  }
})

## facet column
facetCol <- reactive({
  if (is.null(plotDF()) || is.null(input$facetCol)) return('.')
  fc <- ifelse(input$facetCol=='None', '.', input$facetCol)
  if (fc != '.' && fc %in% colnames(plotDF())) fc else '.'
}) 

## facet wrap original 
facetWrapOrig <- reactive({
  if (!is.null(input$facetWrap)) {
    input$facetWrap
  }
})

## facet wrap
facetWrap <- reactive({
  if (is.null(plotDF()) || is.null(input$facetWrap)) return('.')
  fw <- ifelse(input$facetWrap=='None', '.', input$facetWrap)
  if (fw != '.' && fw %in% colnames(plotDF())) fw else '.'
})

## facet scale
facetScale <- reactive({
  if (is.null(input$facetScale)) 'none' else input$facetScale
})

## facet grids
facetGrids <- reactive({
  if (is.null(facetRow()) || is.null(facetCol())) '. ~ .' else 
    paste(facetRow(), '~', facetCol())
})

## xlim
xlim <- reactive({
  if (!is.null(input$xlim)) {
    input$xlim
  }
})

## ylim
ylim <- reactive({
  if (!is.null(input$ylim)) {
    input$ylim
  }
})





## plot title
plotTitle <- reactive({
  if (!is.null(input$plotTitle)) {
    input$plotTitle
  }
})

## x label
xLabel <- reactive({
  if (!is.null(input$xLabel)) {
    input$xLabel
  }
})

## y label
yLabel <- reactive({
  if (!is.null(input$yLabel)) {
    input$yLabel
  }
})

## label font family
labelFontFamily <- reactive({
  if (!is.null(input$labelFontFamily)) {
    input$labelFontFamily
  }
})

## label font family
labelFontFace <- reactive({
  if (!is.null(input$labelFontFace)) {
    input$labelFontFace
  }
})

## label font size
labelFontSize <- reactive({
  if (is.null(input$labelFontSize)) 15 else input$labelFontSize
})

## label font color
labelFontColor <- reactive({
  if (is.null(input$labelFontColor)) 'black' else input$labelFontColor
})

## hjust
hjust <- reactive({
  if (is.null(input$hjust)) 0.5 else input$hjust
})

## vjust
vjust <- reactive({
  if (is.null(input$vjust)) 0.5 else input$vjust
})

## plot theme
plotTheme <- reactive({
  if (is.null(input$plotTheme)) 'theme_grey' else input$plotTheme
})


## plot agg method
plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) 'none' else input$plotAggMeth
})

## raw or man agg dataset type
rawVsManAgg <- reactive({
  if (!is.null(input$rawVsManAgg)) {
    input$rawVsManAgg
  }
})

## plot additional aggregation-by
plotAddAggBy <- reactive({
  if (!is.null(input$plotAddAggBy)) {
    input$plotAddAggBy
  }
})

