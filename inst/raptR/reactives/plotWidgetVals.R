#### current widget values

## dataset name
datasetName <- reactive({
  if (is.null(input$dataset)) return()
  inpu$datasetName
})

## plot type 
plotType <- reactive({
  if (is.null(input$plotType)) return()
  input$plotType
})

## x
x <- reactive({
  if (is.null(input$x)) return()
  input$x
})

## y original 
yOrig <- reactive({
  if (is.null(input$y)) return()
  input$y
})

## y 
y <- reactive({
  if (is.null(input$y)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(finalDF())) return()
  if (is.null(semiAutoAggOn())) return()
  retval <- ensureProperVarName(colnames=colnames(finalDF()), var=input$y, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  retval
})

## color original 
colorOrig <- reactive({
  if (is.null(input$color)) return()
  input$color
})

## color 
color <- reactive({
  if (is.null(plotDF())) return()
  if (is.null(input$color)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  col <- ensureProperVarName(colnames=colnames(plotDF()), var=input$color, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())  
  col <- convertNoneToNULL(col)
  col
})

## color as factor
colorAsFactor <- reactive({
  if (is.null(color())) return()
  varNameAsFactorOrNULL(color())
})

## treat as factor variable (for color)
treatAsFacVarCol <- reactive({
  if (is.null(input$treatAsFacVarCol)) return(FALSE)
  input$treatAsFacVarCol
})

## size original 
sizeOrig <- reactive({
  if (is.null(input$size)) return()
  input$size
})

## size 
size <- reactive({
  if (is.null(plotDF())) return()
  if (is.null(input$size)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  sz <- ensureProperVarName(colnames=colnames(plotDF()), var=input$size, aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  sz <- convertNoneToNULL(sz)
  sz
})

## fill original
fillOrig <- reactive({
  if (is.null(input$fill)) return()
  input$fill
})

## fill 
fill <- reactive({
  if (is.null(input$fill)) return()
  convertNoneToNULL(input$fill)
})

## fill as factor
fillAsFactor <- reactive({
  if (is.null(fill())) return()
  varNameAsFactorOrNULL(fill())  
})

## position 
position <- reactive({
  if (is.null(input$position)) return()
  convertNoneToNULL(input$position)
})

## jitter original
jitterOrig <- reactive({
  if (is.null(input$jitter)) return()
  input$jitter
})

## jitter 
## should return either "jitter" or NULL
## 07/01/2016 - jitter has become broken. for now we will always jitter
jitter <- reactive({
  if (is.null(input$jitter)) return('jitter')
  jit <- input$jitter
  if (jit) 
    jit <- 'jitter' 
  else 
    jit <- 'jitter'
  jit
})

## alpha original 
alphaOrig <- reactive({
  if (is.null(input$alpha)) return()
  input$alpha
})

## alpha 
alpha <- reactive({
  if (is.null(input$alpha)) return(1)
  input$alpha
})

## size magnifier original
sizeMagOrig <- reactive({
  if (is.null(input$sizeMag)) return()
  input$sizeMag
})

## size magnifier 
sizeMag <- reactive({
  if (is.null(input$sizeMag)) return(4)
  input$sizeMag
})

## density black line condition
densBlkLineCond <- reactive({
  if (is.null(input$densBlkLineCond)) return(FALSE)
  input$densBlkLineCond
})

## shape original 
shapeOrig <- reactive({
  if (is.null(input$shape)) return()
  input$shape
})

## shape
shape <- reactive({
  if (is.null(input$shape)) return()
  convertNoneToNULL(input$shape)
})

## shape as factor
shapeAsFactor <- reactive({
  if (is.null(shape())) return()
  varNameAsFactorOrNULL(shape())
})

## smooth original 
smoothOrig <- reactive({
  if (is.null(input$smooth)) return()
  input$smooth
})

## smooth
smooth <- reactive({
  if (is.null(input$smooth)) return()
  convertNoneToNULL(input$smooth)
})

## coordinate flip
coordFlip <- reactive({
  if (is.null(input$coordFlip)) return(FALSE)
  input$coordFlip
})

## bin width
binWidth <- reactive({
  if (is.null(input$binWidth)) return()
  input$binWidth
})

## points overlay condition
ptsOverlayCond <- reactive({
  if (is.null(input$ptsOverlayCond)) return(FALSE)
  input$ptsOverlayCond
})

## facet row original 
facetRowOrig <- reactive({
  if (is.null(input$facetRow)) return()
  input$facetRow
})

## facet row
facetRow <- reactive({
  if (is.null(plotDF()) | is.null(input$facetRow)) return('.')
  fr <- ifelse(input$facetRow=='None', '.', input$facetRow)
  if (fr != '.' & fr %in% colnames(plotDF())) return(fr)
  else return('.')
})

## facet col original 
facetColOrig <- reactive({
  if (is.null(input$facetCol)) return()
  input$facetCol
})

## facet column
facetCol <- reactive({
  if (is.null(plotDF()) | is.null(input$facetCol)) return('.')
  fc <- ifelse(input$facetCol=='None', '.', input$facetCol)
  if (fc != '.' & fc %in% colnames(plotDF())) return(fc)
  else return('.')
}) 

## facet wrap original 
facetWrapOrig <- reactive({
  if (is.null(input$facetWrap)) return()
  input$facetWrap
})

## facet wrap
facetWrap <- reactive({
  if (is.null(plotDF()) | is.null(input$facetWrap)) return('.')
  fw <- ifelse(input$facetWrap=='None', '.', input$facetWrap)
  if (fw != '.' & fw %in% colnames(plotDF())) return(fw)
  else return('.')
})

## facet scale
facetScale <- reactive({
  if (is.null(input$facetScale)) return('none')
  input$facetScale
})

## facet grids
facetGrids <- reactive({
  if (is.null(facetRow()) | is.null(facetCol())) return('. ~ .')
  paste(facetRow(), '~', facetCol())
})

## xlim
xlim <- reactive({
  if (is.null(input$xlim)) return()
  input$xlim
})

## ylim
ylim <- reactive({
  if (is.null(input$ylim)) return()
  input$ylim
})





## plot title
plotTitle <- reactive({
  if (is.null(input$plotTitle)) return()
  input$plotTitle
})

## x label
xLabel <- reactive({
  if (is.null(input$xLabel)) return()
  input$xLabel
})

## y label
yLabel <- reactive({
  if (is.null(input$yLabel)) return()
  input$yLabel
})

## label font family
labelFontFamily <- reactive({
  if (is.null(input$labelFontFamily)) return()
  input$labelFontFamily
})

## label font family
labelFontFace <- reactive({
  if (is.null(input$labelFontFace)) return()
  input$labelFontFace
})

## label font size
labelFontSize <- reactive({
  if (is.null(input$labelFontSize)) return(15)
  input$labelFontSize
})

## label font color
labelFontColor <- reactive({
  if (is.null(input$labelFontColor)) return('black')
  input$labelFontColor
})

## hjust
hjust <- reactive({
  if (is.null(input$hjust)) return(0.5)
  input$hjust
})

## vjust
vjust <- reactive({
  if (is.null(input$vjust)) return(0.5)
  input$vjust
})

## plot theme
plotTheme <- reactive({
  if (is.null(input$plotTheme)) return('theme_grey')
  input$plotTheme
})


## plot agg method
plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) return('none')
  input$plotAggMeth
})

## raw or man agg dataset type
rawVsManAgg <- reactive({
  if (is.null(input$rawVsManAgg)) return()
  input$rawVsManAgg
})

## plot additional aggregation-by
plotAddAggBy <- reactive({
  if (is.null(input$plotAddAggBy)) return()
  input$plotAddAggBy
})




