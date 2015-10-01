#### file paths to cache widget values
datasetNameCacheFilePath <- './_input_cache/datasetName'
plotTypeCacheFilePath <- './_input_cache/plotType'
plotAggMethCacheFilePath <- './_input_cache/plotAggMeth'
xCacheFilePath <- './_input_cache/x'
yCacheFilePath <- './_input_cache/y'
colorCacheFilePath <- './_input_cache/color'
treatAsFacVarColCacheFilePath <- './_input_cache/treatAsFacVarCol'
sizeCacheFilePath <- './_input_cache/size'
fillCacheFilePath <- './_input_cache/fill'
positionCacheFilePath <- './_input_cache/position'
jitterCacheFilePath <- './_input_cache/jitter'
alphaCacheFilePath <- './_input_cache/alpha'
sizeMagCacheFilePath <- './_input_cache/sizeMag'
densBlkLineCondCacheFilePath <- './_input_cache/densBlkLineCond'
shapeCacheFilePath <- './_input_cache/shape'
smoothCacheFilePath <- './_input_cache/smooth'
coordFlipCacheFilePath <- './_input_cache/coordFlip'
binWidthCacheFilePath <- './_input_cache/binWidth'
ptsOverlayCondCacheFilePath <- './_input_cache/ptsOverlayCond'
facetRowCacheFilePath <- './_input_cache/facetRow'
facetColCacheFilePath <- './_input_cache/facetCol'
facetWrapCacheFilePath <- './_input_cache/facetWrap'
facetScaleCacheFilePath <- './_input_cache/facetScale'
xlimCacheFilePath <- './_input_cache/xlim'
ylimCacheFilePath <- './_input_cache/ylim'


#### current widget values

## dataset name
datasetName <- reactive({
  if (is.null(input$dataset)) return()
  inpu$datasetName
})

## plot type 
plotType <- reactive({
  if (is.null(input$plotType)) return()
  dput(input$plotType, file=plotTypeCacheFilePath)
  input$plotType
})

## plot agg method
plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) return('none')
  dput(input$plotAggMeth, file=plotAggMethCacheFilePath)
  input$plotAggMeth
})

## x
x <- reactive({
  if (is.null(input$x)) return()
  dput(input$x, file=xCacheFilePath)
  input$x
})

## y (XXX)
y <- reactive({
  if (is.null(input$y)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(finalDF())) return()
  if (is.null(semiAutoAggOn())) return()
  dput(input$y, file=yCacheFilePath)
  retval <- ensureProperVarName(colnames=colnames(finalDF()), var=input$y, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())
  retval
})

## color (XXX)
color <- reactive({
  dataset <- plotDF()
  if (is.null(dataset)) return()
  if (is.null(input$color)) return()
  if (is.null(plotAggMeth())) return()
  if (is.null(semiAutoAggOn())) return()
  dput(input$color, file=colorCacheFilePath)
  col <- ensureProperVarName(colnames=colnames(dataset), var=input$color, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())  
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
  dput(input$treatAsFacVarCol, file=treatAsFacVarColCacheFilePath)
  input$treatAsFacVarCol
})

## size (XXX)
size <- reactive({
  dataset <- plotDF()
  if (is.null(dataset)) return()
  if (is.null(input$size)) return()
  if (is.null(input$plotAggMeth)) return()
  if (is.null(semiAutoAggOn())) return()
  dput(input$size, file=sizeCacheFilePath)
  sz <- ensureProperVarName(colnames=colnames(dataset), var=input$size, aggMeth=input$plotAggMeth, semiAutoAggOn=semiAutoAggOn())
  sz <- convertNoneToNULL(sz)
  sz
})

## fill (XXX)
fill <- reactive({
  if (is.null(input$fill)) return()
  dput(input$fill, file=fillCacheFilePath)
  convertNoneToNULL(input$fill)
})

## fill as factor
fillAsFactor <- reactive({
  if (is.null(fill())) return()
  varNameAsFactorOrNULL(fill())  
})

## position (XXX)
position <- reactive({
  if (is.null(input$position)) return('None')
  dput(input$position, file=positionCacheFilePath)
  convertNoneToNULL(input$position)
})

#### CONTINUE HERE!!!

## jitter (XXX)
## should return either "jitter" or NULL
jitter <- reactive({
  if (is.null(input$jitter)) return()
  dput(input$jiter, file=jitterCacheFilePath)
  
  jit <- input$jitter
  if (jit) 
    jit <- 'jitter' 
  else 
    jit <- NULL
  jit
})

## alpha (XXX)
alpha <- reactive({
  alp <- input$alpha
  if (is.null(alp)) alp <- 1
  alp
})

## size magnifier (XXX)
sizeMag <- reactive({
  sm <- input$sizeMag
  if (is.null(sm)) sm <- 4
  sm
})

#### FINISH HERE!!!!

## density black line condition
densBlkLineCond <- reactive({
  if (is.null(input$densBlkLineCond)) return(FALSE)
  dput(input$densBlkLineCond, file=densBlkLineCondCacheFilePath)
  input$densBlkLineCond
})

## shape
shape <- reactive({
  if (is.null(input$shape)) return()
  dput(input$shape, file=shapeCacheFilePath)
  convertNoneToNULL(input$shape)
})

## shape as factor
shapeAsFactor <- reactive({
  if (is.null(shape())) return()
  varNameAsFactorOrNULL(shape())
})

## smooth
smooth <- reactive({
  if (is.null(input$smooth)) return()
  dput(input$smooth, file=smoothCacheFilePath)
  convertNoneToNULL(input$smooth)
})

## coordinate flip
coordFlip <- reactive({
  if (is.null(input$coordFlip)) return(FALSE)
  dput(input$coordFlip, file=coordFlipCacheFilePath)
  input$coordFlip
})

## bin width
binWidth <- reactive({
  if (is.null(input$binWidth)) return()
  dput(input$binWidth, file=binWidthCacheFilePath)
  input$binWidth
})

## points overlay condition
ptsOverlayCond <- reactive({
  if (is.null(input$ptsOverlayCond)) return(FALSE)
  dput(input$ptsOverlayCond, file=ptsOverlayCondCacheFilePath)
  input$ptsOverlayCond
})

## facet row
facetRow <- reactive({
  if (is.null(input$facetRow)) return('.')
  dput(input$facetRow, file=facetRowCacheFilePath)
  ifelse(input$facetRow=='None', '.', input$facetRow)
})

## facet column
facetCol <- reactive({
  if (is.null(input$facetCol)) return('.')
  dput(input$facetCol, file=facetColCacheFilePath)
  ifelse(input$facetCol=='None', '.', input$facetCol)
})

## facet wrap
facetWrap <- reactive({
  if (is.null(input$facetWrap)) return('.')
  dput(input$facetWrap, file=facetWrapCacheFilePath)
  ifelse(input$facetWrap=='None', '.', input$facetWrap)
})

## facet scale
facetScale <- reactive({
  if (is.null(input$facetScale)) return('none')
  dput(input$facetScale, file=facetScaleCacheFilePath)
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
  dput(input$xlim, file=xlimCacheFilePath)
  input$xlim
})

## ylim
ylim <- reactive({
  if (is.null(input$ylim)) return()
  dput(input$ylim, file=ylimCacheFilePath)
  input$ylim
})





#### cached widget values
## dataset name cache
datasetName_cache <- reactive({
  if (file.exists(datasetNameCacheFilePath)) dget(datasetNameCacheFilePath)
})

## plot type cache
plotType_cache <- reactive({
  if (file.exists(plotTypeCacheFilePath)) dget(plotTypeCacheFilePath)
})

## plot agg method cache
plotAggMeth_cache <- reactive({
  if (is.null(input$plotAggMeth)) return()
  if (file.exists(plotAggMethCacheFilePath)) dget(plotAggMethCacheFilePath)
})

## x cache
x_cache <- reactive({
  if (is.null(input$x)) return()
  if (file.exists(xCacheFilePath)) dget(xCacheFilePath)
})

## y cache
y_cache <- reactive({
  if (is.null(input$y)) return()
  if (file.exists(yCacheFilePath)) dget(yCacheFilePath)
})

## color cache
color_cache <- reactive({
  if (is.null(input$color)) return()
  if (file.exists(colorCacheFilePath)) dget(colorCacheFilePath)
})

## treat as factor variable (for color) cache
treatAsFacVarCol_cache <- reactive({
  if (is.null(input$treatAsFacVarCol)) return()
  if (file.exists(treatAsFacVarColCacheFilePath)) dget(treatAsFacVarColCacheFilePath)
})

## size cache
size_cache <- reactive({
  if (is.null(input$size)) return()
  if (file.exists(sizeMagCacheFilePath)) dget(sizeMagCacheFilePath)
})

## fill cache
fill_cache <- reactive({
  if (is.null(input$fill)) return()
  if (file.exists(fillCacheFilePath)) dget(fillCacheFilePath)
})

## position cache
position_cache <- reactive({
  if (is.null(input$position)) return()
  if (file.exists(positionCacheFilePath)) dget(positionCacheFilePath)
})

## jitter cache
jitter_cache <- reactive({
  if (is.null(input$jitter)) return()
  if (file.exists(jitterCacheFilePath)) dget(jitterCacheFilePath)
})

## alpha cache
alpha_cache <- reactive({
  if (is.null(input$alpha)) return()
  if (file.exists(alphaCacheFilePath)) dget(alphaCacheFilePath)
})

## size magnifier cache
sizeMag_cache <- reactive({
  if (is.null(input$sizeMag)) return()
  if (file.exists(sizeMagCacheFilePath)) dget(sizeMagCacheFilePath)
})

## density black line condition cache
densBlkLineCond_cache <- reactive({
  if (is.null(input$densBlkLineCond)) return()
  if (file.exists(densBlkLineCondCacheFilePath)) dget(densBlkLineCondCacheFilePath)
})

## shape cache
shape_cache <- reactive({
  if (is.null(input$shape)) return()
  if (file.exists(shapeCacheFilePath)) dget(shapeCacheFilePath)
})

## smooth cache
smooth_cache <- reactive({
  if (is.null(input$smooth)) return()
  if (file.exists(smoothCacheFilePath)) dget(smoothCacheFilePath)
})

## coordinate flip cache
coordFlip_cache <- reactive({
  if (is.null(input$coordFlip)) return()
  if (file.exists(coordFlipCacheFilePath)) dget(coordFlipCacheFilePath)
})

## bin width cache
binWidth_cache <- reactive({
  if (is.null(input$binWidth)) return()
  if (file.exists(binWidthCacheFilePath)) dget(binWidthCacheFilePath)
})

## points overlay condition cache
ptsOverlayCond_cache <- reactive({
  if (is.null(input$ptsOverlayCond)) return()
  if (file.exists(ptsOverlayCondCacheFilePath)) dget(ptsOverlayCondCacheFilePath)
})

## facet row cache
facetRow_cache <- reactive({
  if (is.null(input$facetRow)) return()
  if (file.exists(facetRowCacheFilePath)) dget(facetRowCacheFilePath)
})

## facet column cache
facetCol_cache <- reactive({
  if (is.null(input$facetCol)) return()
  if (file.exists(facetColCacheFilePath)) dget(facetColCacheFilePath)
})

## facet wrap cache
facetWrap_cache <- reactive({
  if (is.null(input$facetWrap)) return()
  if (file.exists(facetWrapCacheFilePath)) dget(facetWrapCacheFilePath)
})

## facet scale cache
facetScale_cache <- reactive({
  if (is.null(input$facetScale)) return()
  if (file.exists(facetScaleCacheFilePath)) dget(facetScaleCacheFilePath)
})

## xlim cache
xlim_cache <- reactive({
  if (is.null(input$xlim)) return()
  if (file.exists(xlimCacheFilePath)) dget(xlimCacheFilePath)
})

## ylim cache
ylim_cache <- reactive({
  if (is.null(input$ylim)) return()
  if (file.exists(ylimCacheFilePath)) dget(ylimCacheFilePath)
})

