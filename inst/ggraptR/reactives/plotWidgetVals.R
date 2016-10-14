#### current widget values
plotType <- reactive({
  input$plotType
})

x <- reactive({
  input$x
})

xAsFactor <- reactive({
  varNameAsFactorOrNULL(input$x)
})

yOrig <- reactive({
  input$y
})

## columns for pairsPlot
columns <- reactive({
  input$columns
})

y <- reactive({
  if (!is.null(input$y) && !is.null(plotAggMeth()) && !is.null(finalDF())
      && !is.null(semiAutoAggOn())) {
    ensureProperVarName(colnames=colnames(finalDF()), var=input$y, 
                        aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
  }
})

colorOrig <- reactive({
  input$color
})

color <- reactive({
  if (!is.null(plotDF()) && !is.null(input$color) && !is.null(plotAggMeth())
      && !is.null(semiAutoAggOn())) {
    col <- ensureProperVarName(colnames=colnames(plotDF()), var=input$color, 
                               aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())  
    convertNoneToNULL(col)
  }
})

colorAsFactor <- reactive({
  varNameAsFactorOrNULL(color())
})

treatAsFacVarCol <- reactive({
  if (is.null(input$treatAsFacVarCol)) FALSE else input$treatAsFacVarCol
})

sizeOrig <- reactive({
  input$size
})

size <- reactive({
  if (!is.null(plotDF()) && !is.null(input$size) && !is.null(plotAggMeth())
      && !is.null(semiAutoAggOn())) {
    sz <- ensureProperVarName(colnames=colnames(plotDF()), var=input$size, 
                              aggMeth=plotAggMeth(), semiAutoAggOn=semiAutoAggOn())
    convertNoneToNULL(sz)
  }
})

fillOrig <- reactive({
  input$fill
})

fill <- reactive({
 convertNoneToNULL(input$fill)
})

fillAsFactor <- reactive({
  varNameAsFactorOrNULL(fill())
})

position <- reactive({
  convertNoneToNULL(input$position)
})

jitterOrig <- reactive({
  input$jitter
})

jitter <- reactive({
  if (!is.null(input$jitter) && input$jitter) 'jitter' else 'identity'
})

alphaOrig <- reactive({
  input$alpha
})

alpha <- reactive({
  if (is.null(input$alpha)) 1 else input$alpha
})

sizeMagOrig <- reactive({
  input$sizeMag
})

sizeMag <- reactive({
  if (is.null(input$sizeMag)) 4 else input$sizeMag
})

densBlkLineCond <- reactive({
  if (is.null(input$densBlkLineCond)) FALSE else input$densBlkLineCond
})

shapeOrig <- reactive({
  input$shape
})

shape <- reactive({
  convertNoneToNULL(input$shape)
})

shapeAsFactor <- reactive({
  varNameAsFactorOrNULL(shape())
})

smoothOrig <- reactive({
  input$smooth
})

smooth <- reactive({
  convertNoneToNULL(input$smooth)
})

coordFlip <- reactive({
  if (is.null(input$coordFlip) || !displayCoordFlipCond()) FALSE else input$coordFlip
})


ggpairsUpCont <- reactive({
  input$ggpairsUpCont
})

ggpairsUpCombo <- reactive({
  input$ggpairsUpCombo
})

ggpairsUpDiscr <- reactive({
  input$ggpairsUpDiscr
})

ggpairsDiagCont <- reactive({
  input$ggpairsDiagCont
})

ggpairsDiagDiscr <- reactive({
  input$ggpairsDiagDiscr
})

ggpairsLowCont <- reactive({
  input$ggpairsLowCont
})

ggpairsLowCombo <- reactive({
  input$ggpairsLowCombo
})

ggpairsLowDiscr <- reactive({
  input$ggpairsLowDiscr
})


binWidth <- reactive({
  input$binWidth
})

ptsOverlayCond <- reactive({
  if (is.null(input$ptsOverlayCond)) FALSE else input$ptsOverlayCond
})

facetRowOrig <- reactive({
  input$facetRow
})

facetRow <- reactive({
  if (is.null(plotDF()) || is.null(input$facetRow)) return('.')
  fr <- ifelse(input$facetRow=='None', '.', input$facetRow)
  if (fr != '.' && fr %in% colnames(plotDF())) fr else '.'
})

facetColOrig <- reactive({
  input$facetCol
})

facetCol <- reactive({
  if (is.null(plotDF()) || is.null(input$facetCol)) return('.')
  fc <- ifelse(input$facetCol=='None', '.', input$facetCol)
  if (fc != '.' && fc %in% colnames(plotDF())) fc else '.'
}) 

facetWrapOrig <- reactive({
  input$facetWrap
})

facetWrap <- reactive({
  if (is.null(plotDF()) || is.null(input$facetWrap)) return('.')
  fw <- ifelse(input$facetWrap=='None', '.', input$facetWrap)
  if (fw != '.' && fw %in% colnames(plotDF())) fw else '.'
})

facetScale <- reactive({
  if (is.null(input$facetScale)) 'none' else input$facetScale
})

facetGrids <- reactive({
  if (is.null(facetRow()) || is.null(facetCol())) '. ~ .' else 
    paste(facetRow(), '~', facetCol())
})

xlim <- reactive({
  if (!is.null(displayXlim()) && displayXlim() && !is.null(input$xlim)) {
    input$xlim
  }
})

ylim <- reactive({
  if (!is.null(displayYlim()) && displayYlim() && !is.null(input$ylim)) {
    input$ylim
  }
})



plotTitle <- reactive({
  input$plotTitle
})

xLabel <- reactive({
  input$xLabel
})

yLabel <- reactive({
  input$yLabel
})

labelFontFamily <- reactive({
  input$labelFontFamily
})

labelFontFace <- reactive({
  input$labelFontFace
})

labelFontSize <- reactive({
  if (is.null(input$labelFontSize)) 15 else input$labelFontSize
})

labelFontColor <- reactive({
  if (is.null(input$labelFontColor)) 'black' else input$labelFontColor
})

hjust <- reactive({
  if (is.null(input$hjust)) 0.5 else input$hjust
})

vjust <- reactive({
  if (is.null(input$vjust)) 0.5 else input$vjust
})

plotTheme <- reactive({
  if (is.null(input$plotTheme)) 'theme_grey' else input$plotTheme
})


plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) 'none' else input$plotAggMeth
})

rawVsManAgg <- reactive({
  input$rawVsManAgg
})

plotAddAggBy <- reactive({
  input$plotAddAggBy
})

