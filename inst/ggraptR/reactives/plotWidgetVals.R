plotType <- reactive({
  plotLoading$status <- F
  if (!is.null(input$plotType)) plotLoading$itersToDraw <- 5
  input$plotType
})

x <- reactive({
  input$x
})

xAsFactor <- reactive({
  varNameAsFactorOrNULL(x())
})

yOrig <- reactive({
  input$y
})

y <- reactive({
  ensureProperVarName(colnames(isolate(finalDF())), yOrig(), 
                      isolate(plotAggMeth()), isolate(semiAutoAggOn()))
})

# pairsPlot
columns <- reactive({
  input$columns
})

colorOrig <- reactive({
  input$color
})

color <- reactive({
  convertNoneToNULL(ensureProperVarName(
    isolate(colnames(plotDF())), colorOrig(), 
            isolate(plotAggMeth()), isolate(semiAutoAggOn())))
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
  convertNoneToNULL(ensureProperVarName(
    isolate(colnames(plotDF())), sizeOrig(), 
    isolate(plotAggMeth()), isolate(semiAutoAggOn())))
})

fillOrig <- reactive({
  input$fill
})

fill <- reactive({
 convertNoneToNULL(fillOrig())
})

fillAsFactor <- reactive({
  varNameAsFactorOrNULL(fill())
})

position <- reactive({
  convertNoneToNULL(input$position)
})

jitter <- reactive({
  if (!is.null(input$jitter) && input$jitter 
      && !plotType() %in% c('line', 'path')) 'jitter' else 'identity'
})

sizeMagOrig <- reactive({
  input$sizeMag
})

sizeMag <- reactive({
  if (is.null(input$sizeMag)) 4 else input$sizeMag
})

alphaOrig <- reactive({
  input$alpha
})

alpha <- reactive({
  if (is.null(alphaOrig())) 1 else alphaOrig()
})

densBlkLineCond <- reactive({
  if (!isolate(displayDensBlkLineCond())) NULL else input$densBlkLineCond
})

shapeOrig <- reactive({
  input$shape
})

shape <- reactive({
  convertNoneToNULL(shapeOrig())
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
  if (is.null(input$coordFlip)) FALSE else input$coordFlip  #####
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
  if (isolate(!displayPtsOverlayCond())) NULL else input$ptsOverlayCond
})

facetRowOrig <- reactive({
  input$facetRow
})

facetRow <- reactive({
  plotDF <- isolate(plotDF())  #####
  if (anyNull(plotDF, input$facetRow)) return('.')
  fr <- ifelse(input$facetRow=='None', '.', input$facetRow)
  if (fr != '.' && fr %in% colnames(plotDF)) fr else '.'
})

facetColOrig <- reactive({
  input$facetCol
})

facetCol <- reactive({
  plotDF <- isolate(plotDF())  #####
  if (anyNull(plotDF, input$facetCol)) return('.')
  fc <- ifelse(input$facetCol=='None', '.', input$facetCol)
  if (fc != '.' && fc %in% colnames(plotDF)) fc else '.'
}) 

facetWrapOrig <- reactive({
  input$facetWrap
})

facetWrap <- reactive({
  plotDF <- isolate(plotDF())  #####
  if (anyNull(plotDF, input$facetWrap)) return('.')
  fw <- ifelse(input$facetWrap=='None', '.', input$facetWrap)
  if (fw != '.' && fw %in% colnames(plotDF)) fw else '.'
})

facetScale <- reactive({
  if (is.null(input$facetScale)) 'none' else input$facetScale
})

facetGrids <- reactive({  ####
  row <- isolate(facetRow())
  col <- isolate(facetCol())
  if (anyNull(row, col)) '. ~ .' else paste(row, '~', col)
})

xlim <- reactive({
  if (isolate(!displayXlim())) NULL else input$xlim
})

ylim <- reactive({
  if (isolate(!displayYlim())) NULL else input$ylim
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

showXYRangeWgts <- reactive({
  !is.null(input$showXYRangeWgts) && input$showXYRangeWgts
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

showAesWgts <- reactive({
  !is.null(input$showAesWgts) && input$showAesWgts
})

showFacetWgts <- reactive({
  !is.null(input$showFacetWgts) && input$showFacetWgts
})

pointsOverlay <- reactive({
  !is.null(input$pointsOverlay) && input$pointsOverlay
})


