plotTypes <- reactive({
  input$plotTypes
})

plotTypeOpts <- reactive({
  reactVals$updatePlotTypeOpts  # updates using an observer
  selOpt <- isolate(plotTypes())
  opts <- unlist(pScheme[if (is.null(selOpt)) T else 
    sapply(pScheme, function(el) selOpt %in% el)])
  names(opts) <- sapply(opts, function(x) capitalize(x) %>% gsub('(\\d)', ' \\1', .))
  opts
})

x <- reactive({
  input$x
})

yOrig <- reactive({
  input$y
})

y <- reactive({
  ensureProperVarName(colnames(isolate(aggDf())), yOrig(), 
                      isolate(plotAggMeth()), semiAutoAggOn())
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
    isolate(colnames(aggLimDf())), colorOrig(), 
            isolate(plotAggMeth()), isolate(semiAutoAggOn())))
})

treatColorAsFactor <- reactive({
  !is.null(input$treatColorAsFactor) && input$treatColorAsFactor
})

sizeOrig <- reactive({
  input$size
})

size <- reactive({
  convertNoneToNULL(ensureProperVarName(
    isolate(colnames(aggLimDf())), sizeOrig(), 
    isolate(plotAggMeth()), isolate(semiAutoAggOn())))
})

fillOrig <- reactive({
  input$fill
})

fill <- reactive({
 convertNoneToNULL(fillOrig())
})

position <- reactive({
  convertNoneToNULL(input$position)
})

jitter <- reactive({
  if (!is.null(input$jitter) && input$jitter) 'jitter' else 'identity'
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

shapeOrig <- reactive({
  input$shape
})

shape <- reactive({
  convertNoneToNULL(shapeOrig())
})

smoothOrig <- reactive({
  input$smooth
})

smooth <- reactive({
  convertNoneToNULL(input$smooth)
})

coordFlip <- reactive({
  !is.null(input$coordFlip) && input$coordFlip
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

nBins <- reactive({
  input$nBins
})

densBlackLine <- reactive({
  input$densBlackLine
})


facetRowOrig <- reactive({
  input$facetRow
})

facetRow <- reactive({
  aggLimDf <- aggLimDf()
  if (anyNull(aggLimDf, input$facetRow)) return('.')
  fr <- ifelse(input$facetRow=='None', '.', input$facetRow)
  if (fr != '.' && fr %in% colnames(aggLimDf)) fr else '.'
})

facetColOrig <- reactive({
  input$facetCol
})

facetCol <- reactive({
  aggLimDf <- aggLimDf()
  if (anyNull(aggLimDf, input$facetCol)) return('.')
  fc <- ifelse(input$facetCol=='None', '.', input$facetCol)
  if (fc != '.' && fc %in% colnames(aggLimDf)) fc else '.'
}) 

facetWrapOrig <- reactive({
  input$facetWrap
})

facetWrap <- reactive({
  aggLimDf <- aggLimDf()
  if (anyNull(aggLimDf, input$facetWrap)) return('.')
  fw <- ifelse(input$facetWrap=='None', '.', input$facetWrap)
  if (fw != '.' && fw %in% colnames(aggLimDf)) fw else '.'
})

facetScale <- reactive({
  if (is.null(input$facetScale)) 'none' else input$facetScale
})

facetGrids <- reactive({
  row <- facetRow()
  col <- facetCol()
  if (anyNull(row, col)) '. ~ .' else paste(row, '~', col)
})

xlim <- reactive({
  input$xlim
  if (!displayXlimCond()) NULL else input$xlim
})

ylim <- reactive({
  input$ylim
  if (!displayYlimCond()) NULL else input$ylim
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

showXYRange <- reactive({
  !is.null(input$showXYRange) && input$showXYRange
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

showAes <- reactive({
  !is.null(input$showAes) && input$showAes
})

showFacet <- reactive({
  !is.null(input$showFacet) && input$showFacet
})

showXYRange <- reactive({
  !is.null(input$showXYRange) && input$showXYRange
})

showTheme <- reactive({
  !is.null(input$showTheme) && input$showTheme
})

showDSTypeAndPlotAgg <- reactive({
  !is.null(input$showDSTypeAndPlotAgg) && input$showDSTypeAndPlotAgg
})

# showPlotAgg <- reactive({
#   !is.null(input$showPlotAgg) && input$showPlotAgg
# })
