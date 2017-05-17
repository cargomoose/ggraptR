#### main ####
datasetName <- reactive({
  input$datasetName
})

plotTypes <- reactive({
  input$plotTypes
})

plotTypesOpts <- reactive({
  reactVals$plotTypeOptsTrigger  # updates using an observer
  isolate({
    reactVals$is_dataset_changed <- F  # to distinguish plotTypes triggers
    getPlotTypeOpts(plotTypes())
  })
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

columns <- reactive({  # for pairsPlot
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

fillOrig <- reactive({
  input$fill
})

fill <- reactive({
  convertNoneToNULL(fillOrig())
})

sizeOrig <- reactive({
  input$size
})

size <- reactive({
  convertNoneToNULL(ensureProperVarName(
    isolate(colnames(aggLimDf())), sizeOrig(), 
    isolate(plotAggMeth()), isolate(semiAutoAggOn())))
})

shapeOrig <- reactive({
  input$shape
})

shape <- reactive({
  convertNoneToNULL(shapeOrig())
})

position <- reactive({
  convertNoneToNULL(input$position)
})

jitter <- reactive({
  if (!is.null(input$jitter) && input$jitter) 'jitter' else 'identity'
})

coordFlip <- reactive({
  !is.null(input$coordFlip) && input$coordFlip
})

smoothOrig <- reactive({
  input$smooth
})

smooth <- reactive({
  convertNoneToNULL(input$smooth)
})

alphaOrig <- reactive({
  input$alpha
})

alpha <- reactive({
  if (is.null(alphaOrig())) 1 else alphaOrig()
})

sizeMagOrig <- reactive({
  input$sizeMag
})

sizeMag <- reactive({
  if (is.null(input$sizeMag)) 4 else input$sizeMag
})

nBins <- reactive({
  input$nBins
})

densBlackLine <- reactive({
  input$densBlackLine
})


#### pairs ####
pairsUpCont <- reactive({
  input$pairsUpCont
})

pairsUpCombo <- reactive({
  input$pairsUpCombo
})

pairsUpDiscr <- reactive({
  input$pairsUpDiscr
})

pairsDiagCont <- reactive({
  input$pairsDiagCont
})

pairsDiagDiscr <- reactive({
  input$pairsDiagDiscr
})

pairsLowCont <- reactive({
  input$pairsLowCont
})

pairsLowCombo <- reactive({
  input$pairsLowCombo
})

pairsLowDiscr <- reactive({
  input$pairsLowDiscr
})


#### facets ####
facetRowOrig <- reactive({
  input$facetRow
})

facetRow <- reactive({
  aggLimDf <- aggLimDf()
  if (anyNull(aggLimDf, input$facetRow)) return('.')
  fr <- if (input$facetRow == 'None') '.' else input$facetRow
  if (fr != '.' && fr %in% colnames(aggLimDf)) fr else '.'
})

facetColOrig <- reactive({
  input$facetCol
})

facetCol <- reactive({
  aggLimDf <- aggLimDf()
  if (anyNull(aggLimDf, input$facetCol)) return('.')
  fc <- if (input$facetCol == 'None') '.' else input$facetCol
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


#### theme ####
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


#### aggregations ####
rawVsManAgg <- reactive({
  input$rawVsManAgg
})

plotAggMeth <- reactive({
  if (is.null(input$plotAggMeth)) 'none' else input$plotAggMeth
})

plotAddAggBy <- reactive({
  input$plotAddAggBy
})
