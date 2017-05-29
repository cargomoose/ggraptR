# if we abandon keep the default first value of selectInput list
# then we will lost any input state after collapsing aestetic elements block

output$itersToDrawCtrl <- renderUI({
  n <- reactVals$itersToDraw
  if (is.null(n)) return()
  if (n > 0) {
    numericInput('itersToDrawInp', NULL, n, width = '80px')
  } else {
    reactVals$readyToDraw <- T
    NULL
  }
})

output$plotTypesCtrl <- renderUI({
  if (is.null(dataset())) return()  # trigger
  curGroupAllOpts <- plotTypesOpts()
  
  isolate({
    pTypes <- plotTypes()
    isInit <- all(sapply(c(x(), y(), columns(), plotTypes()), is.null))
    
    if (isInit) {
      inp <- getInitialArg('initialPlot')
      initialPlot <- if (is.null(inp)) NULL else
        inp %>% tolower() %>% gsub('(plot)| ', '', .)
    }
    
    ls <- list()
    ls[c('opts', 'val')] <- if (isInit) {
      opts <- getPlotTypeOpts(initialPlot, 
                              length(numericVars()), length(categoricalVars()))
      list(opts, if (!is.null(initialPlot)) initialPlot else opts[1])
    } else if (reactVals$is_dataset_changed > 0) {
      if (is.null(pTypes)) list(list('Histogram' = 'histogram'), 'histogram')
    } else {
      reactVals$is_dataset_changed <- 0.5  # will be flipped in plotTypesOpts()
      list(curGroupAllOpts, pTypes)
    }
    
    selectInput("plotTypes", "Plot type", ls$opts, ls$val, multiple = T)
  })
})


#### main ####
output$xCtrl <- renderUI({
  if (displayXCond()) {
    isolate({
      varType <- if (needCatX(plotTypes())) 'categorical' else 'numeric'
      vars <- do.call.pasted(varType, 'Vars')
      if (!length(vars)) varType <- sprintf('%s. Absent' , varType)
      selectInput('x', sprintf('X (%s)', varType), vars, selected = x())
    })
  }
})

output$yCtrl <- renderUI({
  if (displayYCond()) {
    isolate({
      isDiamondsInit <- datasetName() == 'diamonds' && is.null(y())
      vars <- setdiff(numericVars(), if (displayXCond()) x())
      selectInput('y', paste0('Y (numeric', if (!length(vars)) '. Absent', ')'), vars,
                  if (isDiamondsInit) 'price' else y())
    })
  }
})

# columns for pairsPlot
output$columnsCtrl <- renderUI({
  if (displayGgpairsColumnsCond()) {
    isolate(selectInput(
      'columns', 'Columns', choices=names(dataset()),
      selected=if (is.null(columns())) names(dataset())[1:min(ncol(dataset()), 3)] else
        columns(), multiple=T))
  }
})

output$colorCtrl <- renderUI({
  if (displayColorCond()) {
    isolate({
      isScatter <- all('scatter' == plotTypes())
      isDiamondsInit <- isScatter && datasetName() == 'diamonds' && is.null(y())
      opts <- c('None', if (isScatter) 
                 names(dataset()) else categoricalVars())
      
      selectInput('color', 'Color', opts, if (isDiamondsInit) 'color' else color())
    })
  }
})

output$treatColorAsFactorCtrl <- renderUI({
  if (displayTreatAsFactorCond()) {
    isolate(checkboxInput('treatColorAsFactor', 'Treat color as a factor', value=F))
  }
})

output$fillCtrl <- renderUI({
  if (displayFillCond()) {
    isolate(selectInput('fill', 'Fill', c('None', categoricalVars()), fillOrig()))
  }
})

output$sizeCtrl <- renderUI({
  if (displaySizeCond()) {
    isolate(selectInput('size', 'Size', c('None', numericVars()), sizeOrig()))
  }
})

output$shapeCtrl <- renderUI({
  if (displayShapeCond()) {
    isolate(selectInput('shape', 'Shape', c('None', categoricalVars()), shapeOrig()))
  }
})

# position (stack vs. dodge) control options
output$positionCtrl <- renderUI({
  if (displayPositionCond()) {
    isolate(selectInput('position', 'Position', c('stack', 'dodge', 'fill'), position()))
  }
})

output$jitterCtrl <- renderUI({
  if (displayJitterCond()) {
    is_agg_empty <- is.null(plotAggMeth()) || tolower(plotAggMeth()) == 'none'
    isolate(checkboxInput('jitter', 'Apply jitter effect', 
                          all('scatter' == plotTypes()) && is_agg_empty))
  }
})

output$coordFlipCtrl <- renderUI({
  if (displayCoordFlipCond()) {
    isolate(checkboxInput('coordFlip', 'Flip X and Y axis', value=F))
  }
})

output$smoothCtrl <- renderUI({
  if (displaySmthCond()) {
    isolate(selectInput('smooth', 'Smoothing Effect',
                        c('None'='None', 'Linear'='lm', 'Non-linear'='auto'), 
                        smoothOrig()))
  } 
})

output$alphaCtrl <- renderUI({
  if (displayAlphaCond()) {
    sliderInput("alpha", label = "Opacity",
                min=0, max=1, value=0.5, step=0.1)
  }
})

# size magnifier
output$sizeMagCtrl <- renderUI({
  if (displaySizeMagCond()) {
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=3, step=1)
  }
})

output$nBinsCtrl <- renderUI({  # histogram bins options
  if (displayBinsCond()) {
    isolate(sliderInput('nBins', label = "Number of bins", min=5, max=100, 
                        value=if (is.null(nBins())) 16 else nBins()))
  }
})

# density plot color options
output$densBlackLineCtrl <- renderUI({
  if (displayDensBlackLineCond()) {
    isolate(checkboxInput('densBlackLine', 'Draw black outline', 
                          value=densBlackLine()))
  }
})


#### pairs ####
output$pairsUpContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsUpCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(pairsUpCont(), eval(formals(ggpairs)$upper)$continuous)))
  }
})

output$pairsUpComboCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsUpCombo', NULL,
      c('box', 'dot', 'facethist', 'facetdensity', 'denstrip','blank'),
      getFirstNonNull(pairsUpCombo(), eval(formals(ggpairs)$upper)$combo)))
  }
})

output$pairsUpDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsUpDiscr', NULL,
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(pairsUpDiscr(), eval(formals(ggpairs)$upper)$discrete)))
  }
})

output$pairsLowContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsLowCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(pairsLowCont(), eval(formals(ggpairs)$lower)$continuous)))
  }
})

output$pairsLowComboCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsLowCombo', NULL, 
      c('box', 'dot', 'facethi'='facethist', 'facetdensity', 'denstrip', 'blank'),
      getFirstNonNull(pairsLowCombo(), eval(formals(ggpairs)$lower)$combo)))
  }
})

output$pairsLowDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsLowDiscr', NULL, 
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(pairsLowDiscr(), eval(formals(ggpairs)$lower)$discrete)))
  }
})

output$pairsDiagContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsDiagCont', NULL,
      c('density'='densityDiag', 'bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(pairsDiagCont(), eval(formals(ggpairs)$diag)$continuous)))
  }
})

output$pairsDiagDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'pairsDiagDiscr', NULL,
      c('bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(pairsDiagDiscr(), eval(formals(ggpairs)$diag)$discrete)))
  }
})


#### facets ####
output$facetRowCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetRow', 'Facet Row', 
                        c('None', categoricalVars()), facetRowOrig()))
  }
})

output$facetColCtrl <- renderUI({
  facetRow <- facetRowOrig()
  if (displayFacetCond()) {
    isolate(selectInput('facetCol', 'Facet Column',
                        c('None', setdiff(categoricalVars(), facetRow)),
                        facetColOrig()))
  }
})

output$facetWrapCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetWrap', 'Facet Wrap', 
                        c('None', categoricalVars()), facetWrapOrig()))
  }
})

output$facetScaleCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetScale', 'Facet Scale',
                        c('None'='fixed', 'Free X'='free_x', 'Free Y'='free_y', 
                          'Free X & Y'='free'),
                        facetScale()))
  }
})


#### theme ####
output$plotTitleCtrl <- renderUI({
  isolate(textInput('plotTitle', 'Plot Title', value=plotTitle()))
})

output$xLabelCtrl <- renderUI({
  isolate(textInput('xLabel', 'X Label', value=xLabel()))
})

output$yLabelCtrl <- renderUI({
  isolate(textInput('yLabel', 'Y Label', value=yLabel()))
})

output$labelFontFamilyCtrl <- renderUI({
  # only the first 3 fonts are available on Windows machine. Next ones generate warnings
  labelFontFamilyOpts <- c('sans', 'serif', 'mono', 'Calibri', 
                           'Times', 'Helvetica', 'Courier')
  isolate(selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, 
                      labelFontFamily()))
})

output$labelFontFaceCtrl <- renderUI({
  labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
  isolate(selectInput('labelFontFace', 'Label Font Face', 
                      labelFontFaceOpts, labelFontFace()))
})

output$labelFontSizeCtrl <- renderUI({
  isolate(numericInput('labelFontSize', 'Label Font Size', value=labelFontSize(), 
                       min=7, max=30, step=1))
})

output$labelFontColorCtrl <- renderUI({
  isolate(colourInput('labelFontColor', 'Label Font Color',
                      value=labelFontColor()))
})

output$hjustCtrl <- renderUI({
  isolate(numericInput('hjust', 'Horizontal Adjust', hjust(), min=0, max=1, step=0.1))
})

output$vjustCtrl <- renderUI({
  isolate(numericInput('vjust', 'Vertical Adjust', vjust(), min=0, max=1, step=0.1))
})

output$plotThemeCtrl <- renderUI({
  themes <- c('Grey' = 'theme_grey', 
              'Black and White' = 'theme_bw', 
              'LibreOffice Calc' = 'theme_calc',
              'The Economist' = 'theme_economist',
              'Stephen Few' = 'theme_few',
              '538' = 'theme_fivethirtyeight', 
              'Google Docs' = 'theme_gdocs', 
              'HighCharts' = 'theme_hc', 
              'pander' = 'theme_pander', 
              'solarized' = 'theme_solarized', 
              'Stata' = 'theme_stata', 
              'Tufte' = 'theme_tufte', 
              'Wall Street Journal' = 'theme_wsj')
  isolate(selectInput('plotTheme', 'Plot Theme', themes, plotTheme()))
})


#### aggregations ####
# raw vs. manually aggregated. Plot tab
output$rawVsManAggCtrl <- renderUI({
  isolate(selectInput("rawVsManAgg", "Dataset Type",
                      c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                      rawVsManAgg()))
})

# aggregation method (sum, mean, ...). Plot tab
output$plotAggMethCtrl <- renderUI({
  plotTypes()
  isolate(selectInput('plotAggMeth', 'Aggregation Method', 
                      c('None', 'sum', 'mean', 'count', 'min', 'max', 'median'),
                      plotAggMeth()))
})

# additional aggregation by feature options. Plot tab
output$plotAddAggByCtrl <- renderUI({
  if (!is.null(plotTypes()) && semiAutoAggOn()) {
    isolate(selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                        choices=setdiff(colnames(rawDataset()), plotSemiAutoAggByBase()), 
                        multiple=T, selected=plotAddAggBy()))
  }
})
