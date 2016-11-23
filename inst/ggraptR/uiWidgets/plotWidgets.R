# if we abandon keep the default first value of selectInput list
# then we will lost any input state after collapsing aestetic elements block

output$itersToDrawCtrl <- renderUI({
  n <- reactVals$itersToDraw
  if (is.null(n)) return()
  if (n > 0) {
    numericInput('itersToDrawInp', NULL, n, width = '80px')  # 1px
  } else {
    reactVals$readyToDraw <- T
    NULL
  }
})

output$plotTypesCtrl <- renderUI({
  if (displayPlotTypesCond()) {  # !is.null(dataset()) trigger
    normalOpts <- plotTypesOpts()  # the second trigger
    pTypes <- isolate(plotTypes())
    isUpdatedDataset <- isolate(!is.null(x()) && !x() %in% names(dataset()))
    isInit <- isolate(all(sapply(c(x(), y(), columns()), is.null)))
    initialPlot <- if (isInit) {
      sys.frames()[[1]]$initialPlot %>% tolower() %>% gsub('(plot)| ', '', .)
    }
    
    opts <- if (isInit) {
      getPlotTypeOpts(initialPlot)
    } else if (isUpdatedDataset) {
      if (is.null(pTypes)) list('Histogram' = 'histogram')  # may return NULL
    } else {
      normalOpts
    }
    
    val <- if (isInit) {
      initialPlot
    } else if (isUpdatedDataset) {
      if (!is.null(opts)) opts[[1]]
    } else {
      pTypes
    }
    
    selectInput("plotTypes", "Plot type", opts, val, multiple = T)
  }
})

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
      vars <- setdiff(numericVars(), if (displayXCond()) x())
      selectInput('y', paste0('Y (numeric', if (!length(vars)) '. Absent', ')'), vars,y())
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
      opts <- c('None', if (all('scatter' == plotTypes())) 
        names(dataset()) else categoricalVars())
      selectInput('color', 'Color', opts, color())
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
    isolate(checkboxInput('jitter', 'Apply jitter effect', 'scatter' %in% plotTypes()))
  }
})

output$coordFlipCtrl <- renderUI({
  if (displayCoordFlipCond()) {
    isolate(checkboxInput('coordFlip', 'Flip X and Y axis', value=F))  # val=coordFlip()
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
                min=0, max=1, value=0.5, step=0.1) #isolate(alpha())
  }
})

# size magnifier
output$sizeMagCtrl <- renderUI({
  if (displaySizeMagCond()) {
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=3, step=1) #isolate(sizeMag())
  }
})

# histogram bins options
output$nBinsCtrl <- renderUI({
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



# row-wise facet options
output$facetRowCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetRow', 'Facet Row', 
                        c('None', categoricalVars()), facetRowOrig()))
  }
})

# column-wise facet options
output$facetColCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetCol', 'Facet Column', 
                        c('None', categoricalVars()), facetColOrig()))
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

output$xlimCtrl <- renderUI({
  if (displayXlimCond()) {
    isolate({
      if (x() %in% aggDfFactorVars()) {
        selectInput('xlim', label='X Value', choices=unique(as.character(aggDf()[[x()]])), 
                    multiple=T)
      } else if (x() %in% aggDfNumericVars() && !is.null(xRange())) {
        sliderInput("xlim", label="X Range",
                    min=xRange()[1], max=xRange()[2], value=xRange(), sep='')
      }
    })
  }
})

output$ylimCtrl <- renderUI({
  if (displayYlimCond()) {
    isolate({
      if (y() %in% aggDfFactorVars()) {
        selectInput('ylim', label='Y Value', choices=levels(aggDf()[[y()]]), multiple=T)
      } else if (y() %in% aggDfNumericVars() && !is.null(yRange())) {
        sliderInput("ylim", label="Y Range",
                    min=yRange()[1], max=yRange()[2], value=yRange(), sep='')
      }
    })
  }
})

output$plotTitleCtrl <- renderUI({
  if (displayTitlesCond()) {
    isolate(textInput('plotTitle', 'Plot Title', value=plotTitle()))
  }
})

output$xLabelCtrl <- renderUI({
  if (displayTitlesCond()) {
    isolate(textInput('xLabel', 'X Label', value=xLabel()))
  }
})

output$yLabelCtrl <- renderUI({
  if (displayTitlesCond()) {
    isolate(textInput('yLabel', 'Y Label', value=yLabel()))
  }
})

output$labelFontFamilyCtrl <- renderUI({
  if (displayThemeCond()) {
    # only first 3 fonts are available on Windows machine. Next ones generate warnings
    labelFontFamilyOpts <- c('sans', 'serif', 'mono', 'Calibri', 
                             'Times', 'Helvetica', 'Courier')
    isolate(selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, 
                labelFontFamily()))
  }
})

output$labelFontFaceCtrl <- renderUI({
  if (displayThemeCond()) {
    labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
    isolate(selectInput('labelFontFace', 'Label Font Face', 
                        labelFontFaceOpts, labelFontFace()))
  }
})

output$labelFontSizeCtrl <- renderUI({
  if (displayThemeCond()) {
    isolate(numericInput('labelFontSize', 'Label Font Size', value=labelFontSize(), 
                         min=7, max=30, step=1))
  }
})

output$labelFontColorCtrl <- renderUI({
  if (displayThemeCond()) {
    isolate(colourInput('labelFontColor', 'Label Font Color',
                                 value=labelFontColor()))
  }
})

output$hjustCtrl <- renderUI({
  if (displayThemeCond()) {
    isolate(numericInput('hjust', 'Horizontal Adjust', hjust(), min=0, max=1, step=0.1))
  }
})

output$vjustCtrl <- renderUI({
  if (displayThemeCond()) {
    isolate(numericInput('vjust', 'Vertical Adjust', vjust(), min=0, max=1, step=0.1))
  }
})

output$plotThemeCtrl <- renderUI({
  if (displayThemeCond()) {
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
    isolate(selectInput('plotTheme', 'Plot Themes', themes, plotTheme()))
  }
})

# additional aggregation by options
output$plotAddAggByCtrl <- renderUI({
  if (displayPlotAddAggByCond()) {
    isolate(selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                        choices=setdiff(colnames(rawDataset()), plotSemiAutoAggByBase()), 
                        multiple=T, selected=plotAddAggBy()))
  }
})

# dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (displayAggCond()) {
    isolate(selectInput("rawVsManAgg", "Dataset Type",
                        c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                        rawVsManAgg()))
  }
})

# aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (displayAggCond()) {
    isolate(selectInput('plotAggMeth', 'Aggregation Method', 
                        c('None', 'sum', 'mean', 'count', 'min', 'max', 'median'),
                        plotAggMeth()))
  }
})



# checkboxes for widgets groups
output$showAesCtrl <- renderUI({
  plotTypes()
  isolate(checkboxInput('showAes', 'Show aesthetics', 
                        value=!is.null(plotTypes())))# is.null(input$showAes) || showAes()
})

output$showFacetCtrl <- renderUI({
  plotTypes()
  isolate(checkboxInput('showFacet', 'Show facets', value=showFacet()))
})

output$showXYRangeCtrl <- renderUI({
  plotTypes()
  checkboxInput('showXYRange', 'Show ranges', value=showXYRange())
})

output$showThemeCtrl <- renderUI({
  plotTypes()
  checkboxInput('showTheme', 'Show themes', value=showTheme())
})

output$showDSTypeAndPlotAggCtrl <- renderUI({
  plotTypes()
  checkboxInput('showDSTypeAndPlotAgg', 
                'Show dataset type and aggregation method', showDSTypeAndPlotAgg())
})

# output$showPlotAggCtrl <- renderUI({
#   if (!is.null(plotTypes())) {
#     checkboxInput('showPlotAgg', 'Show plot aggregations', value=showPlotAgg())
#   }
# })



output$generatePlotCodeCtl <- renderUI({
  bsButton("generatePlotCode", "Generate Plot Code", type="action", icon = icon("code"))
})

output$generateCode <- renderText({
  reactVals$log[1]
})
