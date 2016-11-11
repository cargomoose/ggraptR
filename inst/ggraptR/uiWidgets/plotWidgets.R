# if we abandon keep the default first value of selectInput list
# then we will lost any input state after collapsing aestetic elements block

output$controlsLoadingCtrl <- renderUI({
  n <- controlsLoading$itersToDrawPlot
  if (is.null(n)) return()
  if (n > 0) {
    numericInput('controlsLoadingInp', NULL, n, width = '80px')  # 1px
  } else {
    controlsLoading$ready <- T
    NULL
  }
})


output$plotTypeCtrl <- renderUI({
  if (!is.null(dataset())) {
    pType <- isolate(input$plotType)
    selectInput("plotType", "Plot type", 
                choices = c('Scatter'='scatter', 'Pairs'='pairs', 'Violin'='violin',
                            'Density 2D'='density2d', 'Line'='line', 'Path'='path',
                            'Histogram'='histogram', 'Density'='density', 
                            'Box'='box', 'Bar'='bar'),   #'Image'='image'
                # need value to change plot type everytime dataset() changes. For trigger
                if (!is.null(pType) && pType == 'scatter') 'histogram')
  }
})

output$xCtrl <- renderUI({
  if (displayXCond()) {
    isolate({
      varType <- if (plotType() %in% c('violin', 'box', 'bar')) 
        'categorical' else 'numeric'
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
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'columns', 'Columns', choices=names(dataset()), 
      selected=if (is.null(columns())) names(dataset())[1:min(ncol(dataset()), 3)] else
        columns(), multiple=T))
  }
})

output$colorCtrl <- renderUI({
  if (displayColorCond()) {
    isolate({
      opts <- c('None', if (plotType() == 'scatter') 
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


output$jitterCtrl <- renderUI({
  if (displayJitterCond()) {
    isolate(checkboxInput('jitter', 'Apply jitter effect', value=plotType() == 'scatter'))
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

## size magnifier
output$sizeMagCtrl <- renderUI({
  if (displaySizeMagCond()) {
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=3, step=1) #isolate(sizeMag())
  }
})

## histogram bins options
output$nBinsCtrl <- renderUI({
  if (displayBinWidthCond()) {
    isolate(sliderInput('nBins', label = "Bin Width", min=5, max=100, 
                        value=if (is.null(nBins())) 16 else nBins()))
  }
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (displayPosCond()) {
    isolate(selectInput('position', 'Position', c('stack', 'dodge', 'fill'), position()))
  }
})

## density plot color options
output$densBlackLineCtrl <- renderUI({
  if (displayDensBlackLineCond()) {
    isolate(checkboxInput('densBlackLine', 'Draw black outline', 
                          value=densBlackLine()))
  }
})

output$pointsOverlayCtrl <- renderUI({  
  if (displayPointsOverlayCond()) { 
    isolate(checkboxInput('pointsOverlay', 'Points Overlay', value=pointsOverlay()))
  }
})

output$ggpairsUpContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsUpCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(ggpairsUpCont(), eval(formals(ggpairs)$upper)$continuous)))
  }
})

output$ggpairsUpComboCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsUpCombo', NULL,
      c('box', 'dot', 'facethist', 'facetdensity', 'denstrip','blank'),
      getFirstNonNull(ggpairsUpCombo(), eval(formals(ggpairs)$upper)$combo)))
  }
})

output$ggpairsUpDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsUpDiscr', NULL,
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(ggpairsUpDiscr(), eval(formals(ggpairs)$upper)$discrete)))
  }
})

output$ggpairsLowContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsLowCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(ggpairsLowCont(), eval(formals(ggpairs)$lower)$continuous)))
  }
})

output$ggpairsLowComboCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsLowCombo', NULL, 
      c('box', 'dot', 'facethi'='facethist', 'facetdensity', 'denstrip', 'blank'),
      getFirstNonNull(ggpairsLowCombo(), eval(formals(ggpairs)$lower)$combo)))
  }
})

output$ggpairsLowDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsLowDiscr', NULL, 
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(ggpairsLowDiscr(), eval(formals(ggpairs)$lower)$discrete)))
  }
})

output$ggpairsDiagContCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsDiagCont', NULL,
      c('density'='densityDiag', 'bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(ggpairsDiagCont(), eval(formals(ggpairs)$diag)$continuous)))
  }
})

output$ggpairsDiagDiscrCtrl <- renderUI({
  if (displayGgpairsCond()) {
    isolate(selectInput(
      'ggpairsDiagDiscr', NULL,
      c('bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(ggpairsDiagDiscr(), eval(formals(ggpairs)$diag)$discrete)))
  }
})



## row-wise facet options
output$facetRowCtrl <- renderUI({
  if (displayFacetCond()) {
    isolate(selectInput('facetRow', 'Facet Row', 
                        c('None', categoricalVars()), facetRowOrig()))
  }
})

## column-wise facet options
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
        selectInput('xlim', label='X Value', choices=xFactorVarUniqVals(), multiple=T)
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
        selectInput('ylim', label='Y Value', choices=yFactorVarUniqVals(), multiple=T)
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
                        choices=setdiff(origVars(), plotSemiAutoAggByBase()), multiple=T,
                        selected=plotAddAggBy()))
  }
})

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (displayAggCond()) {
    isolate(selectInput("rawVsManAgg", "Dataset Type",
                        c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                        rawVsManAgg()))
  }
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (displayAggCond()) {
    isolate(selectInput('plotAggMeth', 'Aggregation Method', 
                        c('None', 'sum', 'mean', 'count', 'min', 'max', 'median'),
                        plotAggMeth()))
  }
})



# checkboxes for widgets groups
output$showAesCtrl <- renderUI({
  if (!is.null(plotType())) {
    isolate(checkboxInput('showAes', 'Show aesthetics', 
                          value=is.null(input$showAes) || showAes()))
  }
})

output$showFacetCtrl <- renderUI({
  if (!is.null(plotType())) {
    isolate(checkboxInput('showFacet', 'Show facets', value=showFacet()))
  }
})

output$showXYRangeCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showXYRange', 'Show ranges', value=showXYRange())
  }
})

output$showThemeCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showTheme', 'Show themes', value=showTheme())
  }
})

output$showDSTypeAndPlotAggCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showDSTypeAndPlotAgg', 
                  'Show dataset type and aggregation method', showDSTypeAndPlotAgg())
  }
})

# output$showPlotAggCtrl <- renderUI({
#   if (!is.null(plotType())) {
#     checkboxInput('showPlotAgg', 'Show plot aggregations', value=showPlotAgg())
#   }
# })



output$generatePlotCodeCtl <- renderUI({
  bsButton("generatePlotCode", "Generate Plot Code", type="action", icon = icon("code"))
})

output$generateCode <- renderText({
  log$plot[1]
})
