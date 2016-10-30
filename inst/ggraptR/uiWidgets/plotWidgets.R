# if we abandon keep the default first value of selectInput list
# then we will lost any input state after collapsing aestetic elements block

output$plotTypeCtrl <- renderUI({
  if (!is.null(dataset())) {
    selectInput(inputId = "plotType", label = "Plot type", 
                choices = c('Scatter'='scatter', 'Pairs'='pairs', 'Violin'='violin',
                            'Line'='line', 'Path'='path',
                            'Histogram'='histogram', 'Density'='density', 
                            'Box'='box', 'Bar'='bar'))
                            #'Image'='image', 
                            #'2-Density', 'density2d'
  }
})

output$xCtrl <- renderUI({
  if (displayXCond()) {
    isolate(selectInput('x', 'X', choices=if (plotType() %in% c('violin', 'box', 'bar'))
      categoricalVars() else numericVars(), selected = x()))
  }
})

output$yCtrl <- renderUI({
  if (displayYCond()) {
    isolate(selectInput('y', 'Y', setdiff(numericVars(), if (displayXCond()) x()), y()))
  }
})

# columns for pairsPlot
output$columnsCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'columns', 'Columns', choices=names(dataset()), 
      selected=if (is.null(columns())) names(dataset())[1:min(ncol(dataset()), 3)] else
        columns(), multiple=T))
  }
})

## color control options
output$colCtrl <- renderUI({
  if (displayColCond()) {
    isolate({
      opts <- c('None', if (plotType() %in% c('scatter', 'pairs')) 
        names(dataset()) else categoricalVars())
      selectInput('color', 'Color', opts, color())
    })
  }
})

## treat-as-a-factor-variable option for color
output$treatAsFacVarColCtrl <- renderUI({
  if (displayTreatAsFacVarColCond()) {
    isolate(checkboxInput('treatAsFacVarCol', 'Treat as a factor variable', 
                          value=treatAsFacVarCol()))
  }
})

output$fillCtrl <- renderUI({
  if (displayFillCond()) {
    isolate(selectInput('fill', 'Fill', c('None', categoricalVars()), fillOrig()))
  }
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (displayPosCond()) {
    isolate(selectInput('position', 'Position', c('None', 'dodge', 'stack'), position()))
  }
})

## jitter options
output$jitCtrl <- renderUI({
  if (displayJitCond()) {
    isolate(checkboxInput('jitter', 'Apply jitter effect', value=TRUE)) #jitter()
  }
})

## geom smoothing options
output$smthCtrl <- renderUI({
  if (displaySmthCond()) {
    isolate(selectInput('smooth', 'Smoothing Effect',
                        c('None'='None', 'Linear'='lm', 'Non-linear'='auto'), 
                        smoothOrig()))
  } 
})

output$sizeCtrl <- renderUI({
  if (displaySizeCond()) {
    isolate(selectInput('size', 'Size', c('None', numericVars()), sizeOrig()))
  }
})

output$shapeCtrl <- renderUI({
  if (displayShapeCond()) {
    # options = c('None', varsUniqValsCntLOEN())
    isolate(selectInput('shape', 'Shape', c('None', categoricalVars()), shapeOrig()))
  }
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (displayBinWidthCond()) {
    isolate({
      # if (!is.null(dataset()) && x() %in% colnames(dataset)) {  # !is.null(x()) && 
      maxVal <- round(diff(range(dataset()[[x()]], na.rm=TRUE)))
      minVal <- stepVal <- if (maxVal > 10) 1 else 0.1
      sliderInput('binWidth', label = "Bin Width",
                  min=minVal, max=maxVal, value=binWidth(), step=stepVal)
    })
  }
})

## density line color options
output$densBlkLineCondCtrl <- renderUI({
  if (displayDensBlkLineCond()) {
    isolate(checkboxInput('densBlkLineCond', 'Draw black outline', 
                          value=densBlkLineCond()))
  }
})

## points overlay options
output$pointsOverlayCtrl <- renderUI({  
  if (displayPointsOverlayCond()) { 
    isolate(checkboxInput('pointsOverlay', 'Points Overlay', value=pointsOverlay()))
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
        c('None'='fixed', 'Free X'='free_x', 'Free Y'='free_y', 'Free X & Y'='free'),
        facetScale()))
  }
})

## alpha (opacity) options
output$alphaCtrl <- renderUI({
  if (showAesWgts()) {
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

output$coordFlipCtrl <- renderUI({
  if (displayCoordFlipCond()) {
    isolate(checkboxInput('coordFlip', 'Flip X and Y axis', value=F))  # val=coordFlip()
  }
})


output$ggpairsUpContCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsUpCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(ggpairsUpCont(), eval(formals(ggpairs)$upper)$continuous)))
  }
})

output$ggpairsUpComboCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsUpCombo', NULL,
      c('box', 'dot', 'facethist', 'facetdensity', 'denstrip','blank'),
      getFirstNonNull(ggpairsUpCombo(), eval(formals(ggpairs)$upper)$combo)))
  }
})

output$ggpairsUpDiscrCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsUpDiscr', NULL,
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(ggpairsUpDiscr(), eval(formals(ggpairs)$upper)$discrete)))
  }
})

output$ggpairsLowContCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsLowCont', NULL, 
      c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
      getFirstNonNull(ggpairsLowCont(), eval(formals(ggpairs)$lower)$continuous)))
  }
})

output$ggpairsLowComboCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsLowCombo', NULL, 
      c('box', 'dot', 'facethi'='facethist', 'facetdensity', 'denstrip', 'blank'),
      getFirstNonNull(ggpairsLowCombo(), eval(formals(ggpairs)$lower)$combo)))
  }
})

output$ggpairsLowDiscrCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsLowDiscr', NULL, 
      c('facetba'='facetbar', 'ratio', 'blank'),
      getFirstNonNull(ggpairsLowDiscr(), eval(formals(ggpairs)$lower)$discrete)))
  }
})

output$ggpairsDiagContCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsDiagCont', NULL,
      c('density'='densityDiag', 'bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(ggpairsDiagCont(), eval(formals(ggpairs)$diag)$continuous)))
  }
})

output$ggpairsDiagDiscrCtrl <- renderUI({
  if (displayGgpairsWgtsCond()) {
    isolate(selectInput(
      'ggpairsDiagDiscr', NULL,
      c('bar'='barDiag', 'blank'='blankDiag'),
      getFirstNonNull(ggpairsDiagDiscr(), eval(formals(ggpairs)$diag)$discrete)))
  }
})


# additional aggregation by options
output$plotAddAggByCtrl <- renderUI({
  if (displayPlotAddAggBy()) {
    isolate(selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                        choices=setdiff(origVars(), plotSemiAutoAggByBase()), multiple=T,
                        selected=plotAddAggBy()))
  }
})

output$xlimCtrl <- renderUI({
  if (displayXlim()) {
    isolate({
      if (x() %in% finalDFNumericVars() && !is.null(xRange())) {
        sliderInput("xlim", label="X Range",
                    min=xRange()[1], max=xRange()[2], value=xRange(), sep='')
      } else if (x() %in% finalDFFactorVars()) {
        selectInput('xlim', label='X Value', choices=xFactorVarUniqVals(), multiple=T)
      }
    })
  }
})

output$ylimCtrl <- renderUI({
  if (displayYlim()) {
    isolate({
      if (y() %in% finalDFNumericVars() && !is.null(yRange())) {
        sliderInput("ylim", label="Y Range",
                    min=yRange()[1], max=yRange()[2], value=yRange(), sep='')
      } else if (y() %in% finalDFFactorVars()) {
        selectInput('ylim', label='Y Value', choices=yFactorVarUniqVals(), multiple=T)
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
  if (displayThemeWgts()) {
    # only first 3 fonts are available on Windows machine. Next ones generate warnings
    labelFontFamilyOpts <- c('sans', 'serif', 'mono', 'Calibri', 
                             'Times', 'Helvetica', 'Courier')
    isolate(selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, 
                labelFontFamily()))
  }
})

output$labelFontFaceCtrl <- renderUI({
  if (displayThemeWgts()) {
    labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
    isolate(selectInput('labelFontFace', 'Label Font Face', 
                        labelFontFaceOpts, labelFontFace()))
  }
})

output$labelFontSizeCtrl <- renderUI({
  if (displayThemeWgts()) {
    isolate(numericInput('labelFontSize', 'Label Font Size', value=labelFontSize(), 
                         min=7, max=30, step=1))
  }
})

output$labelFontColorCtrl <- renderUI({
  if (displayThemeWgts()) {
    shinyjs::isolate(colourInput('labelFontColor', 'Label Font Color',
                                 value=labelFontColor()))
  }
})

output$hjustCtrl <- renderUI({
  if (displayThemeWgts()) {
    isolate(numericInput('hjust', 'Horizontal Adjust', value=hjust(),
                         min=0, max=1, step=0.1))
  }
})

output$vjustCtrl <- renderUI({
  if (displayThemeWgts()) {
    isolate(numericInput('vjust', 'Vertical Adjust', value=vjust(), 
                         min=0, max=1, step=0.1))
  }
})

output$plotThemeCtrl <- renderUI({
  if (displayThemeWgts()) {
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

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (displayAgg()) {
    isolate(selectInput("rawVsManAgg", "Dataset Type",
                        c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                        rawVsManAgg()))
  }
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (displayAgg()) {
    isolate(selectInput('plotAggMeth', 'Aggregation Method', 
                        c('None', 'sum', 'mean', 'count', 'min', 'max', 'median'),
                        plotAggMeth()))
  }
})


# checkboxes for widgets groups
output$showAesWgtsCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showAesWgts', 'Show aesthetics', value=TRUE)
  }
})

output$showFacetWgtsCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showFacetWgts', 'Show facets', value=FALSE)
  }
})

output$showXYRangeWgtsCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showXYRangeWgts', 'Show ranges', value=FALSE)
  }
})

output$showThemeWgtsCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showThemeWgts', 'Show themes', value=FALSE)
  }
})

output$showPlotAggWgtCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showPlotAggWgt', 'Show plot aggregations', value=FALSE)
  }
})


output$showDSTypeAndPlotAggWgtsCtrl <- renderUI({
  if (!is.null(plotType())) {
    checkboxInput('showDSTypeAndPlotAggWgts', 
                  'Show dataset type and aggregation method', value=FALSE)
  }
})


# plot generation
output$generatePlotCodeCtl <- renderUI({
  bsButton("generatePlotCode", "Generate Plot Code", type="action", icon = icon("code"))
})

output$generateCode <- renderText({
  generateCodeReactive()
})
