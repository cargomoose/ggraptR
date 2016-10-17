output$plotTypeCtrl <- renderUI({
  selectInput(inputId = "plotType", label = "Plot type", 
              choices = c('Scatter'='scatter', 'Pairs'='pairs', 'Violin'='violin',
                          'Line'='line', 'Path'='path',
                          'Histogram'='histogram', 'Density'='density', 
                          'Box'='box', 'Bar'='bar'))
                          #'Image'='image', 
                          #'2-Density', 'density2d'
})

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (!is.null(displayRawVsManAgg()) && displayRawVsManAgg()) {
    selectInput("rawVsManAgg", "Dataset Type",
                c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                isolate(rawVsManAgg()))
  }
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (!is.null(displayPlotAggMeth()) && displayPlotAggMeth()) {
    selectInput('plotAggMeth', 'Aggregation Method', 
                c('None', 'sum', 'mean', 'count', 'min', 'max', 'median'),
                isolate(plotAggMeth()))
  }
})

output$xCtrl <- renderUI({
  if (!is.null(input$dataset) && displayXCond()) {
    selectInput('x', 'X', choices=xOpts())
  }
})

output$yCtrl <- renderUI({
  if (!is.null(input$dataset) && displayYCond()) {
    selectInput('y', 'Y', choices=yOpts())
  }
})

# columns for pairsPlot
output$columnsCtrl <- renderUI({
  if (!is.null(input$dataset) && displayColumnsCond()) {
    selectInput('columns', 'Columns', choices=colnamesOpts(), 
                selected=colnamesOpts()[1:min(ncol(dataset()), 3)], multiple=T)
  }
})

## color control options
output$colCtrl <- renderUI({
  if (!is.null(displayColCond()) && displayColCond()) {
    selectInput('color', 'Color', colOpts(), selected="clarity") #isolate(color_sel())
  }
})

## treat-as-a-factor-variable option for color
output$treatAsFacVarColCtrl <- renderUI({
  if (!is.null(displayTreatAsFacVarColCond()) && displayTreatAsFacVarColCond()) {
    checkboxInput('treatAsFacVarCol', 'Treat as a factor variable', 
                  value=isolate(treatAsFacVarCol()))
  }
})

output$fillCtrl <- renderUI({
  if (!is.null(displayFillCond()) && displayFillCond()) {
    selectInput('fill', 'Fill', fillOpts(), isolate(fill_sel()))
  }
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (!is.null(displayPosCond()) && displayPosCond()) {
    selectInput('position', 'Position', c('None', 'dodge', 'stack'), isolate(position()))
  }
})

## jitter options
output$jitCtrl <- renderUI({
  flog.debug("plotWidgets::output$jitCtrl() - Begin", name='all')
  if (is.null(displayJitCond())){
    flog.debug("plotWidgets::output$jitCtrl() - is.null(displayJitCond()) - End", 
               name='all')
    return()
  }
  if (displayJitCond()) {
    flog.debug("plotWidgets::output$jitCtrl() - displayJitCond() - End", name='all')
    flog.debug(jitter(), name='all')
    checkboxInput('jitter', 'Apply jitter effect', value=TRUE) #isolate(jitter())
  }
})

## geom smoothing options
output$smthCtrl <- renderUI({
  if (!is.null(displaySmthCond()) && displaySmthCond() 
      && all(c(input$x, input$y) %in% numericVars())) {
      selectInput('smooth', 'Smoothing Effect', 
                  c('None'='None', 'Linear'='lm', 'Non-linear'='auto'),
                  isolate(smoothOrig()))
  } 
})

output$sizeCtrl <- renderUI({
  if (!is.null(displaySizeCond()) && displaySizeCond()) {
    selectInput('size', 'Size', sizeOpts(), isolate(size_sel()))
  }
})

output$shapeCtrl <- renderUI({
  if (!is.null(displayShapeCond()) && displayShapeCond()) {
    selectInput('shape', 'Shape', shapeOpts(), isolate(shape_sel()))
  }
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (!is.null(displayBinWidthCond()) && !is.null(histMaxBinWidth()) 
      && displayBinWidthCond()) {
    maxVal <- histMaxBinWidth()
    minVal <- stepVal <- if (maxVal > 10) 1 else 0.1
    sliderInput('binWidth', label = "Bin Width",
                min=minVal, max=maxVal, value=isolate(binWidth()), step=stepVal) 
  }
})

## density line color options
output$densBlkLineCondCtrl <- renderUI({
  if (!is.null(displayDensBlkLineCond()) && displayDensBlkLineCond()) {
    checkboxInput('densBlkLineCond', 'Draw black outline', 
                  value=isolate(densBlkLineCond()))
  }
})

## points overlay options
output$ptsOverlayCondCtrl <- renderUI({  
  if (!is.null(displayPtsOverlayCond()) && displayPtsOverlayCond()) { 
    checkboxInput('ptsOverlayCond', 'Points Overlay', value=isolate(ptsOverlayCond()))
  }
})



## row-wise facet options
output$facetRowCtrl <- renderUI({
  if (!is.null(input$showFacetWgts) && input$showFacetWgts) {
    selectInput('facetRow', 'Facet Row', facetOpts(), isolate(facetRow_sel()))
  }
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  if (!is.null(input$showFacetWgts) && input$showFacetWgts) {
    selectInput('facetCol', 'Facet Column', facetOpts(), isolate(facetCol_sel()))
  }
})

output$facetWrapCtrl <- renderUI({
  if (!is.null(input$showFacetWgts) && input$showFacetWgts) {
    selectInput('facetWrap', 'Facet Wrap', facetOpts(), isolate(facetWrap_sel()))
  }
})

output$facetScaleCtrl <- renderUI({
  if (!is.null(input$showFacetWgts) && input$showFacetWgts) {
    selectInput('facetScale', 'Facet Scale',
                c('None'='fixed', 'Free X'='free_x', 
                  'Free Y'='free_y', 'Free X & Y'='free'),
                isolate(facetScale()))
  }
})

## alpha (opacity) options
output$alphaCtrl <- renderUI({
  if (!is.null(input$showAesWgts) && input$showAesWgts) {
    sliderInput("alpha", label = "Opacity",
                min=0, max=1, value=0.5, step=0.1) #isolate(alpha())
  }
})

## size magnifier
output$sizeMagCtrl <- renderUI({
  if (!is.null(displaySizeMagCond()) && displaySizeMagCond()) {
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=3, step=1) #isolate(sizeMag())
  }
})

output$coordFlipCtrl <- renderUI({
  if (displayCoordFlipCond()) {
    checkboxInput('coordFlip', 'Flip X and Y axis', value=isolate(coordFlip()))
  }
})


output$ggpairsUpContCtrl <- renderUI({
  selectInput(
    'ggpairsUpCont', NULL,
    choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
    selected=eval(formals(ggpairs)$upper)$continuous)
})

output$ggpairsUpComboCtrl <- renderUI({
  selectInput(
    'ggpairsUpCombo', NULL,
    choices=c('box', 'dot', 'facethist', 'facetdensity', 'denstrip','blank'),
    selected=eval(formals(ggpairs)$upper)$combo)
})

output$ggpairsUpDiscrCtrl <- renderUI({
  selectInput(
    'ggpairsUpDiscr', NULL,
    choices=c('facetba'='facetbar', 'ratio', 'blank'),
    selected=eval(formals(ggpairs)$upper)$discrete)
})

output$ggpairsLowContCtrl <- renderUI({
  selectInput(
    'ggpairsLowCont', NULL, 
    choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),
    selected=eval(formals(ggpairs)$lower)$continuous)
})

output$ggpairsLowComboCtrl <- renderUI({
  selectInput(
    'ggpairsLowCombo', NULL, 
    choices=c('box', 'dot', 'facethi'='facethist', 'facetdensity', 'denstrip', 'blank'),
    selected=eval(formals(ggpairs)$lower)$combo)
})

output$ggpairsLowDiscrCtrl <- renderUI({
  selectInput(
    'ggpairsLowDiscr', NULL, 
    choices=c('facetba'='facetbar', 'ratio', 'blank'),
    selected=eval(formals(ggpairs)$lower)$discrete)
})

output$ggpairsDiagContCtrl <- renderUI({
  selectInput(
    'ggpairsDiagCont', NULL,
    choices=c('density'='densityDiag', 'bar'='barDiag', 'blank'='blankDiag'),
    selected=eval(formals(ggpairs)$diag)$continuous)
})

output$ggpairsDiagDiscrCtrl <- renderUI({
  selectInput(
    'ggpairsDiagDiscr', NULL,
    choices=c('bar'='barDiag', 'blank'='blankDiag'),
    selected=eval(formals(ggpairs)$diag)$discrete)
})


# additional aggregation by options
output$plotAddAggByCtrl <- renderUI({
  if (!is.null(displayPlotAddAggBy()) && displayPlotAddAggBy()) {
    selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                choices=plotAddAggByOpts(), multiple=T,
                selected=isolate(plotAddAggBy_sel()))
  }
})

output$xlimCtrl <- renderUI({
  if (!is.null(displayXlim()) && displayXlim()) {
    if (input$x %in% finalDFNumericVars()) {
      if (is.null(xRange())) return()
      sliderInput("xlim", label="X Range",
                  min=xRange()[1], max=xRange()[2], value=xRange(), sep='')
    } else if (input$x %in% finalDFFactorVars()) {
      selectInput('xlim', label='X Value', 
                  choices=xFactorVarUniqVals(), multiple=T)
    }
  }
})

# note: ylim() is NOT applicable to histograms
output$ylimCtrl <- renderUI({
  if (!is.null(displayYlim()) && displayYlim()) {
    y <- y()
    if (y %in% finalDFNumericVars()) {
      if (is.null(yRange())) return()
      sliderInput("ylim", label="Y Range",
                  min=yRange()[1], max=yRange()[2], value=yRange(), sep='')
    } else if (y %in% finalDFFactorVars()) {
      selectInput('ylim', label='Y Value',
                  choices=yFactorVarUniqVals(), multiple=T)
    }
  }
})

output$plotTitleCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) &&  !is.null(input$reactive) 
      && displayThemeWgts() && !input$reactive) {
    textInput('plotTitle', 'Plot Title', value=isolate(plotTitle()))
  }
})

output$xLabelCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && !is.null(input$reactive)
      && displayThemeWgts() && !input$reactive) {
    textInput('xLabel', 'X Label', value=isolate(xLabel()))
  }
})

output$yLabelCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && !is.null(input$reactive)
      && displayThemeWgts() && !input$reactive) {
    textInput('yLabel', 'Y Label', value=isolate(yLabel()))
  }
})

output$labelFontFamilyCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts()) {
    # first 3 fonts are available on Windows machine. Next ones generate warnings
    labelFontFamilyOpts <- c('sans', 'serif', 'mono', 'Calibri', 
                             'Times', 'Helvetica', 'Courier')
    selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, 
                isolate(labelFontFamily()))
  }
})

output$labelFontFaceCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts()) {
    labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
    selectInput('labelFontFace', 'Label Font Face', 
                labelFontFaceOpts, isolate(labelFontFace()))
  }
})

output$labelFontSizeCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts())
    numericInput('labelFontSize', 'Label Font Size', value=isolate(labelFontSize()), 
                 min=7, max=30, step=1)
})

output$labelFontColorCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts())
    shinyjs::colourInput('labelFontColor', 'Label Font Color', 
                         value=isolate(labelFontColor()))
})

output$hjustCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts())
    numericInput('hjust', 'Horizontal Adjust', value=isolate(hjust()), 
                 min=0, max=1, step=0.1)
})

output$vjustCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts())
    numericInput('vjust', 'Vertical Adjust', value=isolate(vjust()), 
                 min=0, max=1, step=0.1)
})

output$plotThemeCtrl <- renderUI({
  if (!is.null(displayThemeWgts()) && displayThemeWgts()) {
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
    selectInput('plotTheme', 'Plot Themes', themes, isolate(plotTheme()))
  }
})


# next block is about show/hide checkbox widgets
output$showAesWgtsCtrl <- renderUI({
  checkboxInput('showAesWgts', 'Show aesthetics', value=TRUE)
})

output$showFacetWgtsCtrl <- renderUI({
  checkboxInput('showFacetWgts', 'Show facets', value=FALSE)
})

output$showXYRangeWgtsCtrl <- renderUI({
  checkboxInput('showXYRangeWgts', 'Show ranges', value=FALSE)
})

output$showPlotAggWgtCtrl <- renderUI({
  checkboxInput('showPlotAggWgt', 'Show plot aggregations', value=FALSE)
})

output$showThemeWgtsCtrl <- renderUI({
  checkboxInput('showThemeWgts', 'Show themes', value=FALSE)
})

output$showDSTypeAndPlotAggWgtsCtrl <- renderUI({
  checkboxInput('showDSTypeAndPlotAggWgts', 
                'Show dataset type and aggregation method', value=FALSE)
})

output$generatePlotCodeCtl <- renderUI({
  bsButton("generatePlotCode", label="Generate Plot Code", type="action", 
                    icon = icon("code"))
})

output$generateCode <- renderText({
  generateCodeReactive()
})
