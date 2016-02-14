## plot type options
output$plotTypeCtrl <- renderUI({
  selectInput(inputId = "plotType", label = "Plot Type", 
              choices = c('Scatter'='scatter', 'Line'='line',
                          'Bar'='bar', 'Histogram'='histogram', 
                          'Density'='density', 'Box'='box',
                          'Path'='path'
                          #'Violin'='violin', 
                          #'Image'='image', 
                          #'2-Density', 'density2d'
                          )
              )
})

## dataset type options (raw vs. manually aggregated)
output$rawVsManAggCtrl <- renderUI({
  if (is.null(displayRawVsManAgg())) return()
  if (displayRawVsManAgg()) {
    selectInput("rawVsManAgg", "Dataset Type",
                c("Raw Dataset" = 'raw', "Manually Aggregated" = 'manAgg'),
                isolate(rawVsManAgg()))
  }
})

## aggregation method options (for plot view only)
output$plotAggMethCtrl <- renderUI({
  if (is.null(displayPlotAggMeth())) return()
  if (displayPlotAggMeth()) {
    aggMethOpts <- c('None', 'sum', 'mean', 'count', 'min', 'max', 'median')
    selectInput('plotAggMeth', 'Aggregation Method', aggMethOpts, isolate(plotAggMeth()))
  }
})

## x-axis options
output$xCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  selectInput('x', 'X', choices=xOpts(), selected=isolate(x_sel()))
})

## y-axis options
output$yCtrl <- renderUI({
  if (is.null(input$dataset)) return()
  if (is.null(displayYCond())) return()
  if (displayYCond()) {
    selectInput('y', 'Y', choices=yOpts(), selected=isolate(y_sel()))
  }
})

## color control options
output$colCtrl <- renderUI({
  if (is.null(displayColCond())) return()
  if (displayColCond()) {
    selectInput('color', 'Color', colOpts(), selected="clarity") #isolate(color_sel())
  }
})

## treat-as-a-factor-variable option for color
output$treatAsFacVarColCtrl <- renderUI({
  if (is.null(displayTreatAsFacVarColCond())) return()
  if (displayTreatAsFacVarColCond()) {
    checkboxInput('treatAsFacVarCol', 'Treat as a factor variable.', value=isolate(treatAsFacVarCol()))
  }
})

## fill control options
output$fillCtrl <- renderUI({
  if (is.null(displayFillCond())) return()
  if (displayFillCond()) {
    selectInput('fill', 'Fill', fillOpts(), isolate(fill_sel()))
  }
})

## position (stack vs. dodge) control options
output$posCtrl <- renderUI({
  if (is.null(displayPosCond())) return()
  if (displayPosCond()) {
    selectInput('position', 'Position', c('None', 'dodge', 'stack'), isolate(position()))
  }
})

## jitter options
output$jitCtrl <- renderUI({
  flog.debug("plotWidgets::output$jitCtrl() - Begin", name='all')
  if (is.null(displayJitCond())){
    flog.debug("plotWidgets::output$jitCtrl() - is.null(displayJitCond()) - End", name='all')
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
  if (is.null(displaySmthCond())) return()
  if (displaySmthCond()) {
    if (all(c(input$x, input$y) %in% numericVars())) {
      selectInput('smooth', 'Smoothing Effect', 
                  c('None'='None', 'Linear'='lm', 'Non-linear'='auto'),
                  isolate(smoothOrig()))
    }
  } 
})

## size options
output$sizeCtrl <- renderUI({
  if (is.null(displaySizeCond())) return()
  if (displaySizeCond()) {
    selectInput('size', 'Size', sizeOpts(), isolate(size_sel()))
  }
})

## shape options
output$shapeCtrl <- renderUI({
  if (is.null(displayShapeCond())) return()
  if (displayShapeCond()) {
    selectInput('shape', 'Shape', shapeOpts(), isolate(shape_sel()))
  }
})

## histogram binwidth options
output$binWidthCtrl <- renderUI({
  if (is.null(displayBinWidthCond())) return()
  if (is.null(histMaxBinWidth())) return()
  if (displayBinWidthCond()) {
    sliderInput('binWidth', label = "Bin Width",
                min=1, max=histMaxBinWidth(), value=isolate(binWidth()), step=1) 
  }
})

## density line color options
output$densBlkLineCondCtrl <- renderUI({
  if (is.null(displayDensBlkLineCond())) return()
  if (displayDensBlkLineCond()) {
    checkboxInput('densBlkLineCond', 'Draw black outline', value=isolate(densBlkLineCond()))
  }
})

## points overlay options
output$ptsOverlayCondCtrl <- renderUI({  
  if (is.null(displayPtsOverlayCond())) return()
  if (displayPtsOverlayCond()) { 
    checkboxInput('ptsOverlayCond', 'Points Overlay', value=isolate(ptsOverlayCond()))
  }
})



## row-wise facet options
output$facetRowCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetRow', 'Facet Row', facetOpts(), isolate(facetRow_sel()))
  }
})

## column-wise facet options
output$facetColCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetCol', 'Facet Column', facetOpts(), isolate(facetCol_sel()))
  }
})

## facet wrap options
output$facetWrapCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetWrap', 'Facet Wrap', facetOpts(), isolate(facetWrap_sel()))
  }
})

## facet scale options
output$facetScaleCtrl <- renderUI({
  if (is.null(input$showFacetWgts)) return()
  if (input$showFacetWgts) {
    selectInput('facetScale', 'Facet Scale',
                c('None'='fixed', 'Free X'='free_x', 
                  'Free Y'='free_y', 'Free X & Y'='free'),
                isolate(facetScale()))
  }
})

## alpha (opacity) options
output$alphaCtrl <- renderUI({
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts) {
    sliderInput("alpha", label = "Opacity",
                min=0, max=1, value=0.5, step=0.1) #isolate(alpha())
  }
})


## size magnifier option
output$sizeMagCtrl <- renderUI({
  if (is.null(displaySizeMagCond())) return()
  if (displaySizeMagCond()) {
    sliderInput("sizeMag", label="Size Magnifier",
                min=1, max=25, value=3, step=1) #isolate(sizeMag())
  }
})

## coordinate flip options 
output$coordFlipCtrl <- renderUI({
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts) {
    checkboxInput('coordFlip', 'Flip X and Y coordinates.', value=isolate(coordFlip()))
  }
})


# additional aggregation by options
output$plotAddAggByCtrl <- renderUI({
  if (is.null(displayPlotAddAggBy())) return()
  if (displayPlotAddAggBy()) {
    selectInput('plotAddAggBy', 'Additional Aggregation Variables', 
                choices=plotAddAggByOpts(), multiple=T,
                selected=isolate(plotAddAggBy_sel()))
  }
})

## xlim control
output$xlimCtrl <- renderUI({
  if (is.null(displayXlim())) return()
  if (displayXlim()) {
    if (input$x %in% finalDFNumericVars()) {
      if (is.null(xRange())) return()
      sliderInput("xlim", label="X Range",
                  min=xRange()[1], max=xRange()[2], value=xRange(), round=FALSE)
    } else if (input$x %in% finalDFFactorVars()) {
      selectInput('xlim', label='X Value', 
                  choices=xFactorVarUniqVals(), 
                  #selected=xFactorVarUniqVals(),
                  multiple=T)
    }
  }
})

## ylim control
## note: ylim() is NOT applicable to histograms
output$ylimCtrl <- renderUI({
  if (is.null(displayYlim())) return()
  if (displayYlim()) {
    y <- y()
    
    if (y %in% finalDFNumericVars()) {
      if (is.null(yRange())) return()
      sliderInput("ylim", label="Y Range",
                  min=yRange()[1], max=yRange()[2], value=yRange(), round=FALSE)
    } else if (y %in% finalDFFactorVars()) {
      selectInput('ylim', label='Y Value',
                  choices=yFactorVarUniqVals(), 
                  #selected=yFactorVarUniqVals(),
                  multiple=T)
    }
  }
})



## plot title
output$plotTitleCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (is.null(input$reactive)) return()
  if (displayThemeWgts() & !input$reactive) {
    textInput('plotTitle', 'Plot Title', value=isolate(plotTitle()))
  }
})

## x label
output$xLabelCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (is.null(input$reactive)) return()
  if (displayThemeWgts() & !input$reactive) {
    textInput('xLabel', 'X Label', value=isolate(xLabel()))
  }
})

## y label
output$yLabelCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (is.null(input$reactive)) return()
  if (displayThemeWgts() & !input$reactive) {
    textInput('yLabel', 'Y Label', value=isolate(yLabel()))
  }
})

## label font family 
output$labelFontFamilyCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts()) {
    labelFontFamilyOpts <- c('Calibri', 'sans', 'serif', 'mono', 
                             'Times', 'Helvetica', 'Courier')
    selectInput('labelFontFamily', 'Label Font Family', labelFontFamilyOpts, isolate(labelFontFamily()))
  }
})

## label font face
output$labelFontFaceCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts()) {
    labelFontFaceOpts <- c('plain', 'bold', 'italic', 'bold.italic')
    selectInput('labelFontFace', 'Label Font Face', labelFontFaceOpts, isolate(labelFontFace()))
  }
})

## label font size
output$labelFontSizeCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('labelFontSize', 'Label Font Size', value=isolate(labelFontSize()), min=7, max=30, step=1)
})

## label font color
output$labelFontColorCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    shinyjs::colourInput('labelFontColor', 'Label Font Color', value=isolate(labelFontColor()))
})

## hjust
output$hjustCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('hjust', 'Horizontal Adjust', value=isolate(hjust()), min=0, max=1, step=0.1)
})

## vjust
output$vjustCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
  if (displayThemeWgts())
    numericInput('vjust', 'Vertical Adjust', value=isolate(vjust()), min=0, max=1, step=0.1)
})

## plot theme
output$plotThemeCtrl <- renderUI({
  if (is.null(displayThemeWgts())) return()
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
    selectInput('plotTheme', 'Plot Themes', themes, isolate(plotTheme()))
  }
})


#### show/hide checkbox widgets
## show aesthetic controls
output$showAesWgtsCtrl <- renderUI({
  checkboxInput('showAesWgts', 'Show aesthetics', value=TRUE)
})

## show facet controls
output$showFacetWgtsCtrl <- renderUI({
  checkboxInput('showFacetWgts', 'Show facets', value=FALSE)
})

## show X & Y range controls
output$showXYRangeWgtsCtrl <- renderUI({
  checkboxInput('showXYRangeWgts', 'Show ranges', value=FALSE)
})

## show aggregation controls
output$showPlotAggWgtCtrl <- renderUI({
  checkboxInput('showPlotAggWgt', 'Show plot aggregations', value=FALSE)
})

## show theme controls
output$showThemeWgtsCtrl <- renderUI({
  checkboxInput('showThemeWgts', 'Show themes', value=FALSE)
})

## show dataset type and plot aggregation method controls
output$showDSTypeAndPlotAggWgtsCtrl <- renderUI({
  checkboxInput('showDSTypeAndPlotAggWgts', 
                'Show dataset type and aggregation method', value=FALSE)
})
