#### display conditional reactives

## display y condition reactive
displayYCond <- reactive({
  if (is.null(input$plotType)) return()  
  display <- TRUE
  return (!(input$plotType %in% c('histogram', 'density')))
})

## display color condition reactive
displayColCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts) 
    return (any(input$plotType %in% c('line', 'scatter', 'path')))
})

## display treat-as-a-factor-variable (for color) condition reactive
displayTreatAsFacVarColCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts)
    return (any(input$plotType %in% c('scatter'))) 
})

## display fill condition reactive
displayFillCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts)
    return (any(input$plotType %in% c('box', 'histogram', 'bar', 'density')))
})


## display position condition reactive
displayPosCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  if (input$showAesWgts)
    return (any(input$plotType %in% c('histogram', 'bar')))
})

## display shape condition reactive
displayShapeCond <- reactive({
  if (is.null(input$plotType)) return()  
  if (is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display size condition reactive
displaySizeCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display smooth condition reactive
displaySmthCond <- reactive({
  if (is.null(input$plotType)) return()  
  if (is.null(xType())) return()
  if (is.null(yType())) return()
  if (is.null(input$showAesWgts)) return()
  
  display <- FALSE
  if (input$plotType=='scatter') {
    if (xType()=='continuous' & yType()=='continuous')
        display <- input$showAesWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) {
      if (xType()=='continuous' & yType()=='continuous')
      display <- input$showAesWgts
    }
  }
  display  
})

## display jitter condition reactive 
displayJitCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (input$plotType=='line') {
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display size magnifier condition reactive (belongs to advanced control widgets)
displaySizeMagCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  display <- FALSE
  if (input$plotType=='scatter') {
    display <- input$showAesWgts
  } else if (any(input$plotType %in% c('line', 'path'))) {    
    if (is.null(input$ptsOverlayCond)) return()
    if (input$ptsOverlayCond) display <- input$showAesWgts
  }
  display
})

## display bin width condition reactive
displayBinWidthCond <- reactive({
  if (is.null(input$plotType)) return() 
  if (is.null(input$x)) return()
  if (is.null(input$showAesWgts)) return()
  if (!(input$x %in% finalDFNumericVars())) return()
  if (input$showAesWgts)
    return (input$plotType=='histogram')
})

## display density black line condition reactive
displayDensBlkLineCond <- reactive({
  if (is.null(input$plotType)) return()
  if (is.null(input$showAesWgts)) return()
  return (input$plotType=='density' & input$showAesWgts)
})


## display points overlay checkbox condition reactive
displayPtsOverlayCond <- reactive({
  if (is.null(input$plotType)) return()
  return (input$plotType %in% c('line', 'path'))
})

## display additional aggregation select field condition reactive
displayPlotAddAggBy <- reactive({
  if (is.null(input$showDSTypeAndPlotAggWgts)) return()
  if (is.null(semiAutoAggOn())) return()
  return (input$showDSTypeAndPlotAggWgts & semiAutoAggOn())
})

## display xlim condition reactive
displayXlim <- reactive({
  if (is.null(dataset())) return()
  if (is.null(input$x)) return()
  if (is.null(y())) return()

  if (is.null(input$showXYRangeWgts)) return()
  return(input$showXYRangeWgts)
})

## display ylim condition reactive
displayYlim <- reactive({
  if (is.null(dataset())) return()
  if (is.null(input$x)) return()
  if (is.null(y())) return()

  if (is.null(input$plotType)) return()  
  if (input$plotType=='histogram') return()
  
  if (is.null(input$showXYRangeWgts)) return()
  return(input$showXYRangeWgts)
})

## display raw-vs-manual-agg condition reactive
displayRawVsManAgg <- reactive({
  if (is.null(input$showDSTypeAndPlotAggWgts)) return()
  input$showDSTypeAndPlotAggWgts
})


## display plot aggregation method reactive
displayPlotAggMeth <- reactive({
  if (is.null(input$showDSTypeAndPlotAggWgts)) return()
  input$showDSTypeAndPlotAggWgts
})


displayThemeWgts <- reactive({
  if (is.null(input$showThemeWgts)) return()
  input$showThemeWgts
})
