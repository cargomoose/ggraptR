
## display plot and data table
observeEvent(input$reactive, {
  shinyBS::updateButton(session, "submit", disabled = input$reactive==TRUE)
  
  if (input$reactive) {

    ## display plot reactively
    output$plot <- renderPlot({
      plotInput()
    }, height=700)
    
    ## display data table reactively
    output$displayTable <- DT::renderDataTable({
      DT::datatable(manAggDataset(), filter='bottom')
    })
    
  } else {

    ## display plot upon submit
    output$plot <- renderPlot({
      input$submit
      isolate(plotInput())
    }, height=700)
    
    ## display data table upon submit
    output$displayTable <- DT::renderDataTable({
      input$submit
      isolate(DT::datatable(manAggDataset(), filter='bottom'))
    })
  }
})

## view plot from import tab
observe({
  if(input$viewPlot > 0){
    session$sendCustomMessage("myCallbackHandler", "1")
  }
})

observeEvent(input$reset_input, {
  updateCheckboxInput(session, "reactive", value = FALSE)
  Sys.sleep(0.5)
  shinyjs::reset("dataset")
  shinyjs::reset("plotType")
  shinyjs::reset('rawVsManAgg')
  shinyjs::reset('plotAggMeth')
  shinyjs::reset('x')
  shinyjs::reset('y')
  shinyjs::reset('color')
  shinyjs::reset('treatAsFacVarCol')
  shinyjs::reset('fill')
  shinyjs::reset('position')
  shinyjs::reset('jitter')
  shinyjs::reset('smooth')
  shinyjs::reset('size')
  shinyjs::reset('shape')
  shinyjs::reset('binWidth')
  shinyjs::reset('densBlkLineCond')
  shinyjs::reset('ptsOverlayCond')
  shinyjs::reset('facetRow')
  shinyjs::reset('facetCol')
  shinyjs::reset('facetWrap')
  shinyjs::reset('facetScale')
  shinyjs::reset('alpha')
  shinyjs::reset('sizeMag')
  shinyjs::reset('coordFlip')
  shinyjs::reset('plotAddAggBy')
  shinyjs::reset('xlim')
  shinyjs::reset('ylim')
  shinyjs::reset('plotTitle')
  shinyjs::reset('xLabel')
  shinyjs::reset('yLabel')
  shinyjs::reset('labelFontFamily')
  shinyjs::reset('labelFontFace')
  shinyjs::reset('labelFontSize')
  shinyjs::reset('labelFontColor')
  shinyjs::reset('hjust')
  shinyjs::reset('vjust')
  shinyjs::reset('plotTheme')
  shinyjs::reset('showAesWgts')
  shinyjs::reset('showFacetWgts')
  shinyjs::reset('showXYRangeWgts')
  shinyjs::reset('showPlotAggWgt')
  shinyjs::reset('showThemeWgts')
  shinyjs::reset('showDSTypeAndPlotAggWgts')
  Sys.sleep(0.5)
  updateCheckboxInput(session, "reactive", value = TRUE)
})

## disable/enable toggle between facet grid and facet wrap
observeEvent(c(input$facetCol, input$facetRow, input$facetWrap), {
  if (input$showFacetWgts) {
    if (noFacetSelected()) {
      shinyjs::enable('facetCol')
      shinyjs::enable('facetRow')
      shinyjs::enable('facetWrap')
    } else if (facetGridSelected()) {
      shinyjs::enable('facetCol')
      shinyjs::enable('facetRow')
      shinyjs::disable('facetWrap')
    } else if (facetWrapSelected()) {
      shinyjs::disable('facetCol')
      shinyjs::disable('facetRow')
      shinyjs::enable('facetWrap')
    } 
  }
})

## disable plot title, x and y label text fields when reactivity is enabled
# observeEvent(c(input$plotTitle, input$xLabel, input$yLabel), {
#   if (input$showThemeWgts) {
#     if (plotLabelWidgetsLoaded()) {
#       if (input$reactive) {
#         print('hi')
#         disable('plotTitle')
#         disable('xLabel')
#         disable('yLabel')
#       } else {
#         print('bye')
#         enable('plotTitle')
#         enable('xLabel')
#         enable('yLabel')
#       }
#     }
#   }
# })
