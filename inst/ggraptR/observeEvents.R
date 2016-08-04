
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
  # I hope there is some way to extract these ids from env
  ids <- c('dataset', 'plotType', 'rawVsManAgg', 'plotAggMeth', 'x', 'y', 'color', 
           'treatAsFacVarCol', 'fill', 'position', 'jitter', 'smooth', 'size', 
           'shape', 'binWidth', 'densBlkLineCond', 'ptsOverlayCond', 'facetRow', 
           'facetCol', 'facetWrap', 'facetScale', 'alpha', 'sizeMag', 'coordFlip', 
           'plotAddAggBy', 'xlim', 'ylim', 'plotTitle', 'xLabel', 'yLabel', 
           'labelFontFamily', 'labelFontFace', 'labelFontSize', 'labelFontColor', 
           'hjust', 'vjust', 'plotTheme', 'showAesWgts', 'showFacetWgts', 
           'showXYRangeWgts', 'showPlotAggWgt', 'showThemeWgts', 'showDSTypeAndPlotAggWgts')
  for (id in ids) {
    shinyjs::reset(id)
  }
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
