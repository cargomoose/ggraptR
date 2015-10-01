
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


## disable/enable toggle between facet grid and facet wrap
observeEvent(c(input$facetCol, input$facetRow, input$facetWrap), {
  if (input$showFacetWgts) {
    if (noFacetSelected()) {
      enable('facetCol')
      enable('facetRow')
      enable('facetWrap')
    } else if (facetGridSelected()) {
      enable('facetCol')
      enable('facetRow')
      disable('facetWrap')
    } else if (facetWrapSelected()) {
      disable('facetCol')
      disable('facetRow')
      enable('facetWrap')
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
