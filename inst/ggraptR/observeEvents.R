# display plot or table
observeEvent(input$reactive, {
  shinyBS::updateButton(session, "submit", disabled = input$reactive)
  
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

# view plot from import tab
observe({
  if (input$viewPlot > 0){
    session$sendCustomMessage("myCallbackHandler", "1")
  }
})

# reset inputs
observeEvent(input$reset_input, {
  updateCheckboxInput(session, "reactive", value = FALSE)
  Sys.sleep(0.5)
  for (id in names(input)) {
    shinyjs::reset(id)
  }
  Sys.sleep(0.5)
  updateCheckboxInput(session, "reactive", value = TRUE)
})

# disable/enable toggle between facet grid and facet wrap
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
