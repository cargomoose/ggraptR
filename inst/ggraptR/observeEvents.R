# display plot or table
observeEvent(input$reactive, {
  shinyBS::updateButton(session, "submit", disabled = input$reactive)
  
  if (input$reactive) {
    output$plot <- renderPlot({  # display plot reactively
      buildPlot()
    }, height=700)
    output$displayTable <- DT::renderDataTable({  # display data table reactively
      DT::datatable(manAggDataset(), filter='bottom')
    })
  } else {
    output$plot <- renderPlot({  # display plot upon submit
      input$submit
      isolate(buildPlot())
    }, height=700)
    output$displayTable <- DT::renderDataTable({  # display data table upon submit
      input$submit
      isolate(DT::datatable(manAggDataset(), filter='bottom'))
    })
  }
})

# delay plot building until all controls will be ready
observe({
  nInp <- input$controlsLoadingInp
  isolate({
    n <- controlsLoading$itersToDrawPlot
    if (notNulls(nInp, n) && n != 0) controlsLoading$itersToDrawPlot <- n - 1
  })
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
  if (input$showFacet) {
    if (!isFacetSelected()) {
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
#   if (input$showTheme) {
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
