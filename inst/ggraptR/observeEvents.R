# display plot or table
observeEvent(input$reactive, {
  updateButton(session, "submit", disabled = input$reactive)
  
  if (input$reactive) {
    output$plot <- renderPlot({  # display plot reactively
      buildPlot()
    }, height=700)
    output$displayTable <- DT::renderDataTable({  # display data table reactively
      DT::datatable(manAggDataset(), filter='bottom')
    })
  } else {
    output$plot <- renderPlot({  # display plot upon submit
      c(input$submit, reactVals$readyToDraw)  # dependencies
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
  nInp <- input$itersToDrawInp
  isolate({
    n <- reactVals$itersToDraw
    if (notNulls(nInp, n) && n != 0) reactVals$itersToDraw <- n - 1
  })
})

# trigger update for plopType options
observe({
  pTypes <- plotTypes()
  isolate({
    if (is.null(dataset())) return()
    allDefTypes <- unlist(getStructListNames(getDefinedPlotInputs()))
    needOneGroupOpts <- length(pTypes) == 1 && 
      length(plotTypesOpts()) == length(allDefTypes)
    
    if (is.null(pTypes) || needOneGroupOpts) {
      reactVals$plotTypeOptsTrigger <- Sys.time()  # Sys.time() for trigger
    }
  })
})

# reset inputs
observeEvent(input$reset_input, {
  updateCheckboxInput(session, "reactive", value = FALSE)
  Sys.sleep(0.5)
  # setdiff prevents very unstable bug of infinite recursive refresh of plotTypes
  for (id in setdiff(names(input), 'dataset')) {
    reset(id)
  }
  Sys.sleep(0.5)
  updateCheckboxInput(session, "reactive", value = TRUE)
})

# disable/enable toggle between facet grid and facet wrap
observeEvent(c(input$facetCol, input$facetRow, input$facetWrap), {
  if (showFacet()) {
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

# disable plot title, x and y label text fields when reactivity is enabled
# observeEvent(c(input$plotTitle, input$xLabel, input$yLabel), {
#   if (input$showTheme) {
#     if (checkWidgetsLoaded(input, c('plotTitle', 'xLabel', 'yLabel'))) {
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
