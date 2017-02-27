# aggregation-by options
output$aggByCtrl <- renderUI({
  selectInput('aggBy', 'Aggregate By', choices=colnames(rawDataset()), multiple=T)
})

# aggregation target options
output$aggTargetCtrl <- renderUI({
  selectInput('aggTarget', 'Aggregation Target', getIsNumericVarNames(rawDataset()), 
              multiple=T)
})

# aggregation method options
output$aggMethCtrl <- renderUI({
  aggMethOpts <- c('mean', 'sum', 'count', 'min', 'max', 'median')
  selectInput('aggMeth', 'Aggregation Method', choices=aggMethOpts, multiple=T)
})
