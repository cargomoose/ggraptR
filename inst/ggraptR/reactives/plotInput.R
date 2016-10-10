## scatter plot inputs
scatterPlotInputs <- reactive({
  flog.debug("plot::scatterPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) {
    flog.debug("plot::scatterPlotInputs() - (is.null(plotDF()) - End", name='all')
    return()
  }
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), 
              treatAsFacVarCol=treatAsFacVarCol(), shape=shape(), 
              shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), 
              jitter=jitter(),alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::scatterPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line plot inputs
linePlotInputs <- reactive({
  flog.debug("plot::linePlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
  flog.debug("plot::linePlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## line points overlay inputs
linePtsOverlayInputs <- reactive({
  flog.debug("plot::linePtsOverlayInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), 
              smooth=smooth(), jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::linePtsOverlayInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})


## bar plot inputs
barPlotInputs <- reactive({
  flog.debug("plot::barPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), 
              position=position(), alpha=alpha())
  flog.debug("plot::barPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## histogram inputs
histogramInputs <- reactive({
  flog.debug("plot::histogramInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), position=position(), 
              binWidth=binWidth(), alpha=alpha())
  flog.debug("plot::histogramInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## density plot inputs
densityPlotInputs <- reactive({
  flog.debug("plot::densityPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), fill=fill(), fillAsFactor=fillAsFactor(), 
              densBlkLineCond=densBlkLineCond(), alpha=alpha())
  flog.debug("plot::densityPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## box plot inputs
boxPlotInputs <- reactive({
  flog.debug("plot::boxPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), fill=fill(), fillAsFactor=fillAsFactor(), alpha=alpha())
  flog.debug("plot::boxPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path plot inputs
pathPlotInputs <- reactive({
  flog.debug("plot::pathPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(x=x(), y=y(), color=color(), colorAsFactor=colorAsFactor(), alpha=alpha())
  flog.debug("plot::pathPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## path points overlay inputs
pathPtsOverlayInputs <- reactive({
  flog.debug("plot::pathPtsOverlayInputs() - Begin", name='all')
  if (is.null(plotDF())) return()
  pil <- list(shape=shape(), shapeAsFactor=shapeAsFactor(), size=size(), smooth=smooth(), 
              jitter=jitter(), alpha=alpha(), sizeMag=sizeMag())
  flog.debug("plot::pathPtsOverlayInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

pairsPlotInputs <- reactive({
  flog.debug("plot::pairsPlotInputs() - Begin", name='all')
  if (is.null(plotDF())) return()

  pil <- list(
    columns=columns(), color=color(), fill=fill(), alpha=alpha(),
    upCont=ggpairsUpCont(), upCombo=ggpairsUpCombo(), upDiscr=ggpairsUpDiscr(),
    diagCont=ggpairsDiagCont(), diagDiscr=ggpairsDiagDiscr(),
    lowCont=ggpairsLowCont(), lowCombo=ggpairsLowCombo(), lowDiscr=ggpairsLowDiscr())
  flog.debug("plot::pairsPlotInputs() - End", name='all')
  ensureCorrectPlotInputs(pil, colnames(plotDF()))
})

## plot reactive
plotInput <- reactive({
  flog.debug("plot::plotInput() - Begin", name='all')
  flog.debug("systime - begin", name='all')
  start.time <- Sys.time()
  flog.debug(start.time, name='all')
  
  ptype <- plotType()
  is_horis_axis_actual <- if (ptype == 'pairs') 
    !is.null(columns()) && columns() %in% colnamesOpts() else 
      !is.null(x()) && x() %in% colnamesOpts()
  if (!universalPlotWidgetsLoaded() || !is_horis_axis_actual) return()

  p <- do.call(paste0(ptype, if (ptype == 'histogram') '' else 'Plot'), list())
  if (is.null(p)) return()
  
  if (ptype != 'pairs') {
    if (!noFacetSelected()) {
      if (facetGridSelected()) {
        p <- p + facet_grid(facets=facetGrids(), scales=facetScale())
      } else if (facetWrapSelected()) {  ## facet wrap
        p <- p + facet_wrap(facets=facetWrap(), scales=facetScale())
      }
    }
    
    p <- p + if (coordFlip()) coord_flip()
    
    ## plot labels 
    p <- p + if (!is.null(plotTitle()) && plotTitle() != '') ggtitle(plotTitle())
    p <- p + if (!is.null(xLabel()) && xLabel() != '') xlab(xLabel())
    p <- p + if (!is.null(yLabel()) && yLabel() != '') ylab(yLabel())
    
    ## plot themes
    if (!is.null(plotTheme())) {
      p <- p + do.call(plotTheme(), list())
      
      theme_name <- rev(unlist(str_split(plotTheme(), '_')))[1]
      
      state$theme_name <- theme_name
      color_type_flag <- !is.null(colorType()) && colorType() == 'discrete'
      if (!theme_name %in% c('grey', 'bw', 'economist')) {
        scale_color_name <- sprintf('scale_colour_%s', theme_name)
        if (theme_name == 'calc') {
          scale_color_name <- sub('u', '', scale_color_name)
        }
        p <- p + if (color_type_flag && theme_name != 'tufte') 
          do.call(scale_color_name, list())
      } else if (theme_name == 'economist') {
        p <- p + scale_colour_economist() + if (color_type_flag) scale_color_calc()
      }
    }
    
    ## plot label styles
    state$theme_attrs <- list(family = labelFontFamily(),
                              face = labelFontFace(),
                              color = labelFontColor(),
                              size = labelFontSize(),
                              hjust = hjust(),
                              vjust = vjust())
    p <- p + theme(text=do.call(element_text, state$theme_attrs))
  }
  
  flog.debug("plot::plotInput() - End", name='all')
  flog.debug("proctime - end", name='all')
  end.time <- Sys.time()
  flog.debug(end.time, name='all')
  time.taken <- end.time - start.time
  flog.debug("time.taken", name='all')
  flog.debug(time.taken , name='all')
  
  # add plot history entry
  if (!is.null(p)) {
    logEntry <- generateCode(p)
    curLog <- isolate(log$plot)
    isFirstEntry <- is.null(curLog)
    
    if (isFirstEntry || curLog[[1]] != logEntry) {
      log$plot <- if (isFirstEntry) logEntry else c(logEntry, curLog)
    }
  }
  
  p
})
