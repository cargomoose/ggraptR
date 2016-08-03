class ggraptplot:
  #attributes
  p = ggplot()
  lAttributes = list()
  
  #functions
  function extractPlotAttributes (plot) {
    for all attribute in plot:
    extract attribute #?? regex ??
    store attribute in list
    return list
  }
  
  function <- (plot) {
    p = plot
    lAttributes = extractPlotAttributes(plot)
  }
  
  function getPlot () {
    return p
  }
  
  function generateCode () {
    sCodeGen = ""
    for all attribute in lAttribute: # obviously it won't be this simple
    sCodeGen = sCodeGen + attribute
    return sCodeGen
  }
  