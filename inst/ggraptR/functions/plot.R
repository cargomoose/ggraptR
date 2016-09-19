## function for scatter plot
plotScatter <- function(dataset, ls) {
  flog.debug("plot::plotScatter() - Begin", name='all')  
  
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    if (is.null(ls$size)) 
    geom_point(aes_string(shape=ls$shapeAsFactor), 
               alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag) else
    geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
               alpha=ls$alpha, position=ls$jitter)
  p <- p + if (is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
  p <- p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)
  if (ls$treatAsFacVarCol) {
    p <- p + aes_string(color=ls$colorAsFactor) +
      guides(color=guide_legend(title=ls$color))
  } else {
    p <- p + aes_string(color=ls$color)
  }
  
  flog.debug("plot::plotScatter() - End", name='all')  
  p
}

## function for points overlay
plotPointsOverlay <- function(plot, ls) {
  flog.debug("plot::plotPointsOverlay() - Begin", name='all')    
  
  p <- plot + if (is.null(ls$size)) 
    geom_point(aes_string(shape=ls$shapeAsFactor), 
               alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag) else 
    geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
               alpha=ls$alpha, position=ls$jitter)
  p <- p + if (!is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
  p <- p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)

  flog.debug("plot::plotPointsOverlay() - End", name='all')       
  p
}

## function for line plot
plotLine <- function(dataset, ls) {
  flog.debug("plot::plotLine() - Begin", name='all')
  
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y))
  if (is.null(ls$color))  {
    p <- p + geom_line(aes(group=1), alpha=ls$alpha) 
  } else {
    p <- p + geom_line(aes_string(group=ls$color), alpha=ls$alpha) +
      aes_string(color=ls$colorAsFactor) + 
      guides(color=guide_legend(title=ls$color))
  }
  
  flog.debug("plot::plotLine() - End", name='all')
  p
}

## function for path plot
plotPath <- function(dataset, ls) {
  flog.debug("plot::plotPath() - Begin", name='all')   
  
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_path(alpha=ls$alpha) + 
    if (is.null(ls$color)) geom_line(aes(group=1), alpha=ls$alpha) else
      geom_line(aes_string(group=ls$color), alpha=ls$alpha)
  if (!is.null(ls$color)) {
    p <- p + aes_string(color=ls$colorAsFactor) + 
      guides(color=guide_legend(title=ls$color))
  }
  
  flog.debug("plot::plotPath() - End", name='all')    
  p
}

## function for histogram
plotHistogram <- function(dataset, ls) {
  flog.debug("plot::plotHistogram() - Begin", name='all')
  
  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_histogram(alpha=ls$alpha, position='identity', binwidth=ls$binWidth) + 
    aes_string(fill=ls$fillAsFactor) +  # ls$position
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  
  flog.debug("plot::plotHistogram() - End", name='all')    
  p
}

## function for density plot 
plotDensity <- function(dataset, ls) {
  flog.debug("plot::plotDensity() - Begin", name='all')      

  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_density(alpha=ls$alpha,
                 mapping=do.call(aes_string, 
                                 c(list(group=ls$fillAsFactor, fill=ls$fillAsFactor),
                                   if (!ls$densBlkLineCond) list(color=ls$fillAsFactor))))
  p <- p + if (!is.null(ls$fill)) 
    do.call(guides, c(list(group=guide_legend(title=ls$fill), 
                           fill=guide_legend(title=ls$fill)), 
                      if (!ls$densBlkLineCond) list(color=guide_legend(title=ls$fill))))
  
  flog.debug("plot::plotDensity() - End", name='all')   
  p
}

## function for box plot
plotBox <- function(dataset, ls) {
  flog.debug("plot::plotBox() - Begin", name='all')     
  
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    geom_boxplot(alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  
  flog.debug("plot::plotBox() - End", name='all')   
  p
}

## function for bar plot
plotBar <- function(dataset, ls) {
  flog.debug("plot::plotBar() - Begin", name='all')   

  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_bar(stat='identity', position='identity', alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor) +  # ls$position
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  
  flog.debug("plot::plotBar() - End", name='all')
  p
}

## function for scatter plot
plotPairs <- function(dataset, ls) {
  flog.debug("plot::plotPairs() - Begin", name='all')  
  
  ggpairs_pars <- Filter(
    function(x) !is.null(x), 
    list(dataset, columns=ls$columns,
         mapping=aes_string(color=ls$color, fill=ls$fill, alpha=ls$alpha),
         upper=list(continuous=ls$upCont, combo=ls$upCombo, discrete=ls$upDiscr), 
         diag=ls$diag, lower=ls$low))
  
  # print.ggmatrix is overrided in helper.R script
  p <- do.call(ggpairs, ggpairs_pars)
  
  flog.debug("plot::plotPairs() - End", name='all')  
  p
}
