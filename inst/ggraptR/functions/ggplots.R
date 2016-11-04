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
  p <- p + if (ls$density2d) geom_density_2d()
  if (ls$treatAsFactor) {
    p <- p + aes_string(color=ls$colorAsFactor) +
      guides(color=guide_legend(title=ls$color))
  } else {
    p <- p + aes_string(color=ls$color)
  }
  
  flog.debug("plot::plotScatter() - End", name='all')  
  p
}

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

fillPlotWithPointsOverlay <- function(plot, ls) {
  flog.debug("plot::plotPointsOverlay() - Begin", name='all')    
  
  p <- plot + if (is.null(ls$size)) 
    geom_point(aes_string(shape=ls$shapeAsFactor), 
               alpha=ls$alpha, size=ls$sizeMag) else 
                 geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
                            alpha=ls$alpha)
  p <- p + if (!is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
  p <- p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)
  
  flog.debug("plot::plotPointsOverlay() - End", name='all')       
  p
}

plotHistogram <- function(dataset, ls) {
  flog.debug("plot::plotHistogram() - Begin", name='all')
  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_histogram(alpha=ls$alpha, position='identity', binwidth=ls$binWidth) + 
    aes_string(fill=ls$fillAsFactor) +  # ls$position
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  flog.debug("plot::plotHistogram() - End", name='all')    
  p
}

plotDensity <- function(dataset, ls) {
  flog.debug("plot::plotDensity() - Begin", name='all')      

  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_density(alpha=ls$alpha,
                 mapping=do.call(
                   aes_string, c(list(group=ls$fillAsFactor, fill=ls$fillAsFactor),
                                 if (!ls$densBlackLineCond) list(color=ls$fillAsFactor))))
  p <- p + if (!is.null(ls$fill)) do.call(
    guides, c(list(group=guide_legend(title=ls$fill), fill=guide_legend(title=ls$fill)), 
              if (!ls$densBlackLineCond) list(color=guide_legend(title=ls$fill))))
  
  flog.debug("plot::plotDensity() - End", name='all')
  p
}

plotBox <- function(dataset, ls) {
  flog.debug("plot::plotBox() - Begin", name='all')     
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    geom_boxplot(alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  flog.debug("plot::plotBox() - End", name='all')   
  p
}

plotBar <- function(dataset, ls) {
  flog.debug("plot::plotBar() - Begin", name='all')   
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_bar(stat='identity', position='identity', alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor) +  # ls$position
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  flog.debug("plot::plotBar() - End", name='all')
  p
}

plotViolin <- function(dataset, ls) {
  flog.debug("plot::plotViolin() - Begin", name='all')
  p <- ggplot(dataset, aes_string(x=ls$xAsFactor, y=ls$y)) + 
    geom_violin(alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  flog.debug("plot::plotViolin() - End", name='all')
  p
}

plotPairs <- function(dataset, ls) {
  flog.debug("plot::plotPairs() - Begin", name='all')  
  
  ggpairs_pars <- Filter(
    function(x) !is.null(x), 
    list(dataset, columns=ls$columns,
         mapping=aes_string(color=ls$color, fill=ls$fill, alpha=0.5),
         upper=list(continuous=ls$ggpairsUpCont, combo=ls$ggpairsUpCombo, 
                    discrete=ls$ggpairsUpDiscr), 
         diag=list(continuous=ls$ggpairsDiagCont, discrete=ls$ggpairsDiagDiscr), 
         lower=list(continuous=ls$ggpairsLowCont, combo=ls$ggpairsLowCombo, 
                    discrete=ls$ggpairsLowDiscr)))
  
  state <- sys.frames()[[1]]
  for(i in 1:length(ggpairs_pars)) {
    par <- ggpairs_pars[[i]]
    par_name <- names(ggpairs_pars)[[i]]
    if (par_name %in% c('upper', 'diag', 'lower') && length(unlist(par))) {
      state$pairs[[par_name]] <- par[!sapply(par, is.null)]
    }
  }
  
  # print.ggmatrix() is overrided in helper.R script
  p <- do.call(ggpairs, ggpairs_pars)
  
  flog.debug("plot::plotPairs() - End", name='all')  
  p
}
