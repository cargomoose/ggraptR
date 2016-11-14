plotScatter <- function(dataset, ls) {
  flog.debug("plot::plotScatter()", name='all')
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    if (is.null(ls$size))
      geom_point(aes_string(shape=asFactor(ls$shape)), 
                 alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag) else
      geom_point(aes_string(shape=asFactor(ls$shape), size=ls$size), 
                 alpha=ls$alpha, position=ls$jitter)
  p <- p + if (is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
  p <- p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)
  if (ls$treatColorAsFactor) {
    p + aes_string(color=asFactor(ls$color)) +
      guides(color=guide_legend(title=ls$color))
  } else {
    p + aes_string(color=ls$color)
  }
}

plotDensity2d <- function(dataset, ls) {
  ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + geom_density_2d()
}

plotLine <- function(dataset, ls) {
  flog.debug("plot::plotLine()", name='all')
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y))
  if (is.null(ls$color))  {
    p + geom_line(aes(group=1), alpha=ls$alpha) 
  } else {
    p + geom_line(aes_string(group=ls$color), alpha=ls$alpha) +
      aes_string(color=asFactor(ls$color)) + 
      guides(color=guide_legend(title=ls$color))
  }
}

plotPath <- function(dataset, ls) {
  flog.debug("plot::plotPath()", name='all')   
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_path(alpha=ls$alpha) + 
    if (is.null(ls$color)) geom_line(aes(group=1), alpha=ls$alpha) else
      geom_line(aes_string(group=ls$color), alpha=ls$alpha)
  if (!is.null(ls$color)) {
    p <- p + aes_string(color=asFactor(ls$color)) + 
      guides(color=guide_legend(title=ls$color))
  }
  p
}

fillPlotWithPointsOverlay <- function(plot, ls) {
  flog.debug("plot::plotPointsOverlay()", name='all')    
  p <- plot + if (is.null(ls$size)) 
    geom_point(aes_string(shape=asFactor(ls$shape)), 
               alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag) else 
    geom_point(aes_string(shape=asFactor(ls$shape), size=ls$size), 
               alpha=ls$alpha, position=ls$jitter)
  p <- p + if (!is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
  p
}

plotHistogram <- function(dataset, ls) {
  flog.debug("plot::plotHistogram()", name='all')
  ggplot(dataset, aes_string(x=ls$x)) + 
    geom_histogram(alpha=ls$alpha, position=ls$position, bins=ls$nBins) +
    aes_string(fill=asFactor(ls$fill)) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
}

plotDensity <- function(dataset, ls) {
  flog.debug("plot::plotDensity()", name='all')      

  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_density(alpha=ls$alpha,
                 mapping=do.call(
                   aes_string, c(list(group=asFactor(ls$fill), fill=asFactor(ls$fill)),
                                 if (!ls$densBlackLine) list(color=asFactor(ls$fill)))))
  p + if (!is.null(ls$fill)) do.call(
    guides, c(list(group=guide_legend(title=ls$fill), fill=guide_legend(title=ls$fill)), 
              if (!ls$densBlackLine) list(color=guide_legend(title=ls$fill))))
}

plotBox <- function(dataset, ls) {
  flog.debug("plot::plotBox()", name='all')     
  ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    geom_boxplot(alpha=ls$alpha) + 
    aes_string(fill=asFactor(ls$fill)) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
}

plotBar <- function(dataset, ls) {
  flog.debug("plot::plotBar()", name='all')   
  ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_bar(stat='identity', position=ls$position, alpha=ls$alpha) + # posi='identity'
    aes_string(fill=asFactor(ls$fill)) +  # ls$position
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
}

plotViolin <- function(dataset, ls) {
  flog.debug("plot::plotViolin()", name='all')
  dodge <- position_dodge(width = 0.4)
  p <- ggplot(dataset, aes_string(x=asFactor(ls$x), y=ls$y))
  p <- p + if (!is.null(ls$viol_box)) geom_boxplot(width=0.2, position=dodge)
  p <- p + geom_violin(alpha=ls$alpha, position=dodge) + 
    aes_string(fill=asFactor(ls$fill)) +
    if (!is.null(ls$fill)) guides(fill=guide_legend(title=ls$fill))
  p
}

plotPairs <- function(dataset, ls) {
  flog.debug("plot::plotPairs()", name='all')  
  
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
  do.call(ggpairs, ggpairs_pars)
}
