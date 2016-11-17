fillPlotWithPointsOverlay <- function(plot, ls) {
  p <- plot + if (is.null(ls$size)) 
    geom_point(aes_string(shape=asFactor(ls$shape)),
               alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag) else 
                 geom_point(aes_string(shape=asFactor(ls$shape), size=ls$size), 
                            alpha=ls$alpha, position=ls$jitter)
  p <- p + if (!is.null(ls$size)) scale_size(range=c(1, ls$sizeMag))
  p <- p + if (!is.null(ls$shape)) guides(shape = guide_legend(title=ls$shape))
}

addAes <- function(p, ls, aesKey, aesVal=NULL, as_factor=T) {
  if (is.null(aesVal)) aesVal <- ls[[aesKey]]
  if (is.null(aesVal)) return(p)
  args <- list()
  args[[aesKey]] <- if (as_factor) asFactor(aesVal) else aesVal
  
  p <- p + do.call(aes_string, args)
  if (as_factor) {
    args[[aesKey]] <- guide_legend(title=aesVal)
    p <- p + do.call(guides, args)
  }
  p  
}

plotGgplot <- function(dataset, ls, pTypes) {
  pMap <- c('box'='boxplot', 'scatter'='point')
  ggpTypes <- paste0('geom_', if (pTypes %in% names(pMap)) pMap[[pTypes]] else pTypes)
  apply <- list(sizeMag=!is.null(ls$sizeMag) && is.null(ls$size),
                densBlackLine=!is.null(ls$densBlackLine) && !ls$densBlackLine)
  
  p <- ggplot(dataset, do.call(aes_string, trimList(x=ls$x, y=ls$y)))
  p <- p + do.call(ggpTypes, trimList(  # geom_bar(stat='identity'
    alpha=ls$alpha, 
    bins=ls$nBins, 
    position=if (!is.null(ls$jitter)) ls$jitter else 
             if (pTypes == 'violin') position_dodge(width = 0.4) else ls$position, 
    size=if (apply$sizeMag) ls$sizeMag,
    stat=if (pTypes == 'bar') 'identity'))
  
  p <- addAes(p, ls, 'shape')
  p <- addAes(p, ls, 'fill')
  p <- addAes(p, ls, 'size', as_factor=F)
  p <- addAes(p, ls, 'color', 
              as_factor=is.null(ls$treatColorAsFactor) || ls$treatColorAsFactor)
  if (apply$densBlackLine) p <- addAes(p, ls, aesKey='color', aesVal=ls$fill)
  
  if (apply$sizeMag) p <- p + scale_size(range=c(1, ls$sizeMag))
  p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)
  
  # if (!is.null(ls$viol_box)) 
    # geom_boxplot(width=0.2, position=position_dodge(width = 0.4))
}


# multiple columns
plotPairs <- function(dataset, ls, pTypes) {
  flog.debug("plot::plotPairs()", name='all')  
  
  ggpairs_pars <- Filter(
    function(x) !is.null(x), 
    list(dataset, columns=ls$columns,
         # alpha does not distinguish 0.2 from 0.8. It's binary. Looks like a ggpairs bug
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
