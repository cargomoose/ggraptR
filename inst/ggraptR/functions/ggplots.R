plotGgplot <- function(dataset, inpVals) {
  pMap <- c('box'='boxplot', 'scatter'='point')
  # ensure all plots have y==T or y==F
  stopifnot(length(unique(sapply(inpVals, function(pt) is.null(pt$y)))) == 1)
  x <- if (needCatX(names(inpVals))) asFactor(inpVals[[1]]$x) else inpVals[[1]]$x
  p <- ggplot(dataset, do.call(aes_string, trimList(x=x, y=inpVals[[1]]$y)))
  
  for (lsi in 1:length(inpVals)) {
    ls <- inpVals[[lsi]]
    pType <- names(inpVals)[lsi]
    # if (pType == 'histogram') stop('histogram demo error message')  ####
    ggpType <- paste0('geom_', if (pType %in% names(pMap)) pMap[[pType]] else pType)
    apply <- list(sizeMag=!is.null(ls$sizeMag) && is.null(ls$size),
                  densBlackLine=!is.null(ls$densBlackLine) && !ls$densBlackLine)
    
    geomMapArgs <- trimList(
      shape=asFactor(ls$shape),
      fill=asFactor(ls$fill),
      size=ls$size,
      color=if (!is.null(ls$treatColorAsFactor) && ls$treatColorAsFactor) 
        asFactor(ls$color) else if (apply$densBlackLine) ls$fill else ls$color)
    
    p <- p + do.call(ggpType, trimList(
      mapping= do.call(aes_string, geomMapArgs),
      alpha=ls$alpha, 
      bins=ls$nBins, 
      position=if (!is.null(ls$jitter)) ls$jitter else 
        if (pType %in% c('box', 'violin')) position_dodge(width=0.4) else ls$position,
      size=if (apply$sizeMag) ls$sizeMag,
      stat=if (pType == 'bar') 'identity',
      width=if (pType == 'box') 0.2))
    
    guides_args <- na_omit(sapply(names(geomMapArgs), function(aes) {
      if (apply$densBlackLine && aes == 'color') {
        guide_legend(title=ls$fill)
      } else if (grepl('^as.factor', geomMapArgs[[aes]])) {
        guide_legend(title=ls[[aes]])
      }
    }, simplify=F))
    
    p <- p + if (length(guides_args)) do.call(guides, guides_args)
    p <- p + if (apply$sizeMag) scale_size(range=c(1, ls$sizeMag))
    p <- p + if (!is.null(ls$smooth)) {
      # we need to avoid two different color aestetics: one in geom_, one in smooth 
      # That's why 'else if(is.null(ls$color))' is used
      smoothMapGrp <- if (!is.null(ls$color) && 
                          ls$color %in% getVarNamesUniqValsCntLOEN(dataset))
        geomMapArgs$color else if (is.null(ls$color)) geomMapArgs$shape
      do.call(stat_smooth,   # stat_smooth(method=.., mapping=..orNull)
              trimList(method=ls$smooth,
                       mapping=if (!is.null(smoothMapGrp))aes_string(color=smoothMapGrp)))
    }
  }
  p
}


plotPairs <- function(dataset, inpVals) {
  stopifnot(length(inpVals) == 1)
  ls <- inpVals[[1]]  
  
  ggpairs_pars <- Filter(
    function(x) !is.null(x), 
    list(dataset, columns=ls$columns,
         # alpha doesnt distinguish 0.2 from 0.8. It's boolean. Looks like a ggpairs bug
         mapping=aes_string(color=ls$color, fill=ls$fill, alpha=0.5),
         upper=list(continuous=ls$pairsUpCont, combo=ls$pairsUpCombo, 
                    discrete=ls$pairsUpDiscr), 
         diag=list(continuous=ls$pairsDiagCont, discrete=ls$pairsDiagDiscr), 
         lower=list(continuous=ls$pairsLowCont, combo=ls$pairsLowCombo, 
                    discrete=ls$pairsLowDiscr)))
  
  reactValPairsAes <- list()
  for(i in 1:length(ggpairs_pars)) {
    par <- ggpairs_pars[[i]]
    par_name <- names(ggpairs_pars)[[i]]
    if (par_name %in% c('upper', 'diag', 'lower') && length(unlist(par))) {
      reactValPairsAes[[par_name]] <- par[!sapply(par, is.null)]
    }
  }
  
  # print.ggmatrix() is overrided in helper.R
  p <- do.call(ggpairs, ggpairs_pars)
  p$pairsAes <- reactValPairsAes
  p
}
