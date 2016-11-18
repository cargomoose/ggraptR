generateAes <- function(ls) {
  args <- trimList(
    shape=asFactor(ls$shape),
    fill=asFactor(ls$fill),
    size=ls$size,
    color=if (is.null(ls$treatColorAsFactor) || ls$treatColorAsFactor) 
      asFactor(ls$color) else ls$color
    # if (apply$densBlackLine) addAes(ls, aesKey='color', aesVal=ls$fill)
  )
  do.call(aes_string, args)
}

plotGgplot <- function(dataset, inpVals) {
  pMap <- c('box'='boxplot', 'scatter'='point')
  # ensure all plots have y==T or y==F
  stopifnot(length(unique(sapply(inpVals, function(pt) is.null(pt$y)))) == 1)
  x <- if (needCatX(names(inpVals))) asFactor(inpVals[[1]]$x) else inpVals[[1]]$x
  p <- ggplot(dataset, do.call(aes_string, trimList(x=x, y=inpVals[[1]]$y)))
  
  for (lsi in 1:length(inpVals)) {
    ls <- inpVals[[lsi]]
    pType <- names(inpVals)[lsi]
    ggpType <- paste0('geom_', if (pType %in% names(pMap)) pMap[[pType]] else pType)
    apply <- list(sizeMag=!is.null(ls$sizeMag) && is.null(ls$size),
                  densBlackLine=!is.null(ls$densBlackLine) && !ls$densBlackLine)
    p <- p + do.call(ggpType, trimList(  # geom_bar(stat='identity'
      mapping=generateAes(ls),
      alpha=ls$alpha, 
      bins=ls$nBins, 
      position=if (!is.null(ls$jitter)) ls$jitter else 
        if (pType %in% c('box', 'violin')) position_dodge(width=0.4) else ls$position,
      size=if (apply$sizeMag) ls$sizeMag,
      stat=if (pType == 'bar') 'identity',
      width=if (pType == 'box') 0.2))
  
    p <- p + if (apply$sizeMag) scale_size(range=c(1, ls$sizeMag))
    p <- p + if (!is.null(ls$smooth)) stat_smooth(method=ls$smooth)
  }
  p
}


# multiple columns
plotPairs <- function(dataset, inpVals) {
  stopifnot(length(inpVals) == 1)
  ls <- inpVals[[1]]  
  
  ggpairs_pars <- Filter(
    function(x) !is.null(x), 
    list(dataset, columns=ls$columns,
         # alpha doesnt distinguish 0.2 from 0.8. It's boolean. Looks like a ggpairs bug
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
