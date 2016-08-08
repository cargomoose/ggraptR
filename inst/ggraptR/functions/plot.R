get_ggplot <- function(dataset, aes_obj) {
  p <- ggplot(dataset, aes_obj)
  class(p) <- append(class(p), "rappy")
  p$rappy$dataset_name <- 'diamonds'  # ugly hardcoded. Need to obtain input$dataset here
  # plotDF() does not work
  p
}

## function for line plot
plotLine <- function(dataset, ls) {
  
  flog.debug("plot::plotLine() - Begin", name='all')
  
  p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y))
  
  if (is.null(ls$color)) {
    p <- p + geom_line(aes(group=1), alpha=ls$alpha)
  } else {
    p <- p + geom_line(aes_string(group=ls$color), alpha=ls$alpha)
  }

  if (!is.null(ls$color)) {
    p <- p + aes_string(color=ls$colorAsFactor) + 
      guides(color = guide_legend(title=ls$color))
  }
  
  flog.debug("plot::plotLine() - End", name='all')
  
  return(p)
}


## function for scatter plot
plotScatter <- function(dataset, ls) {
  
  flog.debug("plot::plotScatter() - Begin", name='all')  
  
  if (!is.null(ls$size)) {
    p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
      geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
                 alpha=ls$alpha, position=ls$jitter) + 
      scale_size(range = c(1, ls$sizeMag))
  } else {
    p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
      geom_point(aes_string(shape=ls$shapeAsFactor), 
                 alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag)
  }

  ## coloring points
  if (ls$treatAsFacVarCol) {
    p <- p + aes_string(color=ls$colorAsFactor)
    p <- p + guides(color = guide_legend(title=ls$color))
  } else {
    p <- p + aes_string(color=ls$color)
  }
  
  ## legend label for point shapes
  if (!is.null(ls$shape)) {
    p <- p + guides(shape = guide_legend(title=ls$shape))
  }
  
  ## line smoothing
  if (!is.null(ls$smooth)) {
    p <- p + stat_smooth(method=ls$smooth)
  }
  
  flog.debug("plot::plotScatter() - End", name='all')  

  return(p)
}


## function for points overlay
plotPointsOverlay <- function(plot, ls) {
  
  flog.debug("plot::plotPointsOverlay() - Begin", name='all')    
  
  ## 
  if (!is.null(ls$size)) {
    p <- plot + 
      geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
                 alpha=ls$alpha, position=ls$jitter) + 
      scale_size(range = c(1, ls$sizeMag))
  } else {
    p <- plot + 
      geom_point(aes_string(shape=ls$shapeAsFactor), 
                 alpha=ls$alpha, position=ls$jitter, size=ls$sizeMag)
  }
  
  ## legend label for point shapes
  if (!is.null(ls$shape)) {
    p <- p + guides(shape = guide_legend(title=ls$shape))
  }
  
  if (!is.null(ls$smooth)) {
    p <- p + stat_smooth(method=ls$smooth)
  }

  flog.debug("plot::plotPointsOverlay() - End", name='all')       
  
  return(p)
}

## function for histogram
plotHistogram <- function(dataset, ls) {
  
  flog.debug("plot::plotHistogram() - Begin", name='all')
  
  p <- get_ggplot(dataset, aes_string(x=ls$x)) + 
    geom_histogram(alpha=ls$alpha, position='identity', binwidth=ls$binWidth) + 
    aes_string(fill=ls$fillAsFactor) #ls$position
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }
  
  flog.debug("plot::plotHistogram() - End", name='all')    

  return(p)
}


## function for density plot 
plotDensity <- function(dataset, ls) {
  
  flog.debug("plot::plotDensity() - Begin", name='all')      

  p <- get_ggplot(dataset, aes_string(x=ls$x)) 
  if (ls$densBlkLineCond) {
    p <- p + geom_density(aes_string(group=ls$fillAsFactor, fill=ls$fillAsFactor), alpha=ls$alpha)
    if (!is.null(ls$fill)) 
      p <- p + guides(group = guide_legend(title=ls$fill),
                      fill = guide_legend(title=ls$fill))
  } else {
    p <- p + geom_density(aes_string(group=ls$fillAsFactor, color=ls$fillAsFactor, fill=ls$fillAsFactor), alpha=ls$alpha)
    if (!is.null(ls$fill))
      p <- p + guides(group = guide_legend(title=ls$fill),
                      color = guide_legend(title=ls$fill),
                      fill = guide_legend(title=ls$fill))
  }
  
  flog.debug("plot::plotDensity() - End", name='all')   
  
  return(p)
}


## function for bar plot
plotBar <- function(dataset, ls) {
  
  flog.debug("plot::plotBar() - Begin", name='all')   

  p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_bar(stat='identity', position='identity', alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor)#ls$position
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }
  
  flog.debug("plot::plotBar() - End", name='all')     

  return(p)
}

## function for box plot
plotBox <- function(dataset, ls) {
  
  flog.debug("plot::plotBox() - Begin", name='all')     
  
  p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    geom_boxplot(alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }
  
  flog.debug("plot::plotBox() - End", name='all')   

  return(p)
}


## function for path plot
plotPath <- function(dataset, ls) {
  
  flog.debug("plot::plotPath() - Begin", name='all')   
  
  p <- get_ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_path(alpha=ls$alpha)
  
  if (is.null(ls$color)) {
    p <- p + geom_line(aes(group=1), alpha=ls$alpha)
  } else {
    p <- p + geom_line(aes_string(group=ls$color), alpha=ls$alpha)
  }
  
  if (!is.null(ls$color)) {
    p <- p + aes_string(color=ls$colorAsFactor) + 
      guides(color = guide_legend(title=ls$color))
  }
  
  flog.debug("plot::plotPath() - End", name='all')    
  
  return(p)
}

snakeize <- function(camel_str) {
  s <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", camel_str, perl = TRUE)
  sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case
}

parse_kv <- function(str, sep='_') {
  do.call(sprintf, as.list(unlist(c('%s="%s"', str_split(str, sep)))))
}

wrap_quote <- function(el) {
  if (is.character(el)) {
    sprintf('"%s"', el)
  } else if (is.language(el)) {
    as.character(list(el))
  } else as.character(el)
}

clist <- function(arg_lst) {
  arg_lst <- arg_lst[!sapply(arg_lst, is.null)]
  paste(sapply(1:length(arg_lst), function(par_i) 
    sprintf('%s=%s', names(arg_lst)[par_i], wrap_quote(arg_lst[[par_i]]))), collapse=', ')
}

generateCode <- function(p) {
  # useful thing: geom_bar(alpha=0.8)$print
  
  # reorder inner aes arguments
  for (sorted_arg in c('y', 'x')) {
    if (names(p$mapping)[1] != sorted_arg && sorted_arg %in% names(p$mapping)) {
      arg <- p$mapping[sorted_arg]
      ind <- which(sorted_arg == names(p$mapping))
      p$mapping[[ind]] <- p$mapping[[1]]
      p$mapping[[1]] <- arg[[1]]
      names(p$mapping)[ind] <- names(p$mapping)[[1]]
      names(p$mapping)[1] <- sorted_arg
    }
  }
  res <- sprintf('ggplot(%s, aes(%s))', p$rappy$dataset_name, clist(p$mapping))
  
  for (layer in p$layers) {
    if (any(class(layer$geom) == 'Geom')) {
      geom_name <- snakeize(class(layer$geom)[[1]])
      geom_params <- paste(parse_kv(snakeize(class(layer$stat)[[1]])), 
                           parse_kv(snakeize(class(layer$position)[[1]])), sep=', ')
      if (!is.null(layer$aes_params) && length(layer$aes_params)) {
        geom_params <- paste(geom_params, clist(layer$aes_params), sep=', ')
      }
      if (!is.null(layer$mapping) && length(layer$mapping)) {
        aes_mapping_mask <- sapply(layer$mapping, function(x) !is.null(x))
        if (any(aes_mapping_mask)) {
          geom_params <- sprintf('aes(%s), %s', clist(layer$mapping[aes_mapping_mask]), geom_params)
        }
        # "ggplot(dataset, aes(x=carat, y=price, colour=clarity)) + geom_point(aes(character(0)), stat=\"identity\", position=\"jitter\", alpha=0.5, size=3) + theme_grey() + theme(text=element_text(character(0), character(0), color=\"black\", size=15, hjust=0.5, vjust=0.5)) + xlab(\"carat\") + ylab(\"price\") + facet_null()"
        
      }
      # for (params in c(layer$stat_params, layer$geom_params)) {
      # collision na.rm=F for both stat_params and geom_params
      param_mask <- sapply(layer$stat_params, function(x)
        !is.null(x) && (!is.logical(x) || x) && (!is.numeric(x) || x > 0) &&
          ((!is.vector(x) && !is.list(x)) || length(x) > 0))
      if (any(param_mask)) {
        useful_params <- layer$stat_params[param_mask]
        geom_params <- paste(geom_params, clist(useful_params), sep=', ')
        if ('binwidth' %in% names(useful_params) && geom_name == 'geom_bar') {
          geom_name <- 'geom_histogram'
          # otherwise, Warning message:
          # `geom_bar()` no longer has a `binwidth` parameter. Please use `geom_histogram()` instead.
        }
      }
      res <- sprintf('%s + %s(%s)', res, geom_name, geom_params)
    }
  }
  
  if ('guides' %in% names(p)) {
    guide_params <- c()
    for (guide_i in 1:length(p$guides)) {
      guide_name <- names(p$guides)[guide_i]
      guide <- p$guides[[guide_i]]
      if (guide_name != guide$title) {
        guide_params <- c(guide_params, sprintf('%s=guide_legend(title="%s")', guide_name, guide$title))
      }
    }
    if (length(guide_params)) {
      res <- sprintf('%s + guides(%s)', res, paste(guide_params, collapse=', '))
    }
  }
  
  for (scale in p$scales$scales) {
    if (scale$aesthetics == 'size') {
      # I have found that range information stores as an inner function
      # print(scale_size) -> continuous_scale(.., palette=area_pal(range)) -> ggproto(palette=palette)
      # so we can restore range's low and high by palette() out solving equation: 
      # sqrt(in)*(high-low)+low=out
      low <- p$scales$scales[[1]]$palette(0)
      high <- p$scales$scales[[1]]$palette(1)
      res <- sprintf('%s + scale_size(range=c(%s, %s))', res, low, high)
    } else if (scale$aesthetics == 'colour') {
      res <- sprintf('%s + scale_color_%s()', res, scale$scale_name)
    }
  }
  
  if (class(p$coordinates)[1] == 'CoordFlip') {
    res <- sprintf('%s + %s()', res, snakeize(class(p$coordinates)[1]))
  }
  
  if (!is.null(p$rappy$theme_name)) {
    res <- sprintf('%s + theme_%s()', res, p$rappy$theme_name)
  }
  if (!is.null(p$rappy$theme_attrs)) {
    res <- sprintf('%s + theme(text=element_text(%s))', res, clist(p$rappy$theme_attrs))
  }
  
  for (lab in c('title', 'x', 'y')) {
    if (!is.null(p$labels[[lab]])) {
      res <- sprintf('%s + %s("%s")', res, 
                     if (lab == 'title') 'ggtitle' else paste0(lab, 'lab'), 
                     p$labels[[lab]])
    }
  }
  
  if (all(class(p$facet) != 'null')) {
    res <- sprintf('%s + %s', res, gsub(',', ' +', format(p$facet)))
    free_mask <- unlist(p$facet$free)
    if (any(free_mask)) {
      res <- sub(')$', sprintf(
        ', scales="free%s")', if (all(free_mask)) '' else if (free_mask['x']) '_x' else '_y'), res)
    }
  }
  
  res
}

