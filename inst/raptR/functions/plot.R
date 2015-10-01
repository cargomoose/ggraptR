## function for line plot
plotLine <- function(dataset, ls) {
  
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y))
  
  if (is.null(ls$color)) {
    p <- p + geom_line(aes(group=1), alpha=ls$alpha)
  } else if (x==ls$color) {
    p <- p + geom_line(aes(group=1), alpha=ls$alpha)
  } else {
    p <- p + geom_line(aes_string(group=ls$color), alpha=ls$alpha)
  }

  if (!is.null(ls$color)) {
    p <- p + aes_string(color=ls$colorAsFactor) + 
      guides(color = guide_legend(title=ls$color))
  }
  
  return(p)
}


## function for scatter plot
plotScatter <- function(dataset, ls) {
  
  if (!is.null(ls$size)) {
    p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
      geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
                 alpha=ls$alpha, position=ls$jitter) + 
      scale_size(range = c(1, ls$sizeMag))
  } else {
    p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
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

  return(p)
}


## function for points overlay
plotPointsOverlay <- function(plot, ls) {
  
  ## 
  if (!is.null(ls$size)) {
    p <- plot + 
      geom_point(aes_string(shape=ls$shapeAsFactor, size=ls$size), 
                 alpha=ls$alpha, position=ls$jitter) + 
      scale_size(range = c(1, sizeMag))
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
  
  return(p)
}

## function for histogram
plotHistogram <- function(dataset, ls) {
  p <- ggplot(dataset, aes_string(x=ls$x)) + 
    geom_histogram(alpha=ls$alpha, position=ls$position, binwidth=ls$binWidth) + 
    aes_string(fill=ls$fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }

  return(p)
}


## function for density plot 
plotDensity <- function(dataset, ls) {

  p <- ggplot(dataset, aes_string(x=ls$x)) 
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
  return(p)
}


## function for bar plot
plotBar <- function(dataset, ls) {

  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_bar(stat='identity', position=ls$position, alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }

  return(p)
}

## function for box plot
plotBox <- function(dataset, ls) {
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) + 
    geom_boxplot(alpha=ls$alpha) + 
    aes_string(fill=ls$fillAsFactor)
  
  ## legend labeling for fill
  if (!is.null(ls$fill)) {
    p <- p + guides(fill = guide_legend(title=ls$fill))
  }

  return(p)
}


## function for path plot
plotPath <- function(dataset, ls) {
  p <- ggplot(dataset, aes_string(x=ls$x, y=ls$y)) +
    geom_path(alpha=ls$alpha)
  return(p)
}
