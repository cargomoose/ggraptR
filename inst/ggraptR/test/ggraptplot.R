# "Generate Code" button
# refactoring


#### functions ####
ggraptR <- function(defaultDataFrame="") {
  print('source version')
  gDefaultDataFrame <<- ""
  shiny::runApp('D:/dev/projects/R/raptR/inst/ggraptR', display.mode="normal")
  rm(list=ls())
}
ggraptR::ggraptR()

library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)

# add_ggplot_attribute <- function(p, attr_name, attr_val) {
#   if (!any(class(p) == 'rappy')) {  # we need to mark this object as changed ggplot
#     class(p) <- append(class(p), "rappy")
#   }
#   p[[attr_name]] <- attr_val
# }
# ggraptplot <- function(dataset, x, y=NULL) {  # we need to distinguish classes
#   p <- ggplot(dataset, do.call(aes_string, c(list(x), if (!is.null(y)) list(y))))
#   class(p) <- append(class(p), "rappy")
#   p$dataset_name <- deparse(substitute(dataset))
#   return(p)
# }
# `+` <- function(rappy, e2) {
#   # print(any(class(e2) == 'Layer'))
#   # if (any(class(e2$geom) == 'GeomBar')) {
#     # rappy$geom <- e2$geom
#   # }
#   ggplot2::`%+%`(rappy, e2)
# }

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
  paste(sapply(1:length(arg_lst), function(par_i) 
    sprintf('%s=%s', names(arg_lst)[par_i], wrap_quote(arg_lst[[par_i]]))), 
    collapse=', ')
}

# input argument is for $dataset, $plotTheme
generate_code <- function(p, input) {
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
  res <- sprintf('ggplot(%s, aes(%s))', input$dataset, clist(p$mapping))
  
  for (layer in p$layers) {
    if (any(class(layer$geom) == 'Geom')) {
      geom_name <- snakeize(class(layer$geom)[[1]])
      geom_params <- paste(parse_kv(snakeize(class(layer$stat)[[1]])), 
                           parse_kv(snakeize(class(layer$position)[[1]])), sep=', ')
      if (!is.null(layer$aes_params) && length(layer$aes_params)) {
        geom_params <- paste(geom_params, clist(layer$aes_params), sep=', ')
      }
      if (!is.null(layer$mapping) && length(layer$mapping)) {
        geom_params <- sprintf('aes(%s), %s', clist(layer$mapping), geom_params)
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
  
  if (!is.null(input$plotTheme)) {
    res <- sprintf('%s + theme_%s()', res, input$plotTheme)
  }
  if (!is.null(input$themeAttrs)) {
    res <- sprintf('%s + theme(text=element_text(%s))', res, clist(input$themeAttrs))
  }
  
  for (lab in c('title', 'x', 'y')) {
    if (!is.null(p$labels[[lab]])) {
      res <- sprintf('%s + %s("%s")', res, 
                     if (lab == 'title') 'ggtitle' else paste0(lab, 'lab'), 
                     p$labels[[lab]])
    }
  }
  
  if (!is.null(p$facet)) {
    res <- sprintf('%s + %s', res, gsub(',', ' +', format(p$facet)))
    free_mask <- unlist(p$facet$free)
    if (any(free_mask)) {
      res <- sub(')$', sprintf(
        ', scales="free%s")', if (all(free_mask)) '' else if (free_mask['x']) '_x' else '_y'), res)
    }
  }
  
  res
}


#### run ####
# p <- ggplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) +
#   geom_bar(stat="identity", position='identity', alpha=0.8) + aes_string(fill='factor(gear)')
# p <- ggplot(mtcars, aes_string(x='mpg')) +
#   geom_histogram(binwidth=5) + coord_flip()
# p <- ggplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) +
#   geom_boxplot(alpha=0.5)
# p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) + geom_line(aes(group=1), alpha=0.5)
# p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) + geom_path(alpha=0.5)
# p <- ggplot(mtcars, aes_string(x='mpg')) +
#   geom_density(aes_string(color='factor(am)', fill='factor(cyl)'), alpha=0.5) +
#   guides(color = guide_legend(title='col'), fill=guide_legend(title='fil'))
# p <- ggplot(get(input$dataset), aes_string(x='hp', y='mpg')) +
#   geom_point(aes_string(color='factor(cyl)', size='disp', shape='factor(am)'), alpha=0.5) +
#   stat_smooth(method='lm') +
#   scale_size(range = c(0.5, 8))

# input <- list(dataset='mtcars', plotTheme='fivethirtyeight')  # p$rappy$theme_name
# p <- ggplot(get(input$dataset), aes_string(x='mpg', y='wt')) + geom_point() +
#   facet_grid(vs + am ~ gear, scales="free_x") + 
#   theme_fivethirtyeight() + ggtitle('titit') + xlab('xxlb') + ylab('yylb')

input <- list(dataset='mtcars',
              themeAttrs=list(face = 'bold', color = 'red', size = 12, hjust = 0.8, vjust = 0.3))
p <- ggplot(get(input$dataset), aes_string(x='mpg', y='wt')) + geom_point() +
  facet_grid(facets=am ~ gear, scales="free_x") +
  scale_color_hc() +
  theme(text=element_text(input$themeAttrs))

print(generate_code(p, input))
eval(parse(text=generate_code(p, input)))  # robust test

