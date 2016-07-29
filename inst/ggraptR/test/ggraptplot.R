#### functions ####
# ggraptR()

library(ggplot2)
library(dplyr)
library(stringr)
library(ggraptR)


ggraptplot <- function(dataset, aes) {
  ggplot_obj <- ggplot(dataset, aes)
  class(ggplot_obj) <- append(class(ggplot_obj), "rappy")
  ggplot_obj$dataset_name <- deparse(substitute(dataset))
  return(ggplot_obj)
}

`+` <- function(rappy, e2) {
  # print(any(class(e2) == 'Layer'))
  # if (any(class(e2$geom) == 'GeomBar')) {
    # rappy$geom <- e2$geom
  # }
  ggplot2::`%+%`(rappy, e2)
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
  paste(sapply(1:length(arg_lst), function(par_i) 
    sprintf('%s=%s', names(arg_lst)[par_i], wrap_quote(arg_lst[[par_i]]))), 
    collapse=', ')
}

generate_code <- function(ggrappy) {
  # p$facet$cols %>% names
  # geom_bar(stat="identity", position='identity', alpha=0.8)$print
  
  # reorder inner aes arguments
  for (sorted_arg in c('y', 'x')) {
    if (names(ggrappy$mapping)[1] != sorted_arg && sorted_arg %in% names(ggrappy$mapping)) {
      arg <- ggrappy$mapping[sorted_arg]
      ind <- which(sorted_arg == names(ggrappy$mapping))
      ggrappy$mapping[[ind]] <- ggrappy$mapping[[1]]
      ggrappy$mapping[[1]] <- arg[[1]]
      names(ggrappy$mapping)[ind] <- names(ggrappy$mapping)[[1]]
      names(ggrappy$mapping)[1] <- sorted_arg
    }
  }
  res <- sprintf('ggplot(%s, aes(%s))', ggrappy$dataset_name, clist(ggrappy$mapping))
  
  for (layer in ggrappy$layers) {
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
      if (sum(param_mask)) {
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
  
  if ('guides' %in% names(ggrappy)) {
    guide_params <- c()
    for (guide_i in 1:length(ggrappy$guides)) {
      guide_name <- names(ggrappy$guides)[guide_i]
      guide <- ggrappy$guides[[guide_i]]
      if (guide_name != guide$title) {
        guide_params <- c(guide_params, sprintf('%s=guide_legend(title="%s")', guide_name, guide$title))
      }
    }
    if (length(guide_params)) {
      res <- sprintf('%s + guides(%s)', res, paste(guide_params, collapse=', '))
    }
  }
  if (length(ggrappy$scales$scales)) {
    # I have found that range information stores inside an inner function
    # print(scale_size) -> continuous_scale(.., palette=area_pal(range)) -> ggproto(palette=palette)
    # so we can restore range's low and high by palette out solving equation: sqrt(in)*(high-low)+low=out
    low <- ggrappy$scales$scales[[1]]$palette(0)
    high <- ggrappy$scales$scales[[1]]$palette(1)
    res <- sprintf('%s + scale_size(range=c(%s, %s))', res, low, high)
  }
  res
}


#### run ####
# p <- ggraptplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) +
#   geom_bar(stat="identity", position='identity', alpha=0.8) + aes_string(fill='factor(gear)')
# p <- ggraptplot(mtcars, aes_string(x='mpg')) +
#   geom_histogram(alpha=0.5, position='identity', binwidth=5) + aes_string(fill='factor(gear)')
# p <- ggraptplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) + geom_boxplot(alpha=0.5)
# p <- ggraptplot(mtcars, aes_string(x='hp', y='mpg')) + geom_line(aes(group=1), alpha=0.5)
# p <- ggraptplot(mtcars, aes_string(x='hp', y='mpg')) + geom_path(alpha=0.5)
# p <- ggraptplot(mtcars, aes_string(x='mpg')) + 
#   geom_density(aes_string(color='factor(am)', fill='factor(cyl)'), alpha=0.5) +
#   guides(color = guide_legend(title='col'), fill=guide_legend(title='fil'))
p <- ggraptplot(mtcars, aes_string(x='hp', y='mpg')) +
  geom_point(aes_string(color='factor(cyl)', size='disp', shape='factor(am)'), alpha=0.5) +
  stat_smooth(method='lm') +
  scale_size(range = c(0.5, 8))

print(generate_code(p))
eval(parse(text=generate_code(p)))  # robust test
