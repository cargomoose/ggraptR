#### functions ####
# eval(parse(text=txt))
# plt$layers[[1]]$geom %>% class
# plt$scales$scales[[1]]$scale_name
# plt$facet$cols %>% names

ggplot.rappy <- function(dataset, aes) {
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

wrap_quote <- function(el) if (is.character(el)) sprintf('"%s"', el) else el

generate_code <- function(ggrappy) {
  # geom_bar(stat="identity", position='identity', alpha=0.8)$print
  aes_args <- 1:length(ggrappy$mapping) %>% 
    sapply(function(i) paste(c(names(ggrappy$mapping)[i], ggrappy$mapping[[i]]), collapse = '=')) %>%
    paste(collapse=', ')
  res <- sprintf('ggplot(%s, aes(%s))',
                 ggrappy$dataset_name, aes_args)
  
  for (layer in plt$layers) {
    if (any(class(layer$geom) == 'Geom')) {
      geom_params <- paste(parse_kv(snakeize(class(layer$stat)[[1]])), 
                           parse_kv(snakeize(class(layer$position)[[1]])),
                           sapply(1:length(layer$aes_params), function(par_i) 
                             sprintf('%s=%s', names(layer$aes_params)[par_i], wrap_quote(layer$aes_params[par_i]))),
                           sep=', ')
      res <- sprintf('%s + %s(%s)', res, snakeize(class(layer$geom)[[1]]), geom_params)
    }
  }
  res
}


#### run ####
plt <- ggplot.rappy(mtcars, aes_string(x='factor(cyl)', y='mpg')) + 
  geom_bar(stat="identity", position='identity', alpha=0.8) + aes_string(fill='factor(gear)')
generate_code(plt)

