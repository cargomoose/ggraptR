# "Generate Code" button. Reset inputs, Export Plot


#### functions ####
ggraptR <- function(defaultDataFrame="") {
  print('source version')
  gDefaultDataFrame <<- ""
  shiny::runApp('D:/dev/projects/R/raptR/inst/ggraptR', display.mode="showcase")
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
# p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) +
#   geom_point(aes_string(color='factor(cyl)', size='disp', shape='factor(am)'), alpha=0.5) +
#   stat_smooth(method='lm') +
#   scale_size(range = c(0.5, 8))

# theme_name='fivethirtyeight'
# p <- ggplot(mtcars, aes_string(x='mpg', y='wt')) + geom_point() +
#   facet_grid(vs + am ~ gear, scales="free_x") + 
#   do.call(sprintf('theme_%s', theme_name), list()) + ggtitle('titit') + xlab('xxlb') + ylab('yylb')
# p$rappy$theme_name <- theme_name

theme_attrs=list(family='TT Arial', face = 'bold', color = 'red', size = 12, hjust = 0.8, vjust = 0.3)
p <- ggplot(mtcars, aes_string(x='mpg', y='wt')) + geom_point() +
  facet_grid(facets=am ~ gear, scales="free_x") +
  scale_color_hc() +
  theme(text=do.call(element_text, list(theme_attrs)))
p$rappy$theme_attrs <- theme_attrs
  
p$rappy$dataset_name <- 'mtcars'
print(generate_code(p, input))
eval(parse(text=generate_code(p, input)))  # robust test


