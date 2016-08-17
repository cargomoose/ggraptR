rm(list=ls())
shiny::runApp('C:/GoogleDrive/dev/R/raptR/inst/ggraptR')
ggraptR::ggraptR()

library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)


#### test cases ####
p <- ggplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) +
  geom_bar(stat="identity", position='identity', alpha=0.8) + aes_string(fill='factor(gear)')
p <- ggplot(mtcars, aes_string(x='mpg')) +
  geom_histogram(binwidth=5) + coord_flip()
p <- ggplot(mtcars, aes_string(x='factor(cyl)', y='mpg')) +
  geom_boxplot(alpha=0.5)
p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) + geom_line(aes(group=1), alpha=0.5)
p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) + geom_path(alpha=0.5)
p <- ggplot(mtcars, aes_string(x='mpg')) +
  geom_density(aes_string(color='factor(am)', fill='factor(cyl)'), alpha=0.5) +
  guides(color = guide_legend(title='col'), fill=guide_legend(title='fil'))
p <- ggplot(mtcars, aes_string(x='hp', y='mpg')) +
  geom_point(aes_string(color='factor(cyl)', size='disp', shape='factor(am)'), alpha=0.5) +
  stat_smooth(method='lm') +
  scale_size(range = c(0.5, 8))

theme_name='fivethirtyeight'
p <- ggplot(mtcars, aes_string(x='mpg', y='wt')) + geom_point() +
  facet_grid(vs + am ~ gear, scales="free_x") +
  do.call(sprintf('theme_%s', theme_name), list()) + ggtitle('titit') + xlab('xxlb') + ylab('yylb')
p$rappy$theme_name <- theme_name

theme_attrs=list(family='TT Arial', face = 'bold', color = 'red', size = 12, hjust = 0.8, vjust = 0.3)
p <- ggplot(mtcars, aes_string(x='mpg', y='wt')) + geom_point() +
  facet_grid(facets=am ~ gear, scales="free_x") +
  scale_color_hc() +
  theme(text=do.call(element_text, list(theme_attrs)))
p$rappy$theme_attrs <- theme_attrs


#### check ####
p$rappy$dataset_name <- 'mtcars'
print(generate_code(p, input))
eval(parse(text=generate_code(p, input)))  # robust


