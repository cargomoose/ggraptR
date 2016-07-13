library(dplyr)
library(ggplot2)
library(ggraptR)


ggraptR()


plt <- ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(gear))) + 
  geom_bar(stat="identity") + 
  facet_grid(~gear) + 
  scale_fill_brewer(palette="Dark2")
plt

eval(parse(text=sprintf('ggplot(%s, aes(%s)) + geom_bar(stat="identity")',
    'mtcars',
    sapply(1:length(plt$mapping), function(i) paste(c(names(plt$mapping)[i], plt$mapping[[i]]), collapse = '=')) %>%
  paste(collapse=', '))))

plt$layers[[1]]$geom %>% class
plt$scales$scales[[1]]$scale_name
plt$facet$cols %>% names

# names(plt), str(plt)
x <- ggplot_build(plt)
str(x)



