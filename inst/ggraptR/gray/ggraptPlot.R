#### functions ####
ggplot.rappy <- function(dataset, x) {
  ggplot_obj <- ggplot(dataset, aes_string(x))
  ggplot_obj$state <- list(cur_plot='undef')
  class(ggplot_obj) <- append(class(ggplot_obj), "rappy")
  return(ggplot_obj)
}

geom_bar <- function(self) {
  print('generic')
  UseMethod("geom_bar")
}

geom_bar.rappy <- function(self) {
  print('rappy bar')
  self$state$cur_plot <- 'bar'  # does not work yet. I need to read deeper about environment and argument passing by val.
  # but I'm not sure I should spend time this way
  ggplot2::geom_bar()  # we must use ggplot2:: because geom_bar does not really have Generic function
}


#### run ####
p <- ggplot.rappy(mpg, 'class')
print(p$state)
# > [1] "undef"

p <- p %+% geom_bar(p)
# > [1] "generic"
# > [1] "rappy bar"

print(p$state)
# > [1] "undef"

print(p)  # plots correctly


# ggplot(mpg, aes(class)) + geom_bar()
# ggplot(mtcars, aes(wt, mpg)) + geom_point()


