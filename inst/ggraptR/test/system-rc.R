# http://adv-r.had.co.nz/OO-essentials.html#picking-a-system
# http://www.cyclismo.org/tutorial/R/s3Classes.html
# http://www.agapow.net/programming/r/reference-classes/


Rappy.ggplot <- setRefClass("Rappy.ggplot",
  fields = c('gg'),  # dataset, x
  methods = list(
   
   # initialize <- function(dataset, x) {  # does not work yet
   #   gg <<- ggplot(dataset, aes(x=x))
   #   dataset <<- NULL
   #   x <<- NULL
   # },
   
   `+.Rappy.ggplot` <- function(self, gg2) {  # does not work yet
   # e2name <- deparse(substitute(e2$gg))
   # if (is.theme(e1))
   #   add_theme(e1, e2, e2name)
   # else if (is.ggplot(e1)) 
     # add_ggplot(e1$gg, e2$gg, e2name)
     ggplot2::`%+%`(self$gg, gg2)
   }
   
  )
)

# ggrappy <- Rappy.ggplot$new(dataset, x)
ggrappy <- Rappy.ggplot$new(gg=ggplot(mpg, aes(x=class)))
ggrappy$gg + geom_bar()
# ggrappy + geom_bar()  # does not work yet


