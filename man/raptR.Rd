\name{raptR}
\alias{raptR}
\title{Launch the application on your default web browser}
\description{

  raptR is the command that launches a Browser based User Interface that gives 
  access to the package functionality for data exploration via many different types of 
  plots that the user will be able   to customize. You can expect to have most of the 
  flexibility existing on the ggplot2 package and being able to benefit from it without 
  having to write a single line of code.
  
}
\usage{
raptR()
}
\details{

 Currently some of the graphs that can be generated include:
 scatterplot, lineplot, barplot, histogram, densityplot, boxplot, pathplot
 
 Also, the package allows to generate tables and perform aggregate operations specifying 
 the by and target columns and the operation to be applied (min, max, count, ...).
 
 The user can import it's own data files, specifiying the header, separator and quote options.

}
\references{
A reference
}
\author{ Eugene Dubossarsky \\ eugene@presciient.com }

\examples{
  \dontrun{
    raptR()
  }
}
\keyword{ ~ggplot2 }
\keyword{ ~user interface }
\keyword{ ~web based }
\keyword{ ~shiny }
