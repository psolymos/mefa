\encoding{UTF-8}
\name{report.mcmc.list}
\Rdversion{1.1}
\alias{report.mcmc.list}
\title{
Calculations on mcmc.list objects
}
\description{
Conveniently calculates statistics for mcmc.list objects, or combining them.
}
\usage{
\method{report}{mcmc.list}(x, FUN, ...)
}
\arguments{
  \item{x}{
Objects of class \code{mcmc.list}.
}
  \item{FUN}{
A function to be used in the calculations.
}
  \item{\dots}{
Other arguments passed to \code{FUN}.
}
}
\value{
Returns statistic value based on \code{FUN}, using all values in all chains of the MCMC object.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
data(regmod)
report(regmod, mean)
se <- function(x) sd(x) / sqrt(niter(regmod))
report(regmod, se)
}
\keyword{ methods }
\keyword{ utilities }
