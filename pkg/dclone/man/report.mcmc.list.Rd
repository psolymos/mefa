\name{report.mcmc.list}
\Rdversion{1.1}
\alias{report.mcmc.list}
\alias{combine}
\alias{combine.mcmc.list}
\title{
Calculations on mcmc.list objects
}
\description{
Conveniently calculates statistics for mcmc.list objects, or combining them.
}
\usage{
\method{report}{mcmc.list}(x, FUN, ...)
combine(x, y, ...)
\method{combine}{mcmc.list}(x, y, FUN, var.names=varnames(x), ...)
}
\arguments{
  \item{x, y}{
Objects of class \code{mcmc.list}.
}
  \item{FUN}{
A function to be used in the calculations.
}
  \item{var.names}{
Variable names to use in the returned object. If \code{NULL}, then the variable names in \code{x} are used.
}
  \item{\dots}{
Other arguments passed to \code{FUN}.
}
}
\value{
\code{report} returns statistic value based on \code{FUN}.

\code{combine} returns an \code{mcmc.list} object with values according to \code{FUN} (applied to each values separately).
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\examples{
data(regmod)
report(regmod, mean)
se <- function(x) sd(x) / niter(regmod)
report(regmod, se)
x <- window(regmod, 4000, 5000)
mod <- combine(x, x, "+")
summary(mod)
}
\keyword{ methods }
\keyword{ utilities }
