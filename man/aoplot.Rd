\name{aoplot}
\alias{aoplot}
\alias{aoplot.default}
\title{
Abundance-Occupancy Plot
}
\description{
Abundance-occupancy plot.
}
\usage{
aoplot(x, ...)
\method{aoplot}{default}(x, log = TRUE, xlab, ylab, ...)
}
\arguments{
  \item{x}{
An object, possibly a matrix.
}
  \item{log}{
Logical, if abundance values should be log10 transformed.
}
  \item{xlab, ylab}{
Axis labels, can be missing for default labels.
}
  \item{\dots}{
Other arguments passed to \code{\link{plot}} function.
}
}
\details{
Column sums of \code{x} (species abundances) are plotted against 
column sums of the binarized \code{x} (species occupancy).
}
\value{
Creates a plot as a side effect.

Invisibly returns abundance and occurrence values in a matrix.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\examples{
data(dol.count)
x <- mefa(stcs(dol.count))
aoplot(x)
}
\keyword{ hplot }
\keyword{ methods }
