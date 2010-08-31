\name{image.dist}
\alias{image.dist}
\title{
Image Plot of a 'dist' Object
}
\description{
Image plot of a 'dist' object.
}
\usage{
\method{image}{dist}(x, grad, lab = TRUE, ...)
}
\arguments{
  \item{x}{
An object of class 'dist', not a similarity matrix.
}
  \item{grad}{
Gradient to use for ordering the observations in \code{x}.
}
  \item{lab}{
Labels to use at the left axis, can be a character vector.
}
  \item{\dots}{
Other graphical parameters passed to \code{\link{image}}.
}
}
\value{
Creates an image plot as a side effect.

Invisibly returns plotted values.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\examples{
data(dol.count, dol.samp)
m <- mefa(stcs(dol.count), dol.samp)
d <- dist(m$xtab)
g <- with(m$samp, interaction(microhab, method))
image(d)
}
\keyword{ hplot }
\keyword{ ~kwd2 }
