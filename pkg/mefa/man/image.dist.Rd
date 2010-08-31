\name{image.dist}
\alias{image.dist}
\title{
Image Plot of a 'dist' Object
}
\description{
Image plot of a 'dist' object.
}
\usage{
image.dist(x, grad, labels = FALSE, ...)
}
\arguments{
  \item{x}{
An object of class 'dist', not a similarity matrix.
}
  \item{grad}{
Gradient to use for ordering the observations in \code{x}.
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
