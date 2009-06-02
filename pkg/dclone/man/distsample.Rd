\encoding{UTF-8}
\name{distsample}
\Rdversion{1.1}
\alias{distsample}
\title{
Sample Spatial Point Patterns
}
\description{
Takes a random sample of point pairs.
}
\usage{
distsample(xy, probs = seq(0, 1, 0.2), size = round(nrow(xy) / 10))
}
\arguments{
  \item{xy}{
A two column matrix with coordinates.
}
  \item{probs}{
Probabilities to determine quantiles of Euclidean distances among points of \code{xy}.
(Spatial lag expressed in percentiles of the distances.)
}
  \item{size}{
Number of random point pairs to be chosen within each quantile intervals of the 
Euclidean distances.
}
}
\value{
A data frame with three columns: IDs for point pairs in \code{xy} and their Euclidean distances.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
xy <- cbind(runif(100), runif(100))
ds <- distsample(xy, probs = c(0, 0.05, 0.1, 0.25, 0.5, 1), size=5)
ds
opar <- par(mfrow=c(1,2))
plot(xy)
for (i in 1:nrow(ds))
    lines(xy[as.matrix(ds)[i,1:2],], col=2)
d <- dist(xy)
attributes(d) <- NULL
d <- d[order(d)]
plot(d, d)
points(ds$dist, ds$dist, pch=19, col=2)
par(opar)
}
\keyword{ manip }

