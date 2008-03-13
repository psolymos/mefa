\name{qdef}
\alias{qdef}
\title{Numeric vector conversion based on quantiles}
\description{
Converts a numeric vector into an ordinal one based on quantiles.
}
\usage{
qdef(x, probs = seq(0, 1, 0.25))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.}
  \item{probs}{numeric vector of probabilities with values in code{[0,1]} used by \code{\link{quantile}}.}
}
\value{
A vector of length the same as the input vector, containing integers from \code{1} to \code{length(probs)-1}.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{quantile}}
}
\examples{
(vec <- rnorm(20))
(qd <- qdef(vec))
plot(vec, qd, type="h")
}
\keyword{ manip }

