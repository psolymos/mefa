\name{mapmf}
\alias{mapmf}
\title{Plots Spatial Representation of a Numeric Vector}
\description{
Plots spatial representation of a numeric vector based on \code{xy} coordinates.
}
\usage{
mapmf(xy, z, probs = seq(0, 1, 0.25), type = "heat", pch = 21, cex = 1.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{a matrix with 2 columns}
  \item{z}{a numeric vector to be plotted according the \code{xy}.}
  \item{probs}{\code{probs} argument used by \code{\link{qdef}}, numeric vector of probabilities with values in \code{[0,1]}.}
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{pch}{default for \code{pch} used in the plot (see \code{\link{par}}).}
  \item{cex}{default for \code{cex} used in the plot (see \code{\link{par}}).}
  \item{\dots}{Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).}
}
\note{This function will be replaced soon.}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{qdef}}
  }
\examples{
x <- rep(1:10,10)
y <- rep(1:10, each=10)
z <- rpois(100,10)
mapmf(cbind(x,y),z,type="grey")
mapmf(cbind(x,y),z,NULL)
}
\keyword{ hplot }
