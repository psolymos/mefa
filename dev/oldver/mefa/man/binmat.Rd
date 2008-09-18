\name{binmat}
\alias{binmat}
\title{Make Presence/Absence Matrix}
\description{
This is a utility function to convert a matrix with abundance (or measurement) data into a binary (presence/absence) format.
}
\usage{
binmat(matrix)
}
\arguments{
  \item{matrix}{a matrix, containing data to convert.}
}
\value{
A result is a matrix with same dimensions and row/column names as the original.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\examples{
(x <- matrix(data = rpois(15, 1), nrow = 5, ncol = 3))
binmat(x)
}
\keyword{ manip }
\keyword{ utilities }
