\name{ivgen}
\alias{ivgen}
\title{Writes Output Files for IndVal}
\description{
Writes output files in the format suitable for analysis in the 'IndVal.exe' software by Dufrene and Legendre (1997).
}
\usage{
ivgen(x, cl, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.}
  \item{cl}{a tree object of class \code{\link{hclust}}.}
  \item{k}{an integer scalar defining the desired number of groups (partitions), used by \code{\link{cutree}}.}
}
\value{
Three text files produced into the working directory.
}
\references{
Dufrene, M. and Legendre, P. 1997. Species assemblages and indicator species: the need for a flexible asymmetrical approach. Ecological Monographs 67, 345--366.

IndVal homepage at \url{http://biodiversite.wallonie.be/outils/indval/home.html}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{cutree}}, \code{\link{dist}}, \code{\link{hclust}}
}
\examples{
x <- matrix(rpois(24,1),6,4)
cl <- hclust(dist(x))
plot(cl)
ivgen(x, cl, 3)
}
\keyword{ manip }
