\name{marmat}
\alias{marmat}
\title{Computes Marginal Sums of Presence Absence or Abundance Matrices}
\description{
This is a utility function to calculate statistics on data matrices in objects of class 'xcount' and 'mefa'.
}
\usage{
marmat(x, which = c("samples", "species"), method = c("abund", "occur"), group = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix containing data.}
  \item{method}{sets method to use, \code{"abund"} refers to abundance data (use data in \code{x} as is), 
    \code{"occur"} (occurence) refers to column/row sums in a presence/absence (binary) matrix.}
  \item{which}{\code{"samples"} indicates rows (default), \code{"species"} indicates columns,
    on which the function will be applied over.}
  \item{group}{\code{NULL} (default) or a vector with length sames as the number of rows 
    (for \code{which="samples"}), or columns (for \code{which="samples"}). I \code{NULL} it takes the value 
    \code{c(1:nrow(x))} or \code{c(1:ncol(x))} depending on value of \code{which}.
    It is treated as factor, to \code{\link{aggregate}} values of matrix \code{x}.}
}
\details{
This function can be used on eg. data tables of count or abundance matrices to calculate number of individuals (or eg. total biomass in case of measurements, \code{method="abund"}) or species richness (\code{method="occur"}) within samples (\code{which="samples"}) or groups of samples (\code{which="samples"}, \code{group} differs from the default), or abundance (\code{method="abund"}) or number of occurences (\code{method="occur"}) of species (\code{which="species"}) or species groups (\code{which="species"}, \code{group} differs from the default).
}
\value{
A result is a vector.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\examples{
x <- matrix(data = rpois(8*4, 1), nrow = 8, ncol = 4)
rownames(x) <- letters[1:8]
colnames(x) <- LETTERS[1:4]

marmat(x, "samples", "abund")
marmat(x, "samples", "abund", group=c(1,1,2,2,3,3,4,4))
marmat(x, "samples", "occur")
marmat(x, "samples", "occur", group=c(1,1,2,2,3,3,4,4))

marmat(x, "species", "abund")
marmat(x, "species", "abund", group=c(1,1,2,2))
marmat(x, "species", "occur")
marmat(x, "species", "occur", group=c(1,1,2,2))
}
\keyword{ manip }
\keyword{ utilities }
