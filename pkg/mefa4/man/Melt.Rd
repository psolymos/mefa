\name{Melt}
\alias{Melt}
\title{
Melting Matrices
}
\description{
The function reverses the side effects of cross tabulation.
}
\usage{
Melt(x)
}
\arguments{
  \item{x}{
A matrix, or sparse matrix object, a list of sparse matrices with identical dimnames,
a 'mefa' or 'Mefa' object.
}
}
\value{
A data frame with columns corrsponding to \code{rows}, \code{cols},
possibly \code{segm} (names of the list if \code{x} was a list of
sparse matrices), and \code{value}. \code{value} is numeric, other
columns are factors.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{stack}} in \pkg{utils}, 
and \code{\link[mefa]{melt}} in \pkg{mefa} package.
}
\examples{
xx <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
xx
xx0 <- Xtab(count ~ sample + species, xx)
xx1 <- Xtab(count ~ sample + species + segment, xx)
(M1 <- Melt(xx0))
Melt(as.matrix(xx0))
(M2 <- Melt(xx1))
stopifnot(identical(Xtab(value ~ rows + cols, M1), xx0))
stopifnot(identical(Xtab(value ~ rows + cols + segm, M2), xx1))
}
\keyword{ manip }
