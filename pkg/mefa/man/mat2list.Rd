\encoding{UTF-8}
\name{mat2list}
\Rdversion{1.1}
\alias{mat2list}
\alias{mfill}
\alias{rdrop}
\alias{cdrop}
\title{
Matrix Manipulation Tools
}
\description{
Converts a matrix to a list, calculates matrix fill, or exclude rows/columns with certain rox/column sums.
}
\usage{
mat2list(x, MARGIN = 1)
mfill(x)
rdrop(x, cutoff = 0)
cdrop(x, cutoff = 0)
}
\arguments{
  \item{x}{
A matrix to be converted.
}
  \item{MARGIN}{
Margin in \code{c(1, 2)}, indicating respectively rows or colums to be listed.
}
  \item{cutoff}{
Numeric, exclude rows/columns with sums <= \code{cutoff}.
}
}
\value{
\code{mat2list} returns a list of length \code{nrow(x)} or \code{ncol(x)} depending on \code{MARGIN}.
Elements are rows/columns of \code{x}. Dimnames are retained as names.

\code{mfill} returns a single number with the matrix fill value.

\code{rdrop} and \code{cdrop} return the original matrix but excluding rows (\code{rdrop}), or columns (\code{cdrop})
that has sum <= \code{cutoff}. Excluded row/colum numbers and specifications are stored in the \code{"exclude"} attribute.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
x <- matrix(1:12, 3, 4)
rownames(x) <- letters[1:3]
colnames(x) <- LETTERS[1:4]
mat2list(x, 1)
mat2list(x, 2)
}
\keyword{ manip }

