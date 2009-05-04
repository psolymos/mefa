\encoding{UTF-8}
\name{mat2list}
\Rdversion{1.1}
\alias{mat2list}
\title{
Converts a Matrix to a List.
}
\description{
Converts a matrix to a list.
}
\usage{
mat2list(x, MARGIN = 1)
}
\arguments{
  \item{x}{
A matrix to be converted.
}
  \item{MARGIN}{
Margin in \code{c(1, 2)}, indicating respectively rows or colums to be listed.
}
}
\value{
A list of length \code{nrow(x)} or \code{ncol(x)} depending on \code{MARGIN}.
Elements are rows/columns of \code{x}. Dimnames are retained as names.
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

