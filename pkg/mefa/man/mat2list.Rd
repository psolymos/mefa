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
\references{
\enc{S\'{o}lymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'{o}lymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'{e}ter S\'{o}lymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
x <- matrix(1:12, 3, 4)
rownames(x) <- letters[1:3]
colnames(x) <- LETTERS[1:4]
mat2list(x, 1)
mat2list(x, 2)
}
\keyword{ manip }

