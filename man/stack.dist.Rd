\encoding{UTF-8}
\name{as.data.frame.dist}
\Rdversion{1.1}
\alias{vec2dist}
\alias{is.dist}
\alias{stack.dist}
\title{
Manipulation Tools for Class 'dist'
}
\description{
Converts a 'dist' object into vector and vice versa.
}
\usage{
vec2dist(x, size, labels = NULL, diag = FALSE, upper = FALSE, 
call = FALSE, method = NULL)
is.dist(x)
\method{stack}{dist}(x, dim.names = FALSE, ...)
}
\arguments{
  \item{x}{
A vector or a 'dist' object.
}
  \item{size}{
Integer, the number of observations in the dataset (see \code{\link{dist}}).
}
  \item{labels, diag, upper, call, method}{
Logical, setting attributes of the distance object (see \code{\link{dist}}).
}
  \item{dim.names}{
Logical, if \code{TRUE}, names stored in the \code{Labels} attribute of the 
\code{dist} object are returned as factors instead of row/column indices.
}
  \item{\dots}{
Other arguments passed to functions.
}
}
\value{
\code{vec2dist} returns an object of class \code{dist}.

\code{is.dist} returns a logical statement about the class of the object.

\code{stack.dist} returns a data frame with comuns \code{row} for row id's, \code{col} for column
id's and \code{dist} for the distance values.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \doi{10.18637/jss.v029.i08}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
vec2dist(1:3, 3)
vec2dist(1:3, 3, diag = TRUE)
vec2dist(1:3, 3, diag = TRUE, upper = TRUE)
vec2dist(1:3, 3, labels=letters[1:3])
x <- dist(1:5)
attr(x, "Labels") <- LETTERS[1:5]
x
is.dist(x)
stack(x)
stack(x, dim.names = TRUE)
}
\keyword{ manip }

