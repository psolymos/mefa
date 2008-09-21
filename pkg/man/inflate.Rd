\name{inflate}
\alias{inflate}
\title{Inflate a data frame based on count values in a vector}
\description{
The function inflates a data frame based on count values in a vector corresponding the rows.
}
\usage{
inflate(x, y)
}
\arguments{
  \item{x}{a matrix or data frame, but can be a vector.}
  \item{y}{a vector containing positive (non zero) real numbers.}
}
\details{
Each rows of the data frame \code{x} will be repeated as many times as the corresponding value in the vector \code{y}. This function is called internally by the function \code{\link{stcs}}.
}
\value{
A data frame with columns as in the original (\code{x}) and \code{sum(y)} number of rows.}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}, 
}
\seealso{\code{\link{stcs}}}
\examples{
x <- data.frame(sample = LETTERS[c(1,1,2,2,3)],
    species = letters[c(5,5,5,6,7)],
    count = c(1,2,10,3,4), segment = letters[c(8,9,8,9,8)])
x
inflate(x[,c(1,2,4)], x[,3])
}
\keyword{ manip }
