\encoding{UTF-8}
\name{rep.data.frame}
\alias{rep.data.frame}
\title{Inflate (repeat) a Data Frame}
\description{
The function inflates a data frame based on count values in a vector corresponding the rows.
}
\usage{
\method{rep}{data.frame}(x, ...)
}
\arguments{
  \item{x}{a matrix or data frame, but can be a vector.}
  \item{\dots}{arguments passed to the function \code{\link{rep}}, i.e. \code{times},
    \code{length.out} and \code{each} (see explanation there).}
}
\details{
Each rows of the data frame \code{x} will be repeated as many times as the corresponding value in e.g. the vector \code{times}. This function is called internally by the function \code{\link{stcs}}.
}
\value{
A data frame with columns as in the original (\code{x}) and e.g. \code{sum(times)} number of rows.}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{stcs}}}
\examples{
x <- data.frame(sample = LETTERS[c(1,1,2,2,3)],
    species = letters[c(5,5,5,6,7)],
    count = c(1,2,10,3,4), segment = letters[c(8,9,8,9,8)])
x
rep(x[,c(1,2,4)], times = x[,3])
rep(x[,c(1,2,4)], each = 2)
}
\keyword{ manip }
