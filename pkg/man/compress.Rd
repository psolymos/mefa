\name{compress.factor}
\alias{compress}
\alias{compress.default}
\alias{compress.factor}
\title{Compress a factor based on unique values in a vector}
\description{
The function compress a factor (or any vector) based on unique values in a vector, if the levels of the vector are nested in the levels of the factor.
}
\usage{
compress(x, \ldots)
\method{compress}{default}(x, y, \ldots)
\method{compress}{factor}(x, y, \ldots)
}
\arguments{
  \item{x, y}{factor or any vector.}
  \item{\ldots}{further arguments potentially passed (currently there are none).}
}
\details{
\code{x} and \code{y} should have same lengths, and \code{y} should be nested into \code{x} to fulfill that \code{length(unique(interaction(x, y))) == length(unique(y))}. The procedure is similar to \code{\link{aggregate}} in a sense that we wan to get a shorter vector and preserving values in a nested fashion. But \code{\link{aggregate}} can not be applied for factors or character vectors.
}
\value{
A factor with length as the number of discrete values in \code{y}. Values correspond to the values of \code{x} in unique values of \code{y}.}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{stcs}}}
\examples{
x<-as.factor(rep(letters[c(3,2,4,1)],each=4))
y <- rep(1:8, each=2)
x
y
compress(x,y)
z<-rep(c(3,2,4,1),each=4)
z
compress(z,y)
}
\keyword{ manip }
