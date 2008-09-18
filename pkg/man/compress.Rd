\name{compress.factor}
\alias{compress}
\alias{compress.default}
\alias{compress.factor}
\title{Compress a factor based on discrete values in a vector}
\description{
The function compress a factor (or any vector) based on discrete values in a vector, if the levels of the vector are nested in the levels of the factor.
}
\usage{
report(x, \dots)
\method{compress}{default}(x, y)
\method{compress}{factor}(x, y)
}
\arguments{
  \item{x}{a factor or a vector.}
  \item{y}{a vector with discrete values.}
}
\details{
\code{x} and \code{y} should have same lengths, and \code{y} should be nested into \code{x} to fulfill that \code{length(unique(interaction(x, y))) == length(unique(y))}. The procedure is similar to \code{\link{aggregate}}, but it can not be easily applied for factors. It can, however, be the case when aggregating data in hierarchically nested sampling design.
}
\value{
A factor with length as the number of discrete values in \code{y}.}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}, 
}
\seealso{\code{\link{stsc}}}
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
