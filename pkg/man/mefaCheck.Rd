\encoding{UTF-8}
\name{mefaCheck}
\alias{mefaCheck}
\title{Check an Object of Class 'mefa'}
\description{
The function evaluates the internal structute of a 'mefa' object. This function is called by the \code{\link{is.mefa}} function.
}
\usage{
mefaCheck(x)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
}
\value{
Returns a list, first element is logical with \code{TRUE} indicating that all requirements are met. If this is \code{FALSE}, following elements of the list describe the encountered problems as characters.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}, \code{\link{is.mefa}}}
\examples{
x <- matrix(rpois(20,1), 5, 4)
m <- mefa(x)
mefaCheck(m)
}
\keyword{ manip }
