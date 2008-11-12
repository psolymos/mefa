\encoding{UTF-8}
\name{mefaCheck}
\alias{mefaCheck}
\alias{mefaCompare}
\title{Check and Compare Object(s) of Class 'mefa'}
\description{
The functions evaluates the internal structute of a 'mefa' object.
}
\usage{
mefaCheck(x)
mefaCompare(x1, x2)
}
\arguments{
  \item{x, x1, x2}{an objects of class 'mefa'.}
}
\value{
\code{mefaCheck} returns a list, first element is logical with \code{TRUE} indicating that all requirements are met. If this is \code{FALSE}, following elements of the list describe the encountered problems as characters.  \code{mefaCheck} is called by the \code{\link{is.mefa}} function.

\code{mefaCompare} compares two mefa objects. If all dimnames, and the corresponding values in the \code{xtab} and \code{segm} parts are identical, the result is \code{TRUE}, and otherwise \code{FALSE}. Different ordering of dimnames allowed, the comparison is based on the numeric values determined by dimnames.
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
data(dol.count)
x1 <- mefa(stcs(dol.count))
x2 <- mefa(melt(x1))
mefaCompare(x1, x2)
}
\keyword{ manip }
