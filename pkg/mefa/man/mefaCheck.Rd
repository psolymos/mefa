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
mefaCompare(x1, x2, strict = FALSE)
}
\arguments{
  \item{x, x1, x2}{an object of class 'mefa'.}
  \item{strict}{logical, if \code{TRUE} the function checks if ordering of dimnames are the same, otherwise it compares only matching.}
}
\value{
\code{mefaCheck} returns a list, first element is logical with \code{TRUE} indicating that all requirements are met. If this is \code{FALSE}, following elements of the list describe the encountered problems as characters.  \code{mefaCheck} is called by the \code{\link{is.mefa}} function.

\code{mefaCompare} compares two mefa objects. If all dimnames, and the corresponding values in the \code{xtab} and \code{segm} parts are identical, the result is \code{TRUE}, and otherwise \code{FALSE}. Different ordering of dimnames allowed, the comparison is based on the numeric values determined by dimnames.
}
\references{
\enc{S\'{o}lymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'{o}lymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'{e}ter S\'{o}lymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}, \code{\link{is.mefa}}, \code{\link{all.equal.list}}}
\examples{
x <- matrix(rpois(20,1), 5, 4)
m <- mefa(x)
mefaCheck(m)
data(dol.count)
x1 <- mefa(stcs(dol.count))
x2 <- mefa(melt(x1))
mefaCompare(x1, x2)
mefaCompare(x1, x2, strict = TRUE)
}
\keyword{ manip }
