\encoding{UTF-8}
\name{as.data.frame.mefa}
\alias{as.data.frame.mefa}
\alias{mss}
\alias{msr}
\alias{mts}
\alias{mtr}
\title{Convert an Object of Class 'mefa' Into 'data.frame'}
\description{
The function converts parts of a 'mefa' obect into a data frame.
}
\usage{
\method{as.data.frame}{mefa}(x, arrange, name, make.unique = FALSE)
mss(x, name, make.unique = FALSE)
msr(x, name, make.unique = FALSE)
mts(x, name, make.unique = FALSE)
mtr(x, name, make.unique = FALSE)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{arrange}{a function to determine whet to return, see details. It has no default, because the choice is not trivial.}
  \item{name}{optional character vector, names to return if not all possible elements are needed, see details.}
  \item{make.unique}{logical, useful to use \code{make.unique = TRUE} if names to returne are non-unique, dee details.}
}
\details{
Four kinds of data frames can be returned, depending on the function used as the \code{arrange} argument:

  \item{ss}{returns summary statistics for samples (richness and abundence) and \code{x$samp}.}
  \item{sr}{returns raw data (\code{x$xtab}) and \code{x$samp}.}
  \item{ts}{returns summary statistics for taxa (occurrence and abundence) and \code{x$taxa}.}
  \item{tr}{returns raw data (\code{t(x$xtab)}) and \code{x$taxa}.}

The \code{name} can be used if only a subset of the summary statistics, or raw data should be returned. The character vector should contain names of elements to return (see examples).

It might (rarely) occur, that names of the summary statistics, or the raw data and the column names of the sample/taxa table overlap (contains names that are common in the two set). In this case, the \code{make.unique = TRUE} can be used to resolve non-uniqueness and avoid the error message produced otherwise.

The functions supplied as the \code{arrange} argument can be used separately.
}
\value{
A data frame.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{as.data.frame}}}
\examples{
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## These two are equivalent
as.data.frame(x, mss)
mss(x)
## Return only two species
as.data.frame(x, msr, c("iiso", "ppyg"))
## Taxa table and summary
as.data.frame(x, mts)
## Taxa table and raw data transpose
as.data.frame(x, mtr)
## Why is it useful?
## Instead of
glm(x$xtab[,"amin"] ~ microhab + method, data = x$samp, family = poisson)
## it is more convenient to use
glm(amin ~ microhab + method, data = msr(x), family = poisson)
}
\keyword{ methods }
\keyword{ manip }
