\encoding{UTF-8}
\name{as.mefa}
\alias{as.Mefa}
\alias{as.Xtab}
\alias{as.mefa}
\alias{as.mefa.default}
\alias{as.mefa.array}
\alias{as.mefa.list}
\alias{as.stcs}
\alias{as.stcs.default}
\alias{as.data.frame.stcs}
\alias{as.array.mefa}
\alias{as.list.mefa}
\alias{as.matrix.mefa}
\alias{as.data.frame.mefa}
\alias{mss}
\alias{msr}
\alias{mts}
\alias{mtr}
\title{Conversion Among Various Object Classes}
\description{
These functions coerce into class 'mefa' or 'stcs'; or converts 'mefa' or 'stcs' obects into a 'data.frame', 'matrix', 'array', 'list'.
}
\usage{
as.stcs(x, \dots)
\method{as.stcs}{default}(x, \dots)
as.mefa(x, \dots)
as.Mefa(x, \dots)
as.Xtab(x, \dots)
\method{as.mefa}{default}(x, samp, taxa, \ldots)
\method{as.mefa}{array}(x, \ldots)
\method{as.mefa}{list}(x, \ldots)
\method{as.matrix}{mefa}(x, \ldots)
\method{as.array}{mefa}(x, \ldots)
\method{as.list}{mefa}(x, \ldots)
\method{as.data.frame}{stcs}(x, \ldots)
\method{as.data.frame}{mefa}(x, \ldots, fun, name, make.unique = FALSE)
mss(x, name, make.unique = FALSE, \ldots)
msr(x, name, make.unique = FALSE, \ldots)
mts(x, name, make.unique = FALSE, \ldots)
mtr(x, name, make.unique = FALSE, \ldots)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{samp}{a data frame containing rows for samples, or \code{NULL}. See argument \code{xtab.fixed} and details for sample matching options.}
  \item{taxa}{a data frame containing rows for taxa, or \code{NULL}. See argument \code{xtab.fixed} and details for taxa matching options.}
  \item{fun}{a function to determine what to return, see details. It has no default, because the choice is not trivial.}
  \item{name}{optional character vector, names to return if not all possible elements are needed, see details.}
  \item{make.unique}{logical, useful to use \code{make.unique = TRUE} if names to return are non-unique, see details.}
  \item{\ldots}{other arguments passed to the generic function \code{\link{as.data.frame}}.}
}
\details{
The usage of most of these coercion methods is trivial. Arrays and lists can be coerced into 'mefa' objects as if those were the \code{segm} element of it (either nested, or non nested). The reverse is true, when coercing an object of class 'mefa' into list or array, the \code{segm} element is used.

Coercing among object classes 'matrix' and 'mefa' is done via using the \code{xtab} element.

By \code{as.data.frame.mefa}, four kinds of data frames can be returned, depending on the function used as the \code{fun} argument:
\code{mss} returns summary statistics for samples (richness and abundance) and \code{x$samp};
\code{msr} returns raw data (\code{x$xtab}) and \code{x$samp};
\code{mts} returns summary statistics for taxa (occurrence and abundance) and \code{x$taxa};
\code{mtr} returns raw data (\code{t(x$xtab)}) and \code{x$taxa}.

The \code{name} can be used if only a subset of the summary statistics, or raw data should be returned. The character vector should contain names of elements to return (see examples).

It might (rarely) occur, that names of the summary statistics, or the raw data and the column names of the sample/taxa table overlap (contains names that are common in the two set). In this case, the \code{make.unique = TRUE} can be used to resolve non-uniqueness and avoid the error message produced otherwise.

The functions supplied as the \code{fun} argument can be used separately. Although the usage of the \code{as.data.frame} method is more transparent.
}
\value{
An object of class 'stcs', 'mefa', 'matrix', 'array', 'list' or 'data.frame', depending on the method used.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \doi{10.18637/jss.v029.i08}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}, \code{\link{stcs}}, \code{\link{as.matrix}}, \code{\link{as.list}}, \code{\link{as.array}}, \code{\link{as.data.frame}}}
\examples{
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## These two are equivalent
as.data.frame(x, fun = mss)
mss(x)
## Return only two species
as.data.frame(x, fun = msr, c("iiso", "ppyg"))
## Taxa table and summary
as.data.frame(x, fun = mts)
## Taxa table and raw data transpose
as.data.frame(x, fun = mtr)
## Why is it useful?
## Instead of
glm(x$xtab[,"amin"] ~ microhab + method, data = x$samp, family = poisson)
## it is more convenient to use
glm(amin ~ microhab + method, data = msr(x), family = poisson)
}
\keyword{ methods }
\keyword{ manip }
