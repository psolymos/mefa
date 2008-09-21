\name{mefa}
\alias{mefa-package}
\alias{mefa}
\alias{is.mefa}
\alias{as.mefa}
\alias{print.mefa}
\alias{summary.mefa}
\alias{print.summary.mefa}
\title{Create an object of class 'mefa'}
\description{
An object of class 'mefa' is a compendium of a crosstabulated taxa-by-samples count data, and optionally count data for segments, and linked tables for samples and taxa. Some of the methods for 'mefa' objects are discussed here, others can be found on help pages of the functions \code{\link{aggregate.mefa}}, \code{\link{melt.mefa}}, \code{\link{report.mefa}} and \code{\link{plot.mefa}}.
}
\usage{
mefa(xtab, samp = NULL, taxa = NULL, id.samp = NULL, id.taxa = NULL,
segment = TRUE, nested = FALSE, drop.zero = FALSE, drop.index = FALSE,
xtab.fixed = TRUE)
\method{as}{mefa}(x, \ldots)
\method{is}{mefa}(x)
\method{print}{mefa}(x, \ldots)
\method{summary}{mefa}(object, \ldots)
\method{print}{summary.mefa}(x, nlist = 10, \ldots)
\method{dim}{mefa}(x)
\method{dimnames}{mefa}(x)
}
\arguments{
  \item{xtab}{ ~~Describe \code{xtab} here~~ }
  \item{samp}{ ~~Describe \code{samp} here~~ }
  \item{taxa}{ ~~Describe \code{taxa} here~~ }
  \item{id.samp}{ ~~Describe \code{id.samp} here~~ }
  \item{id.taxa}{ ~~Describe \code{id.taxa} here~~ }
  \item{segment}{ ~~Describe \code{segment} here~~ }
  \item{nested}{ ~~Describe \code{nested} here~~ }
  \item{drop.zero}{ ~~Describe \code{drop.zero} here~~ }
  \item{drop.index}{ ~~Describe \code{drop.index} here~~ }
  \item{xtab.fixed}{ ~~Describe \code{xtab.fixed} here~~ }
  \item{x, object}{an object of class 'mefa'.}
  \item{nlist}{number of elements in the list of segment names to print. List is truncated at 10 by default.}
  \item{\dots}{other arguments passed to functions.}
}
\details{
mefa

The method \code{dim} returns a vector with 3 values for number of samples, number of taxa and number of segments in the 'mefa' object. The third value is 1 in cases, when \code{s$segm} is \code{NULL} (because it is 1 undefined segment essentially identican.to the matrix \code{x$xtab}, thus no need for a replicate). The \code{dimnames} method returns
}
\value{
An object of class 'mefa'. It is a list with 5 elements:
  \item{call}{the function call.}
  \item{xtab}{crosstabulated count data, rows are samples, columns are taxa.}
  \item{segm}{list of matrices if segments are used, othervise \code{NULL}.}
  \item{samp}{a data frame for sample related data (rows correspond to samples) if provided, othervise \code{NULL}.}
  \item{taxa}{a data frame for taxa related data (rows correspond to taxa) if provided, othervise \code{NULL}.}
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\note{
The \code{mefa} function alone can be used instead of the combination of object classes \code{xcount} and \code{xorder} used in older (< 2.0) versions of the mefa package. Further it is also extended by previously undocumented features.
}
\seealso{Further methods detailed on seperate help pages: see \code{\link{aggregate.mefa}} for substracting and aggregating the data, \code{\link{melt.mefa}} for redefining segments or melting data into long format, \code{\link{report.mefa}} for generating report into file, and \code{\link{plot.mefa}} for plotting options.

The simple 2 dimensional wide arrangement of long formatted data can be done by the function \code{\link[reshape]{reshape}} in the \code{reshape} package of Hadley Wickham, and by the \code{\link[simba]{mama}} function in the \code{simba} package by  Gerald Jurasinski. These are, however, not useful either for 3 dimensional (segmented) crosstabulation (see \code{\link{table}} function), or linking sample and taxa data tables with the count data.
}
\examples{

}
\keyword{ classes }
\keyword{ manip }
\keyword{ methods }
