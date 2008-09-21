\name{mefa}
\alias{mefa-package}
\alias{mefa}
\alias{is.mefa}
\alias{as.mefa}
\alias{print.mefa}
\alias{summary.mefa}
\alias{print.summary.mefa}
\alias{dim.mefa}
\alias{dimnames.mefa}
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
  \item{xtab}{count data. Can be an object of class 'stcs', or a matrix or data frame.}
  \item{samp}{a data frame containing rows for samples, or \code{NULL}. See argument \code{xtab.fixed} and details for sample matching options.}
  \item{taxa}{a data frame containing rows for taxa, or \code{NULL}. See argument \code{xtab.fixed} and details for taxa matching options.}
  \item{id.samp}{\code{NULL} if sample names for matching are the rownames of \code{samp}, otherwise, the index of the column of \code{samp} where the names are located.}
  \item{id.taxa}{\code{NULL} if taxa names for matching are the rownames of \code{taxa}, otherwise, the index of the column of \code{taxa} where the names are located.}
  \item{segment}{logical, whether segments column of the 'stcs' object should be used (\code{TRUE}, default). It has no effect when \code{xtab} is matrix or data frame.}
  \item{nested}{logical, whether segments should be nested within each other from the first to the last (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{drop.zero}{logical, if \code{TRUE} empty samples are removed from the result (can be useful before multivariate analyses). Otherwise, empty rows are left intact (\code{FALSE}, default).}
  \item{drop.index}{logical, whether columns of the samples and taxa tables containing the names used for matching should be removed (\code{TRUE}) to reduce redundancy or not (\code{FALSE}, default). This argument has effect only if \code{id.samp} and \code{id.taxa} are not \code{NULL}.}
  \item{xtab.fixed}{logical, if \code{TRUE} (default) the \code{samp} and \code{taxa} tables are subsetted and ordered according to the names of \code{xtab}. If \code{FALSE}, \code{xtab} is also subsetted according to the intersect of the names. Row and column ordering is determined by \code{xtab} in either cases.}
  \item{x, object}{an object of class 'mefa'.}
  \item{nlist}{number of elements in the list of segment names to print. List of segment names is truncated at 10 by default.}
  \item{\dots}{other arguments passed to functions.}
}
\details{
mefa -- effects of options

The method \code{dim} returns a vector with 3 values for number of samples, number of taxa and number of segments in the 'mefa' object. The third value is 1 in cases, when \code{s$segm} is \code{NULL} (because it is 1 undefined segment essentially identican.to the matrix \code{x$xtab}, thus no need for a replicate). The \code{dimnames} method returns
}
\value{
An object of class 'mefa'. It is a list with 5 elements:
  \item{call}{the function call.}
  \item{xtab}{crosstabulated count data, rows are samples, columns are taxa.}
  \item{segm}{list of matrices if segments are used, otherwise \code{NULL}.}
  \item{samp}{a data frame for sample related data (rows correspond to samples) if provided, otherwise \code{NULL}.}
  \item{taxa}{a data frame for taxa related data (rows correspond to taxa) if provided, otherwise \code{NULL}.}
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
\seealso{Further methods detailed on seperate help pages: see \code{\link{aggregate.mefa}} for extracting and aggregating the data, \code{\link{melt.mefa}} for redefining segments or melting data into long format, \code{\link{report.mefa}} for generating report into file, and \code{\link{plot.mefa}} for plotting options.

The simple 2 dimensional wide arrangement of long formatted data can be done by the function \code{\link[reshape]{reshape}} in the \code{reshape} package of Hadley Wickham, and by the \code{\link[simba]{mama}} function in the \code{simba} package by  Gerald Jurasinski. These are, however, not useful either for 3 dimensional (segmented) crosstabulation (see \code{\link{table}} function), or linking sample and taxa data tables with the count data.
}
\examples{
data(DOLINAcounts, DOLINAsamples, DOLINAtaxa)
## Input is an stcs object
x1 <- mefa(stcs(DOLINAcounts))
x1
## Input is a matrix object
x2 <- mefa(x1$xtab)
x2
## A data frame for samples attached
x3 <- mefa(stcs(DOLINAcounts), DOLINAsamples)
x3
## Data frame for samples and taxa attached
x4 <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa)
x4
## Methods
summary(x4)
is(x4, "mefa")
dim(x4)
dimnames(x4)
}
\keyword{ classes }
\keyword{ manip }
\keyword{ methods }
