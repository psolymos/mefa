\encoding{UTF-8}
\name{stcs}
\alias{stcs}
\alias{is.stcs}
\alias{as.stcs}
\alias{summary.stcs}
\title{Create an Object of Class 'stcs'}
\description{
This function creates object of class 'stcs' from a data frame. This is the primary format for long (database style) data sets in the \strong{mefa} package. The 'stcs' object can be converted into an object of class 'mefa' with the function \code{mefa}. The function name 'stcs' refers to first letters of column names of the resulting object: \strong{s}amples, \strong{t}axa, \strong{c}ounts, \strong{s}ebments.
}
\usage{
stcs(dframe, expand = FALSE, drop.zero = FALSE, zero.pseudo = "zero.pseudo")
\method{is}{stcs}(x)
\method{as}{stcs}(x, \dots)
\method{summary}{stcs}(object, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dframe}{a data frame with 2-4 columns. If 2 columns are provided, it is assumed that first column contains sample, while second taxa names. If 3 columns are provided, the first two is treated as sample and taxa names, while the third is treated as count if numeric, and segment if character or factor. If 4 columns are provided, those are assumed to be in the samples, taxa, count, segment order.}
  \item{expand}{logical, whether the object should be (\code{TRUE}) expanded (via the function \code{\link{inflate}}) or not (\code{FALSE}, default). If expanded, each rows correspond to one individual observation (value of the \code{count} column is 1) or a zero sample.}
  \item{drop.zero}{logical, whether samples with zero total count should be left out (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{zero.pseudo}{character, value in the \code{taxa} column that indicates zero samples.}
  \item{x, object}{an object of class 'stcs'.}
  \item{\dots}{further arguments passed to the function \code{stcs}.}
}
\details{
If the data are in a long (database style) format, the \code{stcs} function prepares the ground for the \code{mefa} function to make the cross tabulation. But if only a crosstabulated matrix is needed, the \code{\link{table}} function can be applies as well (in this case be sure to set \code{expand = TRUE}). For subsetting, simply use extraction methods available for data frames (\code{\link{[.data.frame}}).

The summary contains statistics calculated after coercing the object into the class 'mefa'.

Methods for extracting, subsetting are the same as for data frame objects. Plotting 'stcs' objects are discussed elsewhere (see links in 'See also' section).
}
\value{
Returns an object of class 'stcs' with 4 columns. The column names are converted consistently into \code{samp} for samples, \code{taxa} for taxa names, \code{count} for counts and \code{segm} for segment names.

The class has some own methods (i.e. \code{is} and \code{as}), but also has a \code{data.frame} class attribute. Thus all methods available for data frames are applicable to an object of class 'stcs' (e.g. \code{summary}, \code{str}).
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{
The \code{stcs} function nearly equivalent to the \code{sscount} function in older (< 2.0) versions of the \strong{mefa} package.
}
\seealso{\code{\link{data.frame}}, \code{\link{summary.data.frame}}, \code{\link{str}}, \code{\link{table}}}
\examples{
## General long format data
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
x
## Long format as stcs
y <- stcs(x)
y
## Methods
as.stcs(x)
is(y, "stcs")
is(y, "data.frame")
## Effects of arguments
stcs(x, expand = TRUE)
stcs(x, drop.zero = TRUE)
stcs(x, zero.pseudo = "pseudo.secies")
## Input options
stcs(x[,1:2])
stcs(x[,1:3])
stcs(x[,c(1:2,4)])
## The DOLINA dataset
data(DOLINAcounts)
dol <- stcs(DOLINAcounts)
dol
plot(dol)
}
\keyword{ manip }
\keyword{ methods }
\keyword{ classes }
