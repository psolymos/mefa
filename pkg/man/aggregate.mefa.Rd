\name{aggregate.mefa}
\alias{aggregate.mefa}
\alias{[.mefa}
\title{Extracting and aggregating objects of class 'mefa'}
\description{
These methods are useful when extracting or aggregating 'mefa' objects. The count data matrix, the matrices for segments and the linked tables of samples and taxa are extracted or aggrgated at the same time.
}
\usage{
\method{aggregate}{mefa}(x, by.samp = NULL, by.taxa = NULL, inside = TRUE, \ldots)
\method{[}{mefa}(x, i = 1:dim(x)[1], j = 1:dim(x)[2], k = 1:dim(x)[3], drop = FALSE)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{by.samp}{a vector with length as the number of samples and more than one unique values to use aggregating samples.}
  \item{by.taxa}{a vector with length as the number of taxa and more than one unique values to use aggregating taxa.}
  \item{inside}{logica, if \code{TRUE} \code{by.samp} and \code{by.taxa} are searched in \code{x$samp} and \code{x$taxa} (inside \code{x}). If \code{FALSE}, \code{by.samp} and \code{by.taxa}  can be any vector with appropriate length.}
  \item{i}{index value or a vector of indices for samples.}
  \item{j}{index value or a vector of indices for taxa.}
  \item{k}{index value or a vector of indices for segments.}
  \item{drop}{logical, if \code{TRUE} the unused factor leves in the linked tables for samples and species will be dropped after sunstraction. If \code{FALSE} (default) uniused factor levels are left intact.}
  \item{\ldots}{other arguments passed to the generic function.}
}
\details{
If a 'mefa' object is aggregated, the either or both tables for samples and taxa (depending on if the aggregation was made by rows, columns or both) are set to \coude{NULL}. Because aggregation is not straightforward for data frames with mixed type of data (e.g. factors, character vectors), and the applied function (mean, sum, etc.) depends on the type of the numeric variables. Consequently, if new data table is needed, that must be defined again by the function \code{\link{mefa}}. If the original data frame contained only numeric columns, it can be easily aggregated if the applied function is known. For factors and numeric columns, however, such aggregation is not defined. The \code{\link{compress}} function can be used in certain circumstances for this purpose.

The extracting method retains samples and taxa tables as well. If non-nested segments are extracted, the \code{x$xtab} matrix will contain the sum of segments, and not the original values. Thus if a resulting 'mefa' object has only one segment, it will be identical to the \code{x$xtab} matrix. If nested segments are extracted, \code{x$xtab} will be identical to the last segment and not the sum of segments. See the examples for these differences.

The resulting 'mefa' object must contain at least 2 samples and 2 taxa.
}
\value{
An object of class 'mefa'.
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\note{
The \code{aggregate} method replaces the function \code{strify}, the \code{[} method replaces the function \code{exclmf} of the old (< 2.0) versions of the mefa package.
}
\seealso{\code{\link{mefa}}}
\examples{
data(DOLINAcounts, DOLINAsamples, DOLINAtaxa)
x <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa)
## These two are identical
aggregate(x, "microhab")
aggregate(x, x$samp$microhab, inside = FALSE)
## Interaction
aggregate(x, c("method", "microhab"))
## Both samples and taxa tables
aggregate(x, "microhab", "familia")
## Extraction
x[1:10, ]
x[, 1:5]
x[, , "broken"]
x[1:10, 1:5]
x[1:10, 1:5, 2:3]
## Note factor levels
y <- x[which(x$samp$method == "time"), ]
levels(y$samp$method)
z <- x[which(x$samp$method == "time"), drop = TRUE]
levels(z$samp$method)
## Note the differences in total sums
y <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa, nested = TRUE)
x[ , , 2:3]
y[ , , 2:3]
}
\keyword{ manip }
\keyword{ methods }
