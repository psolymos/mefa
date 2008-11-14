\encoding{UTF-8}
\name{aggregate.mefa}
\alias{aggregate.mefa}
\alias{[.mefa}
\title{Extract and Aggregate Objects of Class 'mefa'}
\description{
These methods are useful when extracting or aggregating 'mefa' objects. The count data matrix, the matrices for segments and the linked tables of samples and taxa are extracted or aggrgated at the same time.
}
\usage{
\method{aggregate}{mefa}(x, by.samp = NULL, by.taxa = NULL, \ldots)
\method{[}{mefa}(x, i = 1:dim(x)[1], j = 1:dim(x)[2], k = 1:dim(x)[3], drop = FALSE)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{by.samp}{a vector with length as the number of samples and more than one unique values to use aggregating samples.}
  \item{by.taxa}{a vector with length as the number of taxa and more than one unique values to use aggregating taxa.}
  \item{i, j, k}{index value or a vector of indices for samples, taxa and segments, respectively.}
  \item{drop}{logical, if \code{TRUE} the unused factor leves in the linked tables for samples and species will be dropped after extraction. If \code{FALSE} (default) unused factor levels are left intact.}
  \item{\ldots}{other arguments passed to the generic function.}
}
\details{
If a 'mefa' object is aggregated, either or both tables for samples and taxa (depending on if the aggregation was made by rows, columns or both) are set to \code{NULL}. Because aggregation is not straightforward for data frames with mixed type of data (e.g. factors, character vectors), and the applied function (mean, sum, etc.) depends on the type of the numeric variables. Consequently, if new data table is needed, that must be defined again by the function \code{\link{mefa}}. If the original data frame contained only numeric columns, it can be easily aggregated if the function to apply is known. For factors and numeric columns, however, such aggregation is not defined.

The ordering of dimensions follow the ordering of the indices in the extraction call, except for nested subsets, when sequence of nested segments is not allowd to be affected by the extraction (error message is given). The extracting method retains samples and taxa tables as well. If non-nested segments are extracted, the \code{x$xtab} matrix will contain the sum of segments, and not the original values of \code{x$xtab}. Thus if a resulting 'mefa' object has only one segment, it will be identical to the \code{x$xtab} matrix. If nested segments are extracted, \code{x$xtab} will be identical to the last segment and not the sum of segments. See the examples for these differences. If the original object had nested segments and the first is excluded, segment names change as a result. If only one segment remains in the result, the nested attribute is turned off.

If the aggregation is done over a vector that is not part of the tables inside the 'mefa' object, it must have a class attribute to be recognised, use e.g. the \code{\link{as.factor}} function (see 'Examples').

The resulting 'mefa' object must contain at least 2 samples and 2 taxa.
}
\value{
An object of class 'mefa'.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{
The \code{aggregate} method replaces the function \code{strify}, the \code{[} method replaces the function \code{exclmf} of the old (< 2.0) versions of the \strong{mefa} package.
}
\seealso{\code{\link{mefa}}}
\examples{
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## These two are identical
aggregate(x, "microhab")
aggregate(x, x$samp$microhab, inside = FALSE)
## Interaction
aggregate(x, c("method", "microhab"))
## Both samples and taxa tables
aggregate(x, "microhab", "familia")
## To use a factor
aggregate(x, "microhab", as.factor(rbinom(dim(x)[2], 1, 0.5)))
## Extraction
x[1:10, ]
x[, 1:5]
x[, , "broken"]
x[1:10, 1:5]
x[1:10, 1:5, 2]
## Note factor levels
y <- x[which(x$samp$method == "time"), ]
levels(y$samp$method)
z <- x[which(x$samp$method == "time"), drop = TRUE]
levels(z$samp$method)
## Note the differences in total sums
y <- mefa(stcs(dol.count), dol.samp, dol.taxa, nested = TRUE)
x[ , , 2]
y[ , , 2]
}
\keyword{ manip }
\keyword{ methods }
