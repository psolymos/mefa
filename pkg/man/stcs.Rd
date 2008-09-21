\name{stcs}
\alias{stcs}
\alias{is.stcs}
\alias{as.stcs}
\title{Create object of class 'stcs'}
\description{
This function creates object of class 'stcs' from a data frame. This is the primary format for long (database style) data sets in the mefa package. The 'stcs' object can be used converted into an object of class 'mefa' with the function \code{mefa}
}
\usage{
stcs(xtab, expand = FALSE, drop.zero = FALSE, zero.pseudo = "zero.pseudo")
\method{is}{stcs}(x)
\method{as}{stcs}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtab}{a data frame with 2-4 columns. If 2 columns are provided, it is assumed that first column contains sample, while second taxa names. If 3 column is provided, the first two is treated as sample and taxa names, while the third is treated as count if numeric, and segment if character or factor. If 4 column is provided, those are assumed to be in the samples, taxa, count, segment order.}
  \item{expand}{logical, whether the object should be (\code{TRUE}) expanded (via the function \code{\link{inflate}}) or not (\code{FALSE}, default). If expanded, each rows correspond to one individual observation (value of the \code{count} column is 1) or a zero sample.}
  \item{drop.zero}{logical, whether samples with zero total count should be leaved out (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{zero.pseudo}{character, value in the \code{taxa} column that indicates zero samples.}
  \item{x}{an object of class 'stcs'.}
  \item{\dots}{further arguments passed to the function \code{stcs}.}
}
\details{
If the data are in a long (database style) format, the \code{stcs} function prepares the ground for the \code{mefa} function to make the cross tabulation. But if only a crosstabulated matrix is needed, the \code{\link{table}} function can be applies as well (in this case be sure to set \code{expand = TRUE}). For subsetting, simply use subastraction methods available for data frames (square brackets).

The \code{stcs} function nearly equivalent to the \code{sscount} function in older (< 2.0) versions of the mefa package.
}
\value{
Returns an object of class 'stcs' with 4 columns. The column names are converted consistently into \code{samp} for samples, \code{taxa} for taxa names, \code{count} for counts and \code{segm} for segment names.

The class has some own methods (i.e. \code{is} and \code{as}), but also has a \code{data.frame} class attribute. Thus all methods available for data frames are applicable to an object of class 'stcs' (e.g. \code{summary}, \code{str}).
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{data.frame}}, \code{\link{summary.data.frame}}, \code{\link{str}}, \code{\link{table}}}
\examples{
## General long format of the data
x <- data.frame(sample = LETTERS[c(1,1,2,2,3,4)],
    species = c(letters[c(5,5,5,6,7)], "zero.pseudo"),
    count = c(1,2,10,3,4,0), segment = letters[c(8,9,8,9,8,8)])
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
stcs(x, zero.pseudo = "pseudop.secies")
## Input options
stcs(x[,1:2])
stcs(x[,1:3])
stcs(x[,c(1:2,4)])
## Simple crosstabulation
## Note the removal of empty rows
z <- stcs(x[x$count != 0, ], expand = TRUE)
table(z[,1:2])
table(z[,c(1:2,4)])
## The DOLINA dataset
data(DOLINAcounts)
dol <- stcs(DOLINAcounts)
dol
}
\keyword{ manip }
\keyword{ methods }
\keyword{ classes }
