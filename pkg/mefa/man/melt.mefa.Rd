\encoding{UTF-8}
\name{melt}
\alias{melt}
\alias{melt.mefa}
\title{Melt (Convert) an Object of Class 'mefa' Into 'stcs'}
\description{
The function converts a wide formatted 'mefa' object into a long formatted 'stcs' object. The resulting segment column can take various formats.
}
\usage{
melt(x, \dots)
\method{melt}{mefa}(x, segm.var = NULL, by.samp = TRUE,
raw.out = FALSE, drop.zero = FALSE, \dots)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{segm.var}{\code{NULL} (default) or column indices of samples (if \code{by.samp = TRUE}) or taxa tables (if \code{by.samp = FALSE}). If it refers to more than one column, their interaction is taken. If \code{NULL}, the resulting segment column is filled with \code{"undefined"}.}
  \item{by.samp}{logical, if \code{TRUE} (default) \code{segm.var} is taken from the samples table (\code{x$samp}), if \code{FALSE} \code{segm.var} is taken from the taxa table (\code{x$taxa}).}
  \item{raw.out}{logical, whether the result should contain all the zeros that is in the crosstabulated (wide) matrix (\code{TRUE}) or not (\code{FALSE}, default). Removing the zeros is straightforward for sparse matrices. But empty samples can be ratained if \code{drop.zero = FALSE}.}
  \item{drop.zero}{logical, whether samples with zero total count should be leaved out (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{\dots}{other arguments passed to the function \code{\link{stcs}}.}
}
\details{
If a 'mefa' objet has original segments and \code{segm.var = NULL}, the function returns original segments. In this case, the \code{raw.out = TRUE} cannot be applied (because segments have different zero samples, so the option is meaningless). If the 'mefa' object contains only a data matrix without dimnames, rows and columns are numbered before melting.

Besides the goal to convert from wide format to long format, this 'mefa' specific implementation is also useful, if a segment is not coded along with the long formatted database, but can be found in a linked table of samples or species. In this way, a 'mefa' object with new segments can be defined easily.

If melting is done with a vector that is not part of the tables inside the 'mefa' object, it must have a class attribute to be recognised, use e.g. the \code{\link{as.factor}} function (see 'Examples').
}
\value{
An object of class 'stsc' if \code{raw = FALSE}, pure 'data.frame' othervise (because the zero count flag cannot be applied).
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
The generic function \code{melt} can also be found in the \strong{reshape} package of Hadley Wickham, along with methods for data frames, arrays and lists (\code{\link[reshape]{melt.default}}, \code{\link[reshape]{melt.data.frame}}, \code{\link[reshape]{melt.array}}).

Similar function in the \strong{Hmisc} package by Frank Harrel is the \code{\link[Hmisc]{reShape}}.

Similar function in the \strong{simba} package by  Gerald Jurasinski is the \code{\link[simba]{liste}}.
}
\examples{
data(dol.count, dol.samp, dol.taxa)
x <- mefa(stcs(dol.count), dol.samp, dol.taxa)
## Long format with original segments
melt(x)
## Long format with undefined segment
melt(as.mefa(x, segment = FALSE))
## These two are identical
melt(x, "microhab")
melt(x, x$samp$microhab) ## this is a factor
## Using a simple vector as factor
melt(x, as.factor(rbinom(dim(x)[1], 1, 0.5)))
## Interaction
melt(x, c("method", "microhab"))
## From taxa table
melt(x, "familia", by.samp = FALSE)
## Example for non-integer data
y <- x$xtab
y[y > 0] <-  y[y > 0] + 0.123
z <- mefa(y)
melt(z)
}
\keyword{ methods }
\keyword{ manip }
