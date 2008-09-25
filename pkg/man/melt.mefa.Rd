\encoding{UTF-8}
\name{melt}
\alias{melt}
\alias{melt.mefa}
\title{Melt (convert) a 'mefa' object into an 'stcs' object}
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
  \item{raw.out}{logical, whether the result should contain all the zeros that is in the crosstabulated (wide) matrix (\code{TRUE}) or not (\code{FALSE}, default). Removing the zeros is straightforward for sparse matrices. But empty samples can be ratained if \code{drop.zero = FALSE}. Works only if \code{drop.zero = FALSE}.}
  \item{drop.zero}{logical, whether samples with zero total count should be leaved out (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{\dots}{other arguments passed to the function \code{\link{stcs}}.}
}
\details{
Besides the goal to convert from wide format to long format, this 'mefa' specific implementation is also useful, if a segment is not coded along with the long formatted database, but can be found in a linked table of samples or species. In this way, a 'mefa' object with new segments can be defined easily.

If melting is done with a vector that is not part of the tables inside the 'mefa' object, it must have a class attribute to be recognised, use e.g. the \code{\link{as.factor}} function (see 'Examples').
}
\value{
An object of class 'stsc'.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
The generic function \code{melt} can also be found in the \strong{reshape} package of Hadley Wickham, along with methods for data frames, arrays and lists (\code{\link[reshape]{melt.default}}, \code{\link[reshape]{melt.data.frame}}, \code{\link[reshape]{melt.array}}).

Similar function in the \strong{Hmisc} package by Frank Harrel is the \code{\link[Hmisc]{reShape}}.

Similar function in the \strong{simba} package by  Gerald Jurasinski is the \code{\link[simba]{liste}}.
}
\examples{
data(DOLINAcounts, DOLINAsamples, DOLINAtaxa)
x <- mefa(stcs(DOLINAcounts), DOLINAsamples, DOLINAtaxa)
## Long format with undefined segment
melt(x)
## These two are identical
melt(x, "microhab")
melt(x, x$samp$microhab) ## this is a factor
## Using a simple vector as factor
melt(x, as.factor(rbinom(dim(x)[1], 1, 0.5)))
## Interaction
melt(x, c("method", "microhab"))
## From taxa table
melt(x, "familia", by.samp = FALSE)
}
\keyword{ methods }
\keyword{ manip }
