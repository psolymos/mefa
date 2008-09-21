\name{melt}
\alias{melt}
\alias{melt.mefa}
\title{Melt (convert) a 'mefa' object into an 'stcs' object}
\description{
The function converts a wide formatted 'mefa' object into a long formatted 'stcs' object. The resulting segment column can take various formats.
}
\usage{
melt(x, \dots)
\method{melt}{mefa}(x, segm.var=NULL, by.samp=TRUE, raw.out=FALSE, drop.zero=FALSE, \dots)
}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class 'mefa'}
  \item{segm.var}{\code{NULL} (default) or column indexes of samples (if \code{by.samp = TRUE}) or taxa tables (if \code{by.samp = FALSE}). If it refers more than one column, their interaction is taken. If \code{NULL}, the resulting segment column is filled with \code{"undefined"}.}
  \item{by.samp}{logical, if \code{TRUE} (default) \code{segm.var} is taken from the samples table (\code{x$samp}), if \code{FALSE} \code{segm.var} is taken from the taxa table (\code{x$taxa}).}
  \item{raw.out}{logical, whether the result should contain all the zeros that is in the crosstabulated (wide) matrix (\code{TRUE}) or not (\code{FALSE}, default). Removing the zeros is straightforward for sparse matrices. But empty samples can be ratained if \code{drop.zero = FALSE}.}
  \item{drop.zero}{logical, whether samples with zero total count should be leaved out (\code{TRUE}) or not (\code{FALSE}, default).}
  \item{\dots}{other arguments passed to the function \code{\link{stcs}}.}
}
\details{
The generic function \code{metlt} can also be found in the \code{reshape} package along with methods for data frames, arrays and lists (similar functions in other packages are listed below) 

Besides the goal to convert from wide format to long format, this 'mefa' specific implementation is also useful, if a segment is not coded along with the long formatted database, but can be found in a linked table of samples or species. In this way, a 'mefa' object with new segments can be defined easily.
}
\value{
An object of class 'stsc'.
}
\references{
Solymos P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Similar functions in the \code{reshape} package of Hadley Wickham: \code{\link[reshape]{melt.default}}, \code{\link[reshape]{melt.data.frame}}, \code{\link[reshape]{melt.array}}.

Similar function in the \code{Hmist} package by Frank Harrel: \code{\link[Hmisc]{reShape}}.

Similar function in the \code{simba} package by  Gerald Jurasinski: \code{\link[simba]{liste}}.
}
\examples{

}
\keyword{ methods }
\keyword{ manip }
