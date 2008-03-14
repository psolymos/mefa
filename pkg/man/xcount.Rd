\name{xcount}
\alias{xcount}
\alias{plot.xcount}
\alias{print.xcount}
\title{Makes an Object of Class 'xcount'}
\description{
The function makes an object of class 'xcount' from an object of class 'sscount'.
}
\usage{
xcount(ssc, segment = 0, segment.name = NULL)

\method{print}{xcount}(x, cutoff = 25, \dots)

\method{plot}{xcount}(x, type = "hist", rug = FALSE, logscale = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ssc}{an object of class 'sscount'.}
  \item{segment}{identifier of the \code{segment} within the 'sscount' object
    to make the crosstabulation, the default \code{0} refers to \code{"all"} segments.
    It can have more than one element (but lessthan or equal to the number of elements in the segment list).
    When it is a vector, corresponding levels are combined and named as specified by \code{segment.name}.}
  \item{segment.name}{character, it has effect only when \code{segment} is vector. If not specified, segment value 
    in the result is set to \code{"mixed"} (if all segments are listed, it is set to \code{"all"}).}
  \item{x}{an object of class 'xcount'.}
  \item{type}{character, can be \code{"hist"} for histogram, \code{"rank"} for rank abundance/occurence curves and 
    \code{"biplot"} for plot abundance vs. occurence data.}
  \item{rug}{logical, if \code{TRUE}, add rug to the plot (default is \code{FALSE}). It has effect only if 
    \code{type="hist"}.}
  \item{logscale}{logical, if \code{TRUE}, plotted values are $log10(y+1)$ transformed.
    Default \code{FALSE} leaves the values intact.}
  \item{cutoff}{number of samples/species with zero toal count to be listed.}
  \item{...}{other arguments.}
}
\details{
If data in the 'sscount' object are of measurement type, \code{digits} value is inherited from the 'sscount' object
and used in crosstabulation. For large (>2) \code{digit} values, computing time may take several seconds. Current implementation of handling measurement type data is based on the \code{\link{inflate}} function, which is not fully vectorized.

The \code{print} method produces 4 graphs in one window, displaying histograms \code{type="hist"} or rank abundance curves \code{type="rank"} of the vectors \code{ninds}, \code{srichn}, 
\code{specabund} and \code{specoccur}. If the data are of presence/absence and not abundance type, only \code{srichn} and \code{specoccur} are displayed. These values are based on the function \code{\link{marmat}}. The option \code{type="biplot"} deisplays 2 graphs, one for samples and one for species, combining presence/absence and abundance data in the same plot.
}
\value{
A result is an object of class 'xcount'.
  \item{data}{matrix containing count data of sample/species crosstabulation.
    Crosstabulation results the same dimensions for data matrices of each segments.}
  \item{call}{returns the call.}
  \item{segment}{segment of the 'sscount' object of which the crosstabulation was made, or \code{"all"}.}
  \item{digits}{inherited from the 'sscount' object.}
  \item{nsamples}{number of rows (samples) in \code{data}.}
  \item{nspecies}{number of columns (species) in \code{data}.}
  \item{totalcount}{sum of count or measurement in \code{data}.}
  \item{presences}{sum of presences in \code{data}.}
  \item{ninds}{a vector, containing number of individuals (or sum of measurement values) within samples.}
  \item{srichn}{a vector, containing number of species (based on occurences) within samples.}
  \item{specabund}{a vector, containing number of individuals (or sum of measurement values) within species.}
  \item{specoccur}{a vector, containing number of occurences within species.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{as.xcount}}, \code{\link{check.attrib}}, \code{\link{marmat}}, \code{\link{mefa}},
  \code{\link{sscount}}, \code{\link{xorder}}
}
\examples{
### Example 1: simple atrificial data

ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

xc1 <- xcount(sscount(ss, zc="zero"))
xc1

xc2 <- xcount(sscount(ss, zc="zero.count"))
xc2

xc3 <- xcount(sscount(ss, zc="zero.count"), 0)
xc3

xc4 <- xcount(sscount(ss, zc="zero.count"), 1)
xc4

### Example 2: field data of the dolina

\dontrun{
data(dol.count, dol.sample, landsnail)

dssc <- sscount(dol.count, zc="zero.count", fill=TRUE)
dssc

(xc <- xcount(dssc))
plot(xc, type="hist")
plot(xc, type="rank", logscale = TRUE)
plot(xc, type="biplot")

xcount(dssc, "broken")
xcount(dssc, c("adult", "juvenile"))
xcount(dssc, c(1,4), "mixed")
xcount(dssc, c(1:3))
}
}
\keyword{manip}
\keyword{classes}

