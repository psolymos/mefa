	\name{sscount}
\alias{sscount}
\alias{print.sscount}
\title{Make Object of Class 'sscount'}
\description{
The function makes an object of class 'sscount' from a data frame with columns for samples, species, count and optionally segment. Count column may contain count or measurement data, too. Data can be in 'notebook' style with empty cells.
}
\usage{
sscount(sstable, zc = NULL, fill = FALSE, digits = NULL)

\method{print}{sscount}(x, \dots)
}
\arguments{
  \item{sstable}{dataframe with three or four columns. Columns must be in the order of sample, species, segment (optional) and count.}
  \item{zc}{identifier for samples with zero count, by default it is \code{NULL}. When non existing identifier is given, the value is set to default with warning.}
  \item{fill}{logical, if \code{FALSE} (default), values in \code{sstable} will be left intact,
     if \code{TRUE}, \code{\link{fill.count}} is used to replace \code{NA} values according to the 
     last non-\code{NA} value in a given column of \code{sstable}}
  \item{digits}{\code{NULL} (default) is used when results are count data (eg. number of individuals). If result is 
    not integer, but \code{digits=NULL}, error message is given. If results are not of count type but some kind of measurements 
    (eg. biomass, containing decimals) \code{digits} can be used to set precision of the data representation, 
    in this case use integers (\code{digits=0} results integers, \code{digits=1} for 1 decimal, etc.).}
  \item{x}{an object of class 'sscount'.}
  \item{...}{other arguments.}
}
\details{
When the \code{zc} zero count identifier is used, respective rows in the segment column of the result will be replaced by \code{NA}s, and \code{count} values with \code{1}. Segment levels are also modifed accordingly.
}
\value{
A result is an object of class 'sscount'.
  \item{data}{data frame with columns \code{sample}, \code{species}, \code{segment}, \code{count}.
    First three columns are factors, \code{count} is numeric.}
  \item{call}{returns the call.}
  \item{zc}{flag for samples (rows) with 0 total count, \code{zc="zero.count"}.}
  \item{digits}{stores value of \code{digits} argument, when \code{digits} is not \code{NULL} and data are of 
     count type it is set to \code{NULL}.}
  \item{nsamples}{number of samples (levels) in data column \code{sample}.}
  \item{nspecies}{number of species (levels) in data column \code{species}, 
    "pseudo-species" created by \code{zc} is excluded.}
  \item{segment.levels}{vector with levels in data column \code{segment}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{drtsscount}}, \code{\link{fill.count}}, \code{\link{inflate}}, \code{\link{sscount}},
  \code{\link{ttsscount}}
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

sscount(ss)
sscount(ss, zc="zero.count")
sscount(ss[,c(1,2,4)], zc="zero.count")

ss[,4] <- ss[,4] + 0.23
sscount(ss, digits=0)
sscount(ss, digits=1)
sscount(ss, digits=2)


### Example 2: field data of the dolina

\dontrun{
data(dol.count, dol.sample, landsnail)

sscount(dol.count, zc="zero.count", fill=TRUE)
}
}
\keyword{manip}
\keyword{classes}
