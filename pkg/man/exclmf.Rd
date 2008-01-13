\name{exclmf}
\alias{exclmf}
\title{Excludes Species or Samples in Object of Class 'xcount' or 'mefa'}
\description{
Excludes species or samples in object of class 'xcount' or 'mefa'.
}
\usage{
exclmf(xc, which = c("samples", "species"), excl, empty = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{an object of class 'xcount' or 'mefa' to be used.}
  \item{which}{switch between rows (\code{"samples"}) and columns (\code{"species"}) to be excluded.}
  \item{excl}{list of rows/columns (samples/species) to be excluded (according to \code{which} argument). 
    If \code{excl=NULL}, nothing will be excluded (only useful when \code{empty} is not \code{"none"}).}
  \item{empty}{logical, if \code{FALSE} (default), rows with zero sum will be left intact, 
    \code{TRUE} means that exclusion of empty rows/columns will be executed according to sums of the 
    opposite margin of the sample-species matrix, ie. if samples are to be excluded, the 
    resulting empty columns for species will be removed, and vica versa.}
}
\details{
If species are excluded, some samples might have zero total count. In such cases, 
it is advisable to use \code{empty=TRUE}.

Empty rows can be removed by \code{which="species"}, \code{excl=NULL} and \code{empty=FALSE}.
}
\value{
A result is an object of class 'xcount' or 'mefa' depending on the input object.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\note{
If the resultant crosstable contains less than two rows/columns, an error message is given and there will be no output.
}
\seealso{\code{\link{mefa}}, \code{\link{xcount}}.}
\examples{
ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

spectab <- as.data.frame(rbind(
	c("species3", "family1", "1"),
	c("species2", "family2", "5"),
	c("species1", "family1", "2"),
	c("species5", "family2", "1"),
	c("species4", "family1", "10")
))
colnames(spectab) <- c("species", "taxonomy", "size")

sampletab <- as.data.frame(rbind(
	c("sample3", "bad"),
	c("sample1", "good"),
	c("sample2", "good"),
	c("sample4", "bad")))
colnames(sampletab) <- c("sample", "quality")

xct <- xcount(sscount(ss, zc="zero.count"))
xo1 <- xorder(xct, "samples", sampletab, 1)
xo2 <- xorder(xct, "species", spectab, 1)
mf1 <- mefa (xct, xo1, xo2)

exclmf(xct, "samples", NULL, FALSE)
exclmf(xct, "samples", 3, FALSE)
exclmf(xct, "samples", 3, TRUE)
exclmf(xct, "samples", c(1:2), FALSE)

exclmf(xct, "species", NULL, TRUE) ## use to remove empty rows
exclmf(xct, "species", 1, FALSE)
exclmf(xct, "species", 1, TRUE)

exclmf(mf1, "samples", 3, FALSE)
exclmf(mf1, "samples", 3, TRUE)
}
\keyword{manip}
\keyword{utilities}
