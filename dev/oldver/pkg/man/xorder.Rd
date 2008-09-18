\name{xorder}
\alias{xorder}
\alias{print.xorder}
\title{Make Object of Class 'xorder'}
\description{
The function makes an object of class 'xorder' from an attribute table (data frame) based on comparison with an 'xcount' object. Data frame subsetting is made according to results of function \code{\link{check.attrib}}.
}
\usage{
xorder(xc, which = c("samples", "species"), attrib, index = 0)

\method{print}{xorder}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{an object of class 'xcount'.}
  \item{which}{switch for rows (\code{"samples"}) or columns (\code{"species"}) of the 'xcount' object to be compared
    according to \code{index}.}
  \item{attrib}{data frame to be checked.}
  \item{index}{column identifier within \code{attrib} to be compared according to row/column names (as sample/species
    identifiers) of the 'xcount' object according to value of \code{which}. 
    By default, index = 0, and rownames are taken as identifiers.}
  \item{x}{an object of class 'xorder'.}
  \item{...}{other arguments.}

}
\value{
A result is an object of class 'xorder'.
  \item{data}{data frame, ordered and subsetted according to the 'xcount' object.}
  \item{call}{returns the call.}
  \item{which}{the value of the \code{which} argument, either \code{"samples"} or \code{"species"}.}
  \item{check.setrel}{\code{set.relation} value of function \code{\link{check.attrib}}.}
  \item{na}{indicates the number of \code{NA} values in \code{attrib}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{check.attrib}}, \code{\link{mefa}}, \code{\link{xcount}}
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

xct <- xcount(sscount(ss, "zero.count"))

xo1 <- xorder(xct, "samples", sampletab, 1)
xo2 <- xorder(xct, "species", spectab, 1)

### Example 2: field data of Villany Hills

\dontrun{
data(vtable, vsample, landsnail)

vt <- as.xcount(vtable, FALSE)

spec <- xorder(vt, which="species", landsnail, 2)
spec

sampl <- xorder(vt, which="samples", vsample, 1)
sampl
}
}
\keyword{manip}
\keyword{classes}
