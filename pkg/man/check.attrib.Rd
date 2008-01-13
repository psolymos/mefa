\name{check.attrib}
\alias{check.attrib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks Integrity of 'xcount' Object and Attribute Table}
\description{
This function checks integrity of an object of class 'xcount' and an attribute table (data frame) by comparing row/column names (as sample/species identifiers) and values in an index column of the data frame.
}
\usage{
check.attrib(xc, which = c("samples", "species"), attrib, index = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{an object of class 'xcount'.}
  \item{which}{switch for rows (\code{"samples"}) or columns (\code{"species"}) of the 'xcount' object to be compared
    according to \code{index}.}
  \item{attrib}{data frame to be checked. \code{NA} values are counted.}
  \item{index}{column identifier within \code{attrib} to be compared according to row/column names (as sample/species
    identifiers) of the 'xcount' object according to value of \code{which}. 
    By default, \code{index = 0}, and rownames are taken as identifiers.}
}
\value{
  \item{call}{returns the call.}
  \item{set.relation}{\code{"equal"} means the two sets (row/column names of the 'xcount' object and elements in the \code{index} column) are equal, \code{"inclusion"} means the two sets are inclusive and no missing elements found, \code{"intersect"} means the two sets intersect (required elements are listed in \code{missing}), \code{"separate"} means the two sets have no common elements (required elements are listed in \code{missing}).}
  \item{duplicate}{a list of duplicate elements or \code{NULL}.}
  \item{missing}{a list of missing elements or \code{NULL}.}
  \item{na}{indicates the number of \code{NA} values in \code{attrib}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{mefa}}, \code{\link{xcount}}, \code{\link{xorder}}
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

xct <- xcount(sscount(ss))

check.attrib(xct, "samples", sampletab, 1)
check.attrib(xct, "species", spectab, 1)
check.attrib(xct, "species", spectab, 2)
check.attrib(xct, "species", spectab[2:5,], 1)
check.attrib(xct, "species", spectab[4:5,], 1)

### Example 2: field data of Villany Hills

\dontrun{
data(vtable, vsample, landsnail)

vt <- as.xcount(vtable, FALSE)

check.attrib(vt, which="species", landsnail, 2)
check.attrib(vt, which="samples", vsample, 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}


