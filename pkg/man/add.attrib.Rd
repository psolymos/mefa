\name{add.attrib}
\alias{add.attrib}
\title{Add Attributes to Object of Class 'mefa'}
\description{
The function adds attributes to an object ofclass 'mefa'.
}
\usage{
add.attrib(mf, which = c("samples", "species"), attrib, index = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mf}{a 'mefa' object.}
  \item{which}{attribute table within the 'mefa' object to be extended by new attributes, it can be the
    \code{"species"} and \code{"samples"} attribute table.}
  \item{attrib}{name of the data frame containing new attributes.}
  \item{index}{column index of the data frame \code{attrib} containing new attributes, default (\code{index = 0}) it 
    refers to the rownames of the attribute table.}
}
\value{
A result is an object of class 'mefa' containing the original and the new attributes as well. Index column (\code{index > 0}) from the new attributes table is removed, because it is a duplum.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{mefa}}
}
\examples{
###Example 1: simple atrificial data

ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

sampletab1 <- as.data.frame(rbind(
	c("sample3", "bad"),
	c("sample1", "not-so-bad"),
	c("sample2", "good"),
	c("sample4", "very-bad")))
colnames(sampletab1) <- c("sample", "quality")

sampletab2 <- as.data.frame(rbind(
	c("sample4", "debrecen", "nagyerdo", "bekasto", "2050-12-31", "bencike"),
	c("sample2", "pecs", "tettye", "mecsek", "1956-10-23", "barna"),
	c("sample1", "kiralyret", "bajdazoi", "bank", "2000-01-01", "XY"),
	c("sample3", "budapest", "pest", "rottenbiller", "2007-10-03", "SP")))
rownames(sampletab2)<-sampletab2[,1]
colnames(sampletab2) <- c("sample", "locality1", "locality2", "locality3", "date", "collector")

spectab <- as.data.frame(rbind(
	c("species3", "family1", "1"),
	c("species2", "family2", "5"),
	c("species1", "family1", "2"),
	c("species5", "family2", "1"),
	c("species4", "family1", "10")
))
colnames(spectab) <- c("species", "taxonomy", "size")

xct <- xcount(sscount(ss, zc="zero.count"))
xo1 <- xorder(xct, "samples", sampletab1, 1)
xo2 <- xorder(xct, "species", spectab, 1)

mf1 <- mefa(xct, xo1, xo2)

(mf1a <- add.attrib(mf, "samples", sampletab2))

mf2 <- mefa(xct, NULL, xo2)
(mf2a <- add.attrib(mf, "samples", sampletab2))

### Example 2: field data of Villany Hills

\dontrun{
data(vtable, landsnail, vsample, vhabitat)

vmf <- mefa(
vt <- as.xcount(vtable, FALSE), 
xorder(vt, which="samples", vsample),
xorder(vt, which="species", landsnail[,1:4], 2)
)

extra <- landsnail[,c(2, 5:8)][order(rnorm(nrow(landsnail))),]

vmf2 <- add.attrib(vmf, which="species", extra, 1)
vmf2
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}

