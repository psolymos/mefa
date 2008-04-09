\name{mefa}
\alias{mefa}
\alias{print.mefa}
\alias{plot.mefa}
\title{Makes Object of Class 'mefa'}
\description{
Makes and object of class 'mefa' from an 'xcount' and sample and species attribute 'xorder' objects. Incomplete 'mefa' object can becreated, when one out of samples or species attributes is missing.
}
\usage{
mefa(xc, xorder.samples, xorder.species, n = 1)

\method{print}{mefa}(x, cutoff = 25, \dots)

\method{plot}{mefa}(x, sample.var = NULL, species.var = NULL, \dots)
}
\arguments{
  \item{xc}{object of class 'xcount' or 'xclist'. If \code{xc} is an object of class 'xclist', than the \code{n} argument is used to choose from the list.}
  \item{xorder.samples}{object of class 'xorder' for sample attributes, or \code{NULL}.}
  \item{xorder.species}{object of class 'xorder' for species attributes, or \code{NULL}.}
  \item{n}{element of the 'xclist' object to choose.}
  \item{x}{an object of class 'mefa'.}
  \item{sample.var}{column index (numeric or character) of the sample attribute table to plot as independent variable.}
  \item{species.var}{column index (numeric or character) of the species attribute table to plot as independent variable.}
  \item{cutoff}{number of samples/species with zero toal count to be listed.}
  \item{...}{other arguments.}
}
\details{
The \code{plot} method works as for 'xcount' objects if only a 'mefa' object is specified. If sample and species attributes (one or both) are also specified, scatterplots or box-and-whiskers diagrams are drawn, based on the type of the independent variable. Species richness, number of individuals, abundance and number of species occurrences are plotted as response variable. If the crosstable is binary, only richness and occurences are used.
}
\value{
A result is an object of class 'mefa'.
  \item{data}{count data from the 'xcount' object.}
  \item{sample.attr}{data frame containing data of 'xorder' object containing sample attributes.}
  \item{species.attr}{data frame containing data of 'xorder' object containing species attributes.}
  \item{call}{returns the call.}
  \item{segment}{segment of the 'xcount' object.}
  \item{digits}{digits of the 'xcount' object.}
  \item{nsamples}{number of rows (samples) in \code{data}.}
  \item{nspecies}{number of columns (species) in \code{data}.}
  \item{totalcount}{sum of count or measurement in \code{data}.}
  \item{presences}{sum of presences in \code{data}.}
  \item{ninds}{a vector, containing number of individuals (or sum of measurement values) within samples.}
  \item{srichn}{a vector, containing number of species (based on occurences) within samples.}
  \item{specabund}{a vector, containing number of individuals (or sum of measurement values) within species.}
  \item{specoccur}{a vector, containing number of occurences within species.}
  \item{attributes}{character, indicating the presence of \code{"both"} or only one 
    (\code{"samples.only"} or \code{"species.only"}) attribute tables.}
  \item{attrib.matrix}{a matrix with 2 rows and 3 columns. Columns contain \code{check.setrel},
    number of variables and \code{na} from 'xcount' objects for samples (first row) and species 
    (second row). If one 'xorder' object is missing, respective row contains \code{NA}s.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{check.attrib}}, \code{\link{marmat}}, \code{\link{sscount}}, \code{\link{xcount}}, \code{\link{xorder}}
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

xct <- xcount(sscount(ss, zc="zero.count"))
xo1 <- xorder(xct, "samples", sampletab, 1)
xo2 <- xorder(xct, "species", spectab, 1)

mf1 <- mefa (xct, xo1, xo2)
mf1

mf2 <- mefa(xcount(sscount(ss, zc="zero.count"), 2), xo1, xo2)
mf2

### Example 2: field data of Villany Hills

\dontrun{
data(landsnail, vsample, vtable)

vt <- as.xcount(vtable, FALSE)
spec <- xorder(vt, which="species", landsnail, 2)
sampl <- xorder(vt, which="samples", vsample, 1)

vmf <- mefa(vt, sampl, spec)
vmf

plot(vmf)
plot(vmf,type="rank")
plot(vmf, 3)
plot(vmf, NULL, 5)
plot(vmf, "site.descr", "shell.dimension")

### Example 3: field data of the dolina

data(dol.count, dol.sample, landsnail)

dmf <- mefa(
dxc <- xcount(sscount(fill.count(dol.count), zc="zero.count")),
xorder(dxc, which="samples", dol.sample, 1),
xorder(dxc, which="species", landsnail, 2)
)

dmf

plot(dmf, "microhabitat", "shell.dimension")
}
}
\keyword{manip}
\keyword{classes}
