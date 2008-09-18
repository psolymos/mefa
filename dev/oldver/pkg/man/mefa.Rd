\name{mefa}
\alias{mefa}
\alias{as.mefa}
\alias{print.mefa}
\alias{plot.mefa}
\alias{report.mefa}
\alias{report}
\title{Makes Object of Class 'mefa'}
\description{
Makes and object of class 'mefa' from an 'xcount' and sample and species attribute 'xorder' objects. Incomplete 'mefa' object can becreated, when one out of samples or species attributes are missing.
}
\usage{
mefa(xc, xorder.samples, xorder.species, n = 1)

\method{print}{mefa}(x, cutoff = 25, \dots)

\method{plot}{mefa}(x, sample.var = NULL, species.var = NULL, \dots)

\method{as}{mefa}(x, n = NULL, \dots)

report(x, \dots)
\method{report}{mefa}(x, filename, n = 1, ordering = "species", biotic.data = 1, 
    species.name = 1, species.order = 1, author = 0, 
    tex = FALSE, binary = FALSE, sep = c(", ", " (", ")", "; "), \dots)
}
\arguments{
  \item{xc}{object of class 'xcount' or 'xclist'. If \code{xc} is an object of class 'xclist', than the \code{n} argument is used to choose from the list.}
  \item{xorder.samples}{object of class 'xorder' for sample attributes, or \code{NULL}.}
  \item{xorder.species}{object of class 'xorder' for species attributes, or \code{NULL}.}
  \item{x}{an object of class 'mefa' of 'mflist' (for \code{as.mefa}). If \code{mf} is an object of class 'mflist', than the \code{n} argument is used to choose from the list. For the \code{report} method, the object \code{mf} can not have any \code{NULL} attribute tables.}
  \item{n}{element of the 'xclist' or 'mflist' object to choose.}
  \item{cutoff}{number of samples/species with zero toal count to be listed.}
  \item{sample.var}{column index (numeric or character) of the sample attribute table to plot as independent variable.}
  \item{species.var}{column index (numeric or character) of the species attribute table to plot as independent variable.}
  \item{filename}{name of the file to be created in the working directory.}
  \item{ordering}{type of ordering (sectioning), by \code{"species"} (default) or by \code{"samples"}.}
  \item{biotic.data}{list of columns in the sample attribute table of the 'mefa' object,
    data will be sorted according to subsequent columns (there is no limit for length).}
  \item{species.name}{a column identifier in the species attribute table of the 'mefa' object
    containing species names.}
  \item{species.order}{a column identifier in the species attribute table of the 'mefa' object,
    by which the ordering of species names can be made (eg. according to taxonomy).}
  \item{author}{a column identifier in the species attribute table of the 'mefa' object,
    containing author names for species. By default it is \code{0}, that is no author names are listed.}
  \item{tex}{logical operator, if \code{TRUE} species names will be italicized and sectioning will
    be used according to \code{ordering} argument. Default is \code{FALSE}.}
  \item{binary}{logical operator, if \code{TRUE} count data will not be written into the output file.
    Default is \code{FALSE}, when count data are written.}
  \item{sep}{a character vector, with four elements, containing separator strings. The first is placed between \code{biotic.data} values, second is placed before, third is after the count value,
    fourth is the separator between full data entries. After the last entry,
    a dot is placed instead of the fourth separator.}
  \item{...}{other arguments.}
}
\details{
The \code{plot} method works as for 'xcount' objects if only a 'mefa' object is specified. If sample and species attributes (one or both) are also specified, scatterplots or box-and-whiskers diagrams are drawn, based on the type of the independent variable. Species richness, number of individuals, abundance and number of species occurrences are plotted as response variable. If the crosstable is binary, only richness and occurrences are used.

When \code{tex = TRUE}, the result of the \code{report} method can be directly used in a LaTeX document by copy-pasting the output or by the include option.

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
  \item{srichn}{a vector, containing number of species (based on occurrences) within samples.}
  \item{specabund}{a vector, containing number of individuals (or sum of measurement values) within species.}
  \item{specoccur}{a vector, containing number of occurrences within species.}
  \item{attributes}{character, indicating the presence of \code{"both"} or only one 
    (\code{"samples.only"} or \code{"species.only"}) attribute tables.}
  \item{attrib.matrix}{a matrix with 2 rows and 3 columns. Columns contain values of \code{check.setrel},
    number of variables and \code{na} from 'xorder' objects for samples (first row) and species 
    (second row). If one 'xorder' object is missing, respective row contains \code{NA}s.}

The \code{report} method writes text file named \code{filename} into the working directory.
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

### Reporting
v2 <- strify(vmf, strata = "site.descr", which = "samples")
habi <- xorder(as.xcount(v2), which="samples", vhabitat, 2)
vmf2 <- mefa(as.xcount(v2), habi, spec)

report.mefa("vspec-report.txt", vmf2, "species", c(3,2,4:7), "spec.name", "order", 0,
    FALSE, FALSE, sep = c(", "," (",")","; "))

report.mefa("vsampl-report.txt", vmf2,"samples", c(3,2,4:7), "spec.name", "order", 0,
    FALSE, FALSE, sep = c(", "," (",")",",\n"))

report.mefa("vspec-report.tex", vmf2,"species", c(3,2,4:7), "spec.name", "order", "author",
    TRUE, FALSE, sep = c(", "," (",")","; "))

report.mefa("vsampl-report.tex", vmf2,"samples", c(3,2,4:7), "spec.name", "order", "author",
    TRUE, TRUE, sep = c(", "," (",")",",\n"))

### Sweave example

mefadocs("SampleReport")
}
}
\keyword{manip}
\keyword{classes}
