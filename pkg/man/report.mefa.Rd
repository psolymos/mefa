\name{report.mefa}
\alias{report.mefa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reports Object of Class 'mefa'}
\description{
Makes a report from the data of an object of class 'mefa' in plain text or formatted LaTeX file.
}
\usage{
report.mefa(filename, mf, ordering = "species", biotic.data = 1, 
    species.name = 1, species.order = 1, author = 0, 
    tex = FALSE, binary = FALSE, sep = c(", ", " (", ")", "; "))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{name of the file to be created in the working directory.}
  \item{mf}{an object of class 'mefa'.}
  \item{ordering}{type of ordering (sectioning), by \code{"species"} (default) or by \code{"samples"}.}
  \item{biotic.data}{list of columns in the sample attribute table of the 'mefa' object,
    data will be sorted according to subsequent columns.}
  \item{species.name}{a column identifier in the species attribute table of the 'mefa' object
    containing species names.}
  \item{species.order}{a column identifier in the species attribute table of the 'mefa' object,
    by which the ordering of species names can be made (eg. according to taxonomy).}
  \item{author}{a column identifier in the species attribute table of the 'mefa' object,
    containing author names for species. By default it is \code{0}, that is no author names are listed.}
  \item{tex}{logical operator, if \code{TRUE} species names will be italicized and sectioning will
    be used according to \code{order} argument. Default is \code{FALSE}.}
  \item{binary}{logical operator, if \code{TRUE} count data will not be written into the output file.
    Default is \code{FALSE}, when count data are presented.}
  \item{sep}{a character list with four elements, containing separator strings. The first is placed between \code{biotic.data} values, second is placed before, third is after the count value,
    fourth is the separator between full data entries. After the last entry,
    a dot is placed instead of the fourth separator.}
}
\details{
When \code{tex = TRUE}, the result can be directly used in a LaTeX document by copy-pasting the output or by the include option.
}
\value{
A text file named \code{filename} is written in the working directory.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{mefa}}
}
\examples{
\dontrun{
data(landsnail, vhabitat, vsample, vtable)

vt <- as.xcount(vtable, FALSE)
spec <- xorder(vt, which="species", landsnail, 2)
sampl <- xorder(vt, which="samples", vsample, 1)
vmf <- mefa(vt, sampl, spec)
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
\keyword{methods}
