\name{mefa-package}
\alias{mefa-package}
%\alias{mefa}
\docType{package}
\title{
Multivariate Count Data Handling in Ecology and Biogeography
}
\description{
R package for multivariate count data handling in ecology and biogeography. The name 'mefa' stands for the term 'metafaunistics' indicating that data handling is the most critical and sometimes most time consuming part of data analysis. The package contains object classes and functions to manipulate basic biotic (sample/species/count or crosstabulated) count data and relational attribute tables for samples and species. Functions can handle segments within the count data and samples with zero count. Reports are generated in plain text or LaTeX format.

S3 object classes provided by this package are:

\code{\link{sscount}} for catch results of a field experiment referenced according to samples and species, samples with zero total count can be indicated, rows can be assigned to segments.

\code{\link{xcount}} for sample/species crosstabulation of an object of class \code{\link{sscount}}, samples with zero total count are retained, one \code{\link{xcount}} object refers to one segment only.

\code{\link{xorder}} contains sample or species specific attributes, subsetted and ordered according to rows/columns of an object of class \code{\link{xcount}}.

\code{\link{mefa}} contains data of an \code{\link{xcount}} and two \code{\link{xorder}} objects (for sample and species attributes).

Utility functions are provided to check, manipulate and plot objects.
}
\details{
\tabular{ll}{
Package: \tab mefa\cr
Type: \tab Package\cr
Depends: \tab R (>= 2.6.0), vegan\cr
Version: \tab 1.1-0\cr
Date: \tab 2008-03-14\cr
License: \tab GPL-2\cr
}
}
\author{
Peter Solymos <Solymos.Peter@aotk.szie.hu>
}
\references{
\url{http://mefa.r-forge.r-project.org/}
}
\keyword{ package }
\examples{
\dontrun{
library(mefa)
demo(dolina)               ## Demo showning possible usage of 'mefa' objects
mefadocs("ChangeLog")      ## Browse version history
mefaweb()                  ## Check out website for resources
vignette("mefa-vignette")  ## Open vignette
}
}
