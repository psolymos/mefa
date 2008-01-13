\name{mefa-package}
\alias{mefa-package}
%\alias{mefa}
\docType{package}
\title{
Faunistic Count Data Handling and Reporting
}
\description{
R package for faunistic count data handling and reporting. The name 'mefa' stands for the term 'metafaunistics' indicating that handling of basic data is only the first, but the most critical and sometimes most time consuming part of data analysis. It contains functions to create and manage objects combining basic faunistic (sample/species/count or crosstabulated) count data and sample/species attribute tables. Segments within the count data and samples with zero count can be indicated and used in subsequent operations. Reports can be generated in plain text or LaTeX format.

Objects provided by this package are:

\code{\link{sscount}} for catch results of a field experiment referencedaccording to samples and species, samples with zero total count can be indicated, rows can be assignet to segments.

\code{\link{xcount}} for sample/species crosstabulation of an object of class \code{\link{sscount}}, samples with zero total count are retained, one \code{\link{xcount}} object refers to one segment only.

\code{\link{xorder}} contains sample or species specific attributes, subsetted and ordered according to rows/columns of an object of class \code{\link{xcount}}.

\code{\link{mefa}} contains data of an \code{\link{xcount}} and two \code{\link{xorder}} objects (for sample and species attributes).

Utility functions are provided to check and manipulate objects.
}
\details{
\tabular{ll}{
Package: \tab mefa\cr
Type: \tab Package\cr
Version: \tab 1.0-4\cr
Date: \tab 2008-01-06\cr
License: \tab GPL-2\cr
}
}
\author{
Peter Solymos <Solymos.Peter@aotk.szie.hu>
}
\references{
\url{http://www.univet.hu/users/psolymos/personal/}
}
\keyword{ package }
\examples{
\dontrun{
library(mefa)
demo(dolina)               ## Demo showning possible usage of 'mefa' objects
mefadocs("ChangeLog")      ## Browse version history
vignette("mefa-vignette")  ## Open vignette
}
}
