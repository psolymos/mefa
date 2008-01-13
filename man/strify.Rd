\name{strify}
\alias{strify}
\title{Stratifies Object of Class 'xcount' or 'mefa'}
\description{
Stratifies an object of class 'xcount' or 'mefa' according to levels of a factor, or sample/species attribute, respectively.
}
\usage{
strify(xc, strata, which = c("samples", "species"))
}
\arguments{
  \item{xc}{an object of class 'xcount' or 'mefa' to be stratified.}
  \item{strata}{a vector with length equal to the number of samples/species (according to \code{which} argument).
    For a 'mefa' object \code{strata} is a column within the samples/species attribute table (according to
    \code{which} argument).}
  \item{which}{switches between rows (\code{"samples"}) and columns (\code{"species"}) to be stratified.}
}
\details{
If the original object is of class 'xcount', the result will be also of 'xcount'. I the original object is of class 'mefa', it must have at least one attribute table, that is not \code{NULL} (and not the same as stated by \code{which}), because the stratified attribute table will be \code{NULL} in the end. If both attribute tables are not \code{NULL}, the result will be of 'mefa', if the other attribute table is \code{NULL}, the result will be of 'xcount'. Sttatifying according to a \code{NULL} attribute table has no sense, and an error message is given. In this case use function \code{\link{as.xcount}}.
}
\value{
A result is an object of class 'xcount' of 'mefa'.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{as.xcount}}, \code{\link{check.attrib}}, \code{\link{mefa}}, \code{\link{sscount}}, 
  \code{\link{xcount}}, \code{\link{xorder}}
}
\examples{
\dontrun{
data(landsnail, vhabitat, vsample, vtable)

v1 <- as.xcount(vtable, FALSE, segment="mixed")
spec <- xorder(v1, which="species", landsnail, 2)
sampl <- xorder(v1, which="samples", vsample, 1)
vmf <- mefa(v1, sampl, spec)

v2 <- strify(vmf, strata = "site.descr", which = "samples")
v2 ## output is 'mefa'

v3 <- strify(v2, strata = "familia", which = "species")
v3 ## output is 'xcount'

check.attrib(as.xcount(v2), which="samples", vhabitat, 2)
habi <- xorder(as.xcount(v2), which="samples", vhabitat, 2)
vmf2 <- mefa(as.xcount(v2), habi, spec)
vmf2 ## 'mefa' again with attributes
}
}
\keyword{manip}
\keyword{utilities}
