\name{amf}
\alias{amf}
\title{Extract and Attach Marginal Statistics and Attribute Table From Object of Class 'mefa'}
\description{
This function extracts and optionally attaches data from an object of class 'mefa'.}
\usage{
amf(x, which = "samples", attach = FALSE)
}
\arguments{
  \item{x}{an object of class 'mefa'.}
  \item{which}{switch for attribute table, either \code{"samples"} (default) or \code{"species"}.}
  \item{attach}{logical, see value for details.}
}
\details{
If attribute table is \code{NULL}, an error message is given.
}
\value{
Result is a data frame with respective marginal statistics (see \code{}, in \code{\link{mefa}} documentation) and the attribute table itself. If \code{attach=FALSE} (default) a data frame is created, if \code{attach=TRUE}, the resulting data frame is made and attached.}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{attach}}, \code{\link{mefa}}
}
\examples{
\dontrun{
data(landsnail, vsample, vtable)

(vmf <- mefa(vt <- as.xcount(vtable, FALSE),
xorder(vt, which="samples", vsample, 1),
xorder(vt, which="species", landsnail, 2)))

amf(vmf)
amf(vmf,attach=TRUE)
srichn

amf(vmf, "species")
}
}
\keyword{manip}
