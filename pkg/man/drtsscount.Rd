\name{drtsscount}
\alias{drtsscount}
\title{Make Object of Class 'sscount' from Table with Duplicate Rows}
\description{
The function makes an object of class 'sscount' from a table (data frame), when some samples are presented in more than one rows (identifiers are not unique). Count and measurement type data are allowed.
}
\usage{
drtsscount(table, sample, segment = "unspecified", digits = NULL)
}
\arguments{
  \item{table}{a data frame with count data. \code{NA} values are not allowed.}
  \item{sample}{a vector with same length as number of rows in \code{table}. Used as factor for coding samples.
    \code{NA} values are not allowed.}
  \item{segment}{arbitrary name of the segment (eg. adult, juvenile, male, female), included in the table, 
    by default it is \code{"unspecified"}.}
  \item{digits}{\code{NULL} (default) is used when results are count data (eg. number of individuals). If result is 
    not integer, but \code{digits=NULL},  error message is given. If results are not of count type but some kind of measurements 
    (eg. biomass, containing decimals) \code{digits} can be used to set precision of the data representation, 
    in this case use integers (\code{digits=0} results integers, \code{digits=1} means 1 decimal, etc.).}

}
\value{
A result is an object of class 'sscount'.
}
\note{
In \code{table}, columns must refer to species, unlike in \code{\link{ttsscount}}, where species can be rows either by using the \code{species.columns = FALSE} argument. If this is the case, transpose the data frame as in the example below. Samples (rows) with 0 total count are flagged as 'zero.count' in the resulting 'sscount' object.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}, \code{\link{ttsscount}}
}
\examples{
\dontrun{
data(vsample, vtable)

v1 <- drtsscount(t(vtable), vsample$site.descr)
v1

v2 <- drtsscount(t(vtable), vsample$site.descr, segment = "adult")
v2
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
