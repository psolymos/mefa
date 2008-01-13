\name{as.xcount}
\alias{as.xcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes an Object of Class 'xcount' from a Table}
\description{
The function converts a sample/species crosstable into an 'xcount' object. Count and measurement type data are allowed.
}
\usage{
as.xcount(table, species.columns = TRUE, segment = "unspecified", digits=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{a data frame or matrix with sample/species count data. It can either be an object of class 'mefa',
    in this case, only the \code{table$xcount} part will be used, (\code{table$segment} is inherited. 
    \code{NA} values are not allowed.}
  \item{species.columns}{if \code{FALSE}, species are rows and samples are columns. By default it is \code{TRUE}.}
  \item{segment}{arbitrary name of the segment (eg. adult, juvenile, male, female) whose data are
    included in the table, by default it is \code{"unspecified"}. In case of 'mefa' objects, segment name 
    (default or explicitlydefined) will be overwritten by the inherited \code{table$segment} value.}
  \item{digits}{\code{NULL} (default) is used when results are count data (eg. number of individuals). If result is 
    not integer, but \code{digits=NULL}, error message is given. If results are not of count type but some kind of measurements 
    (eg. biomass, containing decimals) \code{digits} can be used to set precision of the data representation, 
    in this case use integers (\code{digits=0} results integers, \code{digits=1} means 1 decimal, etc.).}
}
\value{
A result is an 'xcount' object.
}

\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}, \code{\link{xcount}}, \code{\link{mefa}}, \code{\link{xorder}}, \code{\link{check.attrib}}
}
\examples{
### Example 1: simple atrificial data

xc <- as.data.frame(rbind(
c(4, 0, 0),
c(2, 4, 0),
c(0, 0, 5),
c(0, 0, 0)
))
rownames(xc) <- c("sample1", "sample2", "sample3", "sample4")
colnames(xc) <- c("species1", "species2", "species3")

(xc1 <- as.xcount(xc))

(xc2 <- as.xcount(xc, segment = "female"))

xc <- xc + 0.2
(xc3 <- as.xcount(xc, digits=0))

(xc4 <- as.xcount(xc, digits=1))

### Example 2: field data of Villany Hills

\dontrun{
data(vtable)

v1 <- as.xcount(vtable, species.columns = FALSE)
v1

v2 <- as.xcount(vtable, species.columns = FALSE, segment = "adult")
v2
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{classes}

