\encoding{UTF-8}
\name{ithcase}
\alias{ithcase}
\title{Substract 'i'th Cases of the Data}
\description{
Substracts the $i$th cases of \code{x} along each level of factor \code{y}. The function is useful for vectorized data, vhen items are repeated in known sequences.
}
\usage{
ithcase(x, y, i = 1, na.rm = FALSE)
}
\arguments{
  \item{x}{a vector to be substracted.}
  \item{y}{a vector which is used to determine repeating sequences of the valoes of \code{x}. Unique values or factor levels are used for this.}
  \item{i}{numeric, determines position of values taken from \code{x} within levels of \code{y}. Can be a single value or a vector. If it is vector, its length must be same as unique values in \code{y}.}
  \item{na.rm}{logical, shoulr NAs removed from the result? The removel of NAs takes place befor subtsracting.}
}
\details{
\code{x} and \code{y} should be of the same lengths.
}
\value{
A vector with length same as number of levels, or unique values in \code{y}. If \code{na.rm = TRUE}, it can be shorter, because the removal of NA values takes place prior to substracting. Names of the result is same as levels of \code{y}.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\seealso{\code{\link{abmi.is.na}}}
\examples{
data(abmi.data)
ithcase(abmi.data$sitecap[,9], abmi.data$sitecap$ABMI.Site)
}
\keyword{ manip }
