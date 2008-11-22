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
\value{
A vector with length same as number of levels, or unique values in \code{y}. If \code{na.rm = TRUE}, it can be shorter, because the removal of NA values takes place prior to substracting.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\note{

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, y, i=1, na.rm=FALSE)
{
    if (length(x) != length(y))
        stop("dimensions do not match")
    y <- factor(as.character(y))
    if (na.rm) {
        y <- y[!is.na.abmi(x)]
        x <- x[!is.na.abmi(x)]
    }
    if (length(i == 1)) {
        i <- rep(i, nlevels(y))
        names(i) <- levels(y)
    }
    out <- unlist(lapply(levels(y), function(z) x[y==z][i[z]]))
    names(out) <- levels(y)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
