\encoding{UTF-8}
\name{ithcase}
\alias{ithcase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ithcase(x, y, i = 1, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{i}{ ~~Describe \code{i} here~~ }
  \item{na.rm}{ ~~Describe \code{na.rm} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
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
