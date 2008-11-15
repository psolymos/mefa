\encoding{UTF-8}
\name{is.na.abmi}
\alias{is.na.abmi}
\alias{convert.na.abmi}
\alias{na.omit.abmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
is.na.abmi(x, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
convert.na.abmi(x, value = NA, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"))
na.omit.abmi(x, value = NA, na.char = c("NA", "VNA", "DNC", "PNA", "SNI"), drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{na.char}{ ~~Describe \code{na.char} here~~ }
  \item{value}{ ~~Describe \code{value} here~~ }
  \item{drop}{ ~~Describe \code{drop} here~~ }
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
function(x, na.char=c("NA", "VNA", "DNC", "PNA", "SNI"))
{
    if (NCOL(x) == 1) y <- x \%in\% na.char
    else {
        is.it.mat <- is.matrix(x)
        y <- sapply(as.data.frame(x), function(a) a \%in\% na.char)
        if (is.it.mat) y <- as.matrix(y)
    }
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
