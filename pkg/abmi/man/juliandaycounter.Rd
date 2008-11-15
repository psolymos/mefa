\encoding{UTF-8}
\name{juliandaycounter}
\alias{juliandaycounter}
\alias{hourcounter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
juliandaycounter(x, format = "%d-%b-%y", yr.start = 8, yr.end = 9, na.rm = FALSE)
hourcounter(x, with.min = TRUE, with.sec = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{format}{ ~~Describe \code{format} here~~ }
  \item{yr.start}{ ~~Describe \code{yr.start} here~~ }
  \item{yr.end}{ ~~Describe \code{yr.end} here~~ }
  \item{with.min}{ ~~Describe \code{with.min} here~~ }
  \item{with.sec}{ ~~Describe \code{with.sec} here~~ }
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
function(x, format="\%d-\%b-\%y", yr.start=8, yr.end=9, na.rm=FALSE)
{
    if (NCOL(x) != 1)
        stop("'x' must be vector")
    x <- as.character(x)
    naid <- is.na.abmi(x)
    fulldate <- x[!naid]
    if (any(nchar(fulldate) != 9))
        stop("not adequate date format")
    year <- paste("20", substr(fulldate, yr.start, yr.end), sep="")
    jan1 <- as.Date(unlist(lapply(year, function(x) paste(x,"-01-01", sep=""))))
    fulldate <- as.Date(fulldate, format)
    out <- as.numeric(fulldate) - as.numeric(jan1)
    if (na.rm) {
        return(out)
        } else {
            y <- numeric(length(x))
            y[!naid] <- out
            y[naid] <- NA
            return(y)
        }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
