\encoding{UTF-8}
\name{read.abmi}
\alias{read.abmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.abmi(filename, dir = getwd(), remove.empty = TRUE, header = TRUE, sep = ",", quote = "\"", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{dir}{ ~~Describe \code{dir} here~~ }
  \item{remove.empty}{ ~~Describe \code{remove.empty} here~~ }
  \item{header}{ ~~Describe \code{header} here~~ }
  \item{sep}{ ~~Describe \code{sep} here~~ }
  \item{quote}{ ~~Describe \code{quote} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function(filename, dir=getwd(), remove.empty=TRUE, header=TRUE, sep=",", quote="\"", ...)
{
    oldir <- getwd()
    setwd(dir)
    x <- read.table(filename, header=header, sep=sep, quote=quote, ...)
    if (remove.empty) x <- x[ , -ncol(x)]
    setwd(oldir)
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
