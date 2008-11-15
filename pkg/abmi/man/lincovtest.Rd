\encoding{UTF-8}
\name{lincovtest}
\alias{lincovtest}
alias{plot.lincovtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lincovtest(y, x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...)
\method{plot}{lincovtest}(x, type = "b", ylab = "Estimated coefficients", xlab = "Quantiles", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{probs}{ ~~Describe \code{probs} here~~ }
  \item{na.rm}{ ~~Describe \code{na.rm} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
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
function(y, x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...)
{
    require(mefa)
    q <- qvector(x, probs = probs, na.rm = na.rm, ...)
    z <- as.factor(q)
    mm <- glm(y ~ z, family=binomial)
#    m <- c(0, coef(mm)[-1])
    m <- coef(mm)
    names(m) <- levels(z)
    class(m) <- c("lincovtest", class(m))
    return(m)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
