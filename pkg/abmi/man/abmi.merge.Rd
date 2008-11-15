\encoding{UTF-8}
\name{merge.abmi}
\alias{merge.abmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
merge.abmi(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
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
function(x, na.rm = FALSE)
{
    inherits(x, "list") || stop("'x' must be a list")
    n <- length(x)
    if (n < 2)
        stop("length of 'x' should be at least 2")
    nam <- names(x)
    names(x) <- NULL
    x <- lapply(x, as.data.frame)
    f <- unlist(lapply(x, function(z) is.factor(z[,1])))
    rnam <- rownames(x[[1]])
    for (i in 1:n) {
        rnam <- union(rnam, rownames(x[[i]]))
        if (isTRUE(f[i]))
            x[[i]][,1] <- as.character(x[[i]][,1])
    }
    out <- list()
    for (i in 1:n) {
        out[[i]] <- vector(mode = mode(x[[i]][,1]), length = length(rnam))
        out[[i]][match(rownames(x[[i]]), rnam)] <- x[[i]][,1]
        out[[i]][!(rnam \%in\% rownames(x[[i]]))] <- NA
        names(out)[i] <- nam[i]
    }
    out <- as.data.frame(out)
    rownames(out) <- rnam
    if (na.rm) 
        out <- out[apply(is.na(out), 1, sum) == 0,]
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
