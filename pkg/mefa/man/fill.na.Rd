\encoding{UTF-8}
\name{fill.na}
\alias{fill.na}
\title{Replace <NA> Values with Nearest Above Values}
\description{
The function replaces \code{<NA>} values with the nearest values above them in the same column.
}
\usage{
fill.na(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector, a matrix or a data frame.}
}
\details{
This function is useful when converting a 'notebook style' data into an 'stcs' object. The 'notebook style' data means that observations are registered in a non-redundant way, e.g. when typing in a spreadsheet.
}
\value{
A matrix or a data frame.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \url{http://www.jstatsoft.org/v29/i08/}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{This function is not vectorized, so can be a bit slow for large data sets.}
\seealso{\code{\link{stcs}}}
\examples{
x <- data.frame(
    samp = c("s1",NA,"s2",NA,"s3","s4"),
    taxa = c("t1",NA,"s1","s2","s3",NA),
    count = c(1, 2, 10, 3, 4, 1),
    segm = c("male","female","male","female","male","male"))
x
fill.na(x)
fill.na(c(1:4, NA))
}
\keyword{ manip }
