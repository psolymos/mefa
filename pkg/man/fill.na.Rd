\encoding{UTF-8}
\name{fill.na}
\alias{fill.na}
\title{Replace <NA> values with nearest above values in a matrix}
\description{
The function replaces \code{<NA>} values with the nearest values above them in the same column.
}
\usage{
fill.na(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix or data frame.}
}
\details{
This function is useful when converting a 'notebook style' data into an 'stsc' object. The 'notebook style' data means that observations are registeres in a non-redundant way, e.g. when typing in a spreadsheet.
}
\value{
A matrix or data frame.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
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
}
\keyword{ manip }
