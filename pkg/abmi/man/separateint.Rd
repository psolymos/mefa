\encoding{UTF-8}
\name{separateint}
\alias{separateint}
\title{Seperates A Character Vector into Two Parts}
\description{
This function separates two parts of a character vector. It intend to be 'reverse' of the function \code{\link{interaction}}.
}
\usage{
separateint(x, step = 1, spacer = 1)
}
\arguments{
  \item{x}{a character vector or factor.}
  \item{step}{number of character to separate (counted backwards).}
  \item{spacer}{number of characters to be excluded, just before the second part determined by \code{step}.}
}
\details{
It operates backwards, so starts at the end of the strings.
}
\value{
A two-column matrix with characters. Column names refer to parts as \code{first} and \code{second}. Rownames are the same as the names for the original vector.
}
\references{\url{http://www.abmi.ca/}}
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
\note{
This is not a general purpose function, it was created to do the same trick as in the example.}
\seealso{\code{\link{interaction}}}
\examples{
data(abmi.data)
int <- interaction(abmi.data$sitecap$ABMI.Site, abmi.data$sitecap$Point.Count.Station)
## Random sample from the interaction vector
set.seed(123)
int <- int[sample(1:length(int), 20)]
int
## Strings are of different lengths
nchar(as.character(int))
separateint(int)
}
\keyword{ manip }
