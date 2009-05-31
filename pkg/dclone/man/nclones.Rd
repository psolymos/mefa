\encoding{UTF-8}
\name{BIC}
\Rdversion{1.1}
\alias{nclones}
\alias{nclones.list}
\alias{nclones.default}
\title{
Number of Clones
}
\description{
Retrieves the number of clones from an object.
}
\usage{
nclones(x, ...)
\method{nclones}{default}(x, ...)
\method{nclones}{list}(x, ...)
}
\arguments{
  \item{x}{
An object of class 'dclone' (or an object having an \code{n.clones} attribute).
}
  \item{\dots}{
Fitted model objects.
}
}
\value{
Returns the number of observations, or the number of clones.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{AIC}}
}
\examples{
## example(glm)
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
nobservations(glm.D93)
## dclone and nclones
x <- dclone(1:10, 10)
nclones(x)
}
\keyword{ manip }
