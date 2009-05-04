\encoding{UTF-8}
\name{BIC}
\Rdversion{1.1}
\alias{nobservations}
\title{
Number of Observations
}
\description{
Retrieves the number of observations from a fitted model object.
}
\usage{
nobservations(fit, where=NULL)
}
\arguments{
  \item{fit}{
Fitted model objects.
}
}
\value{
Returns the number of observations.
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
}
\keyword{ manip }
