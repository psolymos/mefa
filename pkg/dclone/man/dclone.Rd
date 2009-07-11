\encoding{UTF-8}
\name{dclone}
\Rdversion{1.1}
\alias{dclone}
\alias{dclone.default}
\alias{print.dclone}
\title{
Clone R Objects
}
\description{
It makes clones of R objects, that is values in the object are repeated \eqn{n} times, 
leaving the original structure of the object intact.
}
\usage{
dclone(x, ...)
\method{dclone}{default}(x, n.clones = 1, ...)
\method{print}{dclone}(x, ...)
}
\arguments{
  \item{x}{
An R object to be cloned, or a cloned object to print.
}
  \item{n.clones}{
Number of clones.
}
  \item{\dots}{
Other arguments passed to function.
}
}
\value{
An object with class attributes \code{"dclone"} plus the original one(s).
Dimensions of the original object might change according to \code{n.clones}.
The function tries to take care of names, sometimes replacing those with the
combination of the original names and an integer for number of clones.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
## scalar
dclone(4, 2)
## vector
(x <- 1:6)
dclone(x, 2)
## matrix
(m <- matrix(x, 2, 3))
dclone(m, 2)
## data frame
(dfr <- as.data.frame(t(m)))
dclone(dfr, 2)
## list
(l <- as.list(dfr))
dclone(l, 2)
}
\keyword{ manip }
