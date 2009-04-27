\name{dclone}
\Rdversion{1.1}
\alias{dclone}
\title{
Clone R Objects
}
\description{
It makes clones of R objects, that is values in the object are repeated \eqn{n} times, 
leaving the original structure of the object intact.
}
\usage{
dclone(x, n.clones = 1)
}
\arguments{
  \item{x}{
An R object to be cloned.
}
  \item{n.clones}{
Number of clones.
}
}
\value{
An object with class attributes \code{"dclone"} plus the original one(s).
Dimensions of the original object might change according to \code{n.clones}.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
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
