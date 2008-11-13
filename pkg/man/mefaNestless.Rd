\encoding{UTF-8}
\name{mefaNestless}
\alias{mefaNestless}
\title{Make Non-Nested 'mefa' Object}
\description{
The functions evaluates the internal structute of a 'mefa' object.
}
\usage{
mefaNestless(x)
}
\arguments{
  \item{x}{an objects of class 'mefa' with nested segments.}
}
\value{
Returns an object of class 'mefa', with non-nested segments.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}, \code{\link{is.mefa}}}
\examples{
data(dol.count)
x <- mefa(stcs(dol.count), nested = TRUE)
x
y <- mefaNestless(x)
y
}
\keyword{ manip }
