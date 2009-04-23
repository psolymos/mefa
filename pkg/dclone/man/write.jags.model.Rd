\name{clean.jags.model}
\Rdversion{1.1}
\alias{write.jags.model}
\alias{clean.jags.model}
\title{
Removes a (JAGS model) file from the hard drive.
}
\usage{
write.jags.model(model, filename = "model.bug", dir = getwd(), n = 5)
clean.jags.model(filename = "model.bug", dir = getwd())
}
\arguments{
  \item{model}{
JAGS model to write onto the hard drive (see Example).
}
  \item{filename}{
Character, the name of the file to write/remove.
}
  \item{dir}{
Optional argument for directory where to write or 
look for the file to remove.
}
  \item{n}{
Numeric, length of a random string to be attached to \code{filename} 
if \code{filename} already exists.
}
}
\details{
\code {write.jags.model} is built upon the function 
\code{\link{write.model}} of the \pkg{R2WinBUGS} package.

\code {clean.jags.model} is built upon the function 
\code{\link{file.remove}}, and 
intended to be used internally to clean the JAGS 
model file up after estimating sessions, 
ideally via the \code{\link{on.exit}} function.
}
\value{
\code {write.jags.model} invisibly returns the name of the file
that was written eventually (possibly including random string).

\code {clean.jags.model} invisibly returns the result of 
\code{\link{file.remove}} (logical).
Original working directory is then restored.
}
\author{
\author{\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}}
}
\seealso{
\code{\link[R2WinBUGS]{write.model}}, \code{\link{file.remove}}
}
\examples{
## example missing
}
\keyword{ IO }
