\name{jags.fit}
\Rdversion{1.1}
\alias{jags.fit}
\title{
Convenient function to fit JAGS models.
}
\usage{
jags.fit(data, params, model, n.chains = 3, n.update = 0, n.iter = 5000, thin = 1, n.adapt = 1000, ...)
}
\arguments{
  \item{data}{
A list containing the data.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string, name of the model file.
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{n.update}{
Number of updates before iterations.
}
  \item{n.iter}{
Number of iterations.
}
  \item{thin}{
Thinning value.
}
  \item{n.adapt}{
Number of steps for adaptation.
}
  \item{\dots}{
Further arguments passed to \code{\link[rjags]{jags.model}}.
}
}
\value{
An \code{mcmc} or \code{mcmc.list} object.
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[rjags]{jags.model}}, \code{\link[coda]{coda.samples}}
}
\keyword{ models }

