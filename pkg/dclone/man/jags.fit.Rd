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
\examples{
\dontrun{
## simple regression example from the JAGS manual
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x)
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    sigma <- 1.0/sqrt(tau)
    tau ~ dgamma(1.0E-3, 1.0E-3)
}
## data generation
set.seed(1234)
N <- 100
alpha <- 1
beta <- -1
sigma <- 0.5
x <- runif(N)
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## write model onto hard drive
jmodnam <- write.jags.model(jfun)
## fit the model
regmod <- jags.fit(jdata, jpara, jmodnam, n.chains = 3)
## cleanup
clean.jags.model(jmodnam)
## model summary
summary(regmod)
}
}
\keyword{ models }

