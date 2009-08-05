\name{jags.dclone}
\Rdversion{1.1}
\alias{jags.dclone}
\alias{dctable}
\alias{dctable.default}
\title{
Iterative Algorithm for Testing Data Cloning Convergence
}
\description{
\code{\link{jags.fit}} is iteratively used to fit a model with
increasing the number of clones used until data cloning convergence
is reached.
}
\usage{
jags.dclone(data, params, model, n.clones, 
multiply = NULL, unchanged = NULL,
quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), trace = 1, ...)
dctable(x, ...)
\method{dctable}{default}(x, ...)
}
\arguments{
  \item{data}{
A list containing the data.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use itaratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} argument
to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} argument
to be left unchanged.
}
  \item{quantiles}{
  Quantiles of the posterior distribution to save for each monitored parameters
  to be calculate in each step.
}
  \item{trace}{
  If positive, information is printed during the running of the iterations.
}
  \item{x}{
  An MCMC object.
}
  \item{\dots}{
  Other values supplied to \code{\link{jags.fit}}, e.g.
  initial values, \code{n.chains}, \code{n.adapt}, 
    \code{n.update}, \code{thin}, \code{n.iter}.
}
}
\details{
The function fits a JAGS model with increasing numbers of clones, as supplied by
the argument \code{k}. Data cloning is done by the function \code{dclone} using
the arguments \code{multiply} and \code{unchanged}.

After each model fit, data cloning convergence is tested based on the maximal
eigenvalue of the posterior unscaled variance-covariance matrix
(\code{\link{lambdamax.diag}}) and the p-value of the Shapiro-Wilk multivariate 
normality test (\code{\link{shapiro.diag}}).

Critical values for these two diagnostics are arbitrarily set by 0.05
(\code{getOption("dclone.crit")}). Values can be changed to e.g. to
as \code{options("dclone.crit" = c(0.01,0.1))} for stricter criteria.

\code{dctable} returns the \code{"dctable"} attribute of an MCMC object.
This attribute is created during the iterative data cloning process,
to store convergence diagnostics and statistics for the sampled parameters,
because only the last MCMC object is kept. This way, there is a track of
changes in the estimates.
}
\value{
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
Khurram Nadeem, \email{knadeem@math.ualberta.ca}

\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\note{
}
\seealso{
Data cloning: \code{\link{dclone}}

Model fitting: \code{\link{jags.fit}}

Diagnostics: \code{\link{lambdamax.diag}}, \code{\link{shapiro.diag}}, 
for critical values see \code{getOption("dclone.crit")}.
}
\examples{
\dontrun{
## simulation
## Poisson GLMM
set.seed(1234)
n <- 100
beta <- c(1, -1)
sigma <- 0.1
alpha <- rnorm(n, 0, sigma)
x <- runif(n)
X <- model.matrix(~x)
linpred <- X %*% beta + alpha
Y <- rpois(n, exp(linpred))
## JAGS model as a function
jfun <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], betas[i,])
        for (j in 1:p) {
            betas[i,j] <- beta[j]
        }
        alpha[i] ~ dnorm(0, tau)
    }
    for (j in 1:p) {
        beta[j] ~ dnorm(0, 0.001)
    }
    tau ~ dgamma(1.0E-3, 1.0E-3)
    sigma <- 1.0/sqrt(tau)
}
## data
jdata <- list(n = n, Y = Y, X = X, p = NCOL(X))
## iteartive fitting
jmod <- jags.dclone(jdata, c("beta", "sigma"), jfun, 
    n.clones = 2:4, multiply = "n", unchanged = "p")
## summary with DC SE and R hat
## it is also noted if convergence is reached
summary(jmod)
## coralheads example
data(coralheads)
chx <- model.matrix(~log(A), coralheads)
chd <- list(Y = coralheads$S, X = chx, n = nrow(coralheads), p = 2)
chm <- jags.dclone(chd, c("beta", "sigma"), jfun, 
    n.clones = 2:8, multiply = "n", unchanged = "p")
dctable(chm)
}
}
\keyword{ models }
\keyword{ htest }
