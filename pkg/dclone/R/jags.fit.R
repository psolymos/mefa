jags.fit <-
function(data, params, model, n.chains=3, n.adapt=1000, n.update=0, thin=1, n.iter=5000, progress.bar="text", ...)
{
    n.clones <- nclones.list(data)
    if (is.function(model)) {
        model <- write.jags.model(jfun)
        on.exit(clean.jags.model(model))
    }
    m <- jags.model(model, data, n.chain=n.chains, n.adapt=n.adapt, ...)
    if (n.update > 0) {
        update(m, n.update, progress.bar=progress.bar)
    }
    res <- coda.samples(m, params, n.iter=n.iter, thin=thin, progress.bar=progress.bar)
    attr(res, "n.clones") <- n.clones
    res
}
