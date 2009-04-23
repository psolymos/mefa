jags.fit <-
function(data, params, model, n.chains=3, n.update=0, n.iter=5000, thin=1, n.adapt=1000, ...)
{
    m <- jags.model(model, data, n.chain=n.chains, n.adapt=n.adapt, ...)
    if (n.update > 0)
        update(m, n.update)
    res <- coda.samples(m, params, n.iter=n.iter, thin=thin)
    res
}

