combine.mcmc.list <-
function(x, y, FUN, var.names=varnames(x), ...)
{
    ax1 <- attributes(x)
    ay1 <- attributes(y)
    ax2 <- attributes(x[[1]])
    ay2 <- attributes(y[[1]])
    if (!identical(ax1, ay1) || !identical(ax1, ay1))
        warning("mcmc.list settings are not identical")
    nc <- length(x)
    n <- prod(dim(x[[1]]))
    FUN <- match.fun(FUN)
    rval <- x
    for (i in 1:nc) {
        rval[[i]] <- sapply(1:n, function(z)
            FUN(array(x[[i]])[z], array(y[[i]])[z], ...))
        attributes(rval[[i]]) <- ax2
    }
    attributes(rval) <- ax1
    varnames(rval) <- var.names
    rval
}
