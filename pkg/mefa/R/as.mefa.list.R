`as.mefa.list` <-
function(x, ...)
{
    n <- length(x)
    if (n == 1 && is.matrix(x[[1]]))
        return(as.mefa.default(x[[1]]))
    tmp <- x
    for (i in 2:n) {
        tmp[[i]] <- tmp[[(i-1)]] + x[[i]]
    }
    m <- mefa(tmp[[n]])
    m$segm <- tmp
    m <- as.mefa.default(m, ...)
    m$call <- match.call()
    m
}
