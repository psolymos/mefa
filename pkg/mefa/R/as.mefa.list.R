`as.mefa.list` <-
function(x, nested = FALSE, ...)
{
    n <- length(x)
    if (n == 1 && is.matrix(x[[1]]))
        return(as.mefa.default(x[[1]]))
    tmp <- x
    for (i in 2:n) {
        tmp[[i]] <- tmp[[(i-1)]] + x[[i]]
    }
    m <- mefa(tmp[[n]])
    if (nested) {
        m$segm <- x
        attr(m, "nested") <- TRUE
    } else {
        m$segm <- tmp
    }
    m
}

