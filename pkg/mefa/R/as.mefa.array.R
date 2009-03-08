`as.mefa.array` <-
function(x, nested = FALSE, ...)
{
    if (length(dim(x)) == 2)
        return(as.mefa.default(x[[1]]))
    n <- dim(x)[3]
    tmp <- list()
    tmp[[1]] <- x[,,1]
    for (i in 2:n) {
        tmp[[i]] <- tmp[[(i-1)]] + x[,,i]
    }
    m <- mefa(tmp[[n]])
    if (nested) {
        m$segm <- x
        attr(m, "nested") <- TRUE
    } else {
        names(tmp) <- dimnames(x)[[3]]
        m$segm <- tmp
    }
    m
}

