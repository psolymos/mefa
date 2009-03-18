library(mefa)
data(dol.count)
ss <- stcs(dol.count)
x <- mefa(ss)

## as.* methods

`as.matrix.mefa` <-
function(x, ...)
{
    x$xtab
}

`as.list.mefa` <-
function(x, ...)
{
    if (is.null(x$segm) || length(x$segm) == 1) {
        return(list(x$xtab))
    } else {
        return(x$segm)
    }
}

`as.array.mefa` <-
function(x, ...)
{
    DIM <- dim(x)
    DIMNAMES <- dimnames(x)
    DATA <- unlist(as.list.mefa(x))
    array(DATA, DIM, DIMNAMES)
}

`as.data.frame.stcs` <-
function(x, ...)
{
    attr(x, "call") <- NULL
    attr(x, "expand") <- NULL
    attr(x, "zero.count") <- NULL
    attr(x, "zero.pseudo") <- NULL
    class(x) <- "data.frame"
    x
}

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




