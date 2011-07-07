## utility functions

nonDuplicated <- function(x, y, change.rownames=FALSE) {
    if (length(dim(x)) != 2)
        stop("'x' must have 2 dimensions")
    z <- deparse(substitute(y))
    if (z %in% colnames(x))
        y <- x[,z]
    if (nrow(x) != length(y))
        stop("non matching arguments 'x' and 'y'")
    keep <- !duplicated(y)
    out <- x[keep,]
    if (change.rownames)
        rownames(out) <- y[keep]
    out
}

Melt <-
function(x)
{
    if (inherits(x, "Mefa"))
        x <- x@xtab
    if (inherits(x, "mefa"))
        x <- as(x$xtab, "dgCMatrix")
    if (inherits(x, "matrix"))
        x <- as(x, "dgCMatrix")
    if (inherits(x, "sparseMatrix")) {
        x <- as(x, "dgTMatrix")
        rows <- x@i + 1L
        cols <- x@j + 1L
        y <- x@x
        out <- data.frame(rows = factor(x@Dimnames[[1]][rows], 
            levels=x@Dimnames[[1]]), 
            cols = factor(x@Dimnames[[2]][cols], 
            levels=x@Dimnames[[2]]), 
            value = y)
    } else if (is.list(x) && all(sapply(x, function(z) 
        inherits(z, "sparseMatrix")))) {
        if (!all(sapply(x[-1], function(z) 
            identical(z@Dimnames, x[[1]]@Dimnames))))
            stop("dimnames of list elements must be identical")
        n <- length(x)
        X <- rows <- cols <- y <- vector("list", n)
        for (k in 1:n) {
            X[[k]] <- as(x[[k]], "dgTMatrix")
            rows[[k]] <- X[[k]]@i + 1L
            cols[[k]] <- X[[k]]@j + 1L
            y[[k]] <- X[[k]]@x
        }
        out <- data.frame(rows = factor(x[[1]]@Dimnames[[1]][unlist(rows)], 
            levels=x[[1]]@Dimnames[[1]]), 
            cols = factor(x[[1]]@Dimnames[[2]][unlist(cols)], 
            levels=x[[1]]@Dimnames[[2]]), 
            segm = as.factor(rep(names(x), sapply(y, length))),
            value = unlist(y))
    } else stop("object class not appropriate")
    out
}
