## class definitions

nonDuplicated <- function(x, y, change.rownames=FALSE) {
    if (length(dim(x)) != 2)
        stop("'x' must have 2 dimensions")
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
    if (inherits(x, "matrix"))
        x <- as(x, "dgCMatrix")
    if (inherits(x, "sparseMatrix")) {
        x <- as(x, "dgTMatrix")
        rows <- x@i + 1L
        cols <- x@j + 1L
        y <- x@x
        out <- data.frame(rows = as.factor(rows), 
            cols = as.factor(cols), value = y)
        levels(out$rows) <- x@Dimnames[[1]]
        levels(out$cols) <- x@Dimnames[[2]]
    } else if (is.list(x) && all(sapply(x, function(z) 
        inherits(z, "sparseMatrix")))) {
        n <- length(x)
        X <- rows <- cols <- y <- vector("list", n)
        for (k in 1:n) {
            X[[k]] <- as(x[[k]], "dgTMatrix")
            rows[[k]] <- X[[k]]@i + 1L
            cols[[k]] <- X[[k]]@j + 1L
            y[[k]] <- X[[k]]@x
        }
        out <- data.frame(rows = as.factor(unlist(rows)), 
            cols = as.factor(unlist(cols)), 
            segm = as.factor(rep(names(x), sapply(y, length))),
            value = unlist(y))
        levels(out$rows) <- x[[1]]@Dimnames[[1]]
        levels(out$cols) <- x[[1]]@Dimnames[[2]]
    } else stop("object class not appropriate")
    out
}
