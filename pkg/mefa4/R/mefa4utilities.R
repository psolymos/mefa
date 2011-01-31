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

