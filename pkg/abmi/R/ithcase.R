## substracts just the first cases of x along each level of factor y
## i is the index for ith element to choose, can be a vector
ithcase <-
function(x, y, i=1, na.rm=FALSE)
{
    if (length(x) != length(y))
        stop("dimensions do not match")
    y <- factor(as.character(y))
    if (na.rm) {
        y <- y[!abmi.is.na(x)]
        x <- x[!abmi.is.na(x)]
    }
    if (length(i == 1)) {
        i <- rep(i, nlevels(y))
        names(i) <- levels(y)
    }
    out <- unlist(lapply(levels(y), function(z) x[y==z][i[z]]))
    names(out) <- levels(y)
    return(out)
}
