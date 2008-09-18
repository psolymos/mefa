`inflate` <-
function (x, y)
{
    x <- data.frame(x)
    if (nrow(x) != length(y))
        stop("dimensions do not match")
    if (!identical(all.equal(y, round(y)), TRUE))
        stop("y must be integer")
    if (any(y <= 0))
        stop("y must be positive")
    out <- apply(x, 2, function(x) {
        z <- mapply(rep, x, y)
        names(z) <- NULL # this is needed to overcome dupl. row names
        unlist(z)
        })
    return(data.frame(out))
}

