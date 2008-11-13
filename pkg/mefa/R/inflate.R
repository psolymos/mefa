`inflate` <-
function (x, by)
{
    x <- data.frame(x)
    if (nrow(x) != length(by))
        stop("dimensions do not match")
    if (!identical(all.equal(by, round(by)), TRUE))
        stop("'by' must be integer")
    if (any(by <= 0))
        stop("'by' must be positive")
    out <- apply(x, 2, function(x) {
        z <- mapply(rep, x, by)
        names(z) <- NULL
        unlist(z)
        })
    return(data.frame(out))
}

