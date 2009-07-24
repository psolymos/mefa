`fill.na` <-
function (x)
{
    x <- as.matrix(x)
    if (any(is.na(x[1,])))
        stop("cannot replace 'NA's in first place")
    out <- x
    for (cols in 1:ncol(x)) {
        for (rows in 1:nrow(x)) {
            if (is.na(x[rows, cols])) {
                rows2 <- rows - 1
                out[rows, cols] <- out[rows2, cols]
            }
        }
    }
    return(out)
}

