`fill.na` <-
function (x)
{
# not vectorized
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

