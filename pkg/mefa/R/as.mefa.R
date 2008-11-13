`as.mefa` <-
function(x, ...)
{
    if (inherits(x, "mefa")) {
## unnested !!!
        ss <- if (dim(x)[3] > 1)
            melt.mefa(x) else x$xtab
        y <- mefa(ss, x$samp, x$taxa, ...)
        y$call <- match.call()
        return(y) 
        } else return(mefa(x, ...))
}

