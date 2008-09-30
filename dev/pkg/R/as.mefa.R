`as.mefa` <-
function(x, ...)
{
    if (inherits(x, "mefa"))
        return(x) else return(mefa(x, ...))
}

