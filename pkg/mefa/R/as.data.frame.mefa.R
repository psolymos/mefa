as.data.frame.mefa <-
function(x, fun, name, make.unique = FALSE, ...)
{
    fun <- match.fun(fun)
    if (!inherits(x, "mefa"))
        stop("'x' must be of class mefa")
    if (missing(fun))
        stop("'fun' argument is missing")
    if (!is.function(fun))
        stop("'fun' argument must be a function")
    fun(x, name, make.unique, ...)
}
